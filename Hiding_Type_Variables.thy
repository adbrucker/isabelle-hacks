(***********************************************************************************
 * Copyright (c) 2018 Achim D. Brucker
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * Repository:   https://git.logicalhacking.com/adbrucker/isabelle-hacks/
 * Dependencies: None (assert.thy is used for testing the theory but it is 
 *                     not required for providing the functionality of this hack)
 ***********************************************************************************)

chapter\<open>Using Print and Parse Translations for Hiding Type Variables\<close>
theory
  "Hiding_Type_Variables"
imports 
  "Assert" (* Can be replaced by Main, after removing all assertions. *)
keywords
    "register_default_tvars"
    "update_default_tvars_mode"::thy_decl
begin
text\<open>
  This theory implements a mechanism for declaring default type variables for 
  data types. This comes handy for complex data types with many type variables. 
  The theory sets up both configurable print and parse translations that allows 
  for replacing @{emph \<open>all\<close>} type variables by @{text  \<open>(_)\<close>}, e.g., a five-ary 
  constructor @{text \<open>('a, 'b, 'c, 'd, 'e) foo\<close>} can be shorted to 
  @{text \<open>(_) foo\<close>}. The use of this shorthand in output (printing) and input 
  (parsing) is, on a per-type basis, user-configurable using the top-level 
  commands @{text \<open>register_default_tvars\<close>} (for registering the names of the 
  default type variables and the print/parse mode) and  
  @{text \<open>update_default_tvars_mode\<close>} (for changing the print/parse mode 
  dynamically).  

  The input also supports short-hands for declaring default sorts (e.g., 
  @{text \<open>(_::linorder)\<close>} specifies that all default variables need to be 
  instances of the sort (type class) @{class \<open>linorder\<close>} and short-hands
  of overriding a suffice (or prefix) of the default type variables. For 
  example, @{text \<open>('state) foo _.\<close>} is a short-hand for 
  @{text \<open>('a, 'b, 'c, 'd, 'state) foo\<close>}.
}\<close>

section\<open>Implementation\<close>
subsection\<open>Theory Managed Data Structure\<close>
ML\<open>
signature HIDE_TVAR = sig
  datatype print_mode = print_all | print | noprint
  datatype tvar_subst = right | left
  datatype parse_mode = parse | noparse 
  type hide_varT     = {
                         name: string,
                         tvars:  typ list,
                         typ_syn_tab : (string * typ list*string) Symtab.table, 
                         print_mode: print_mode,
                         parse_mode: parse_mode
                       } 
  val parse_print_mode : string -> print_mode
  val parse_parse_mode : string -> parse_mode
  val register    : string -> print_mode option -> parse_mode option -> 
                    theory -> theory
  val update_mode : string -> print_mode option -> parse_mode option ->
                    theory -> theory
  val lookup      : theory -> string -> hide_varT option
  val hide_tvar_tr' : string -> Proof.context -> term list -> term 
  val hide_tvar_ast_tr : Proof.context -> Ast.ast list -> Ast.ast
  val hide_tvar_subst_ast_tr : tvar_subst -> Proof.context -> Ast.ast list 
                               -> Ast.ast
  val hide_tvar_subst_return_ast_tr : tvar_subst -> Proof.context 
                               -> Ast.ast list -> Ast.ast
end

structure Hide_Tvar : HIDE_TVAR = struct
  datatype print_mode = print_all | print | noprint
  datatype tvar_subst = right | left
  datatype parse_mode = parse | noparse 
  type hide_varT     = {
                         name: string,
                         tvars:  typ list,
                         typ_syn_tab : (string * typ list*string) Symtab.table, 
                         print_mode: print_mode,
                         parse_mode: parse_mode
                       } 
  type hide_tvar_tab = (hide_varT) Symtab.table
  fun hide_tvar_eq (a, a') = (#name a) = (#name a')
  fun merge_tvar_tab (tab,tab') = Symtab.merge hide_tvar_eq (tab,tab')

  structure Data = Generic_Data
  (
    type T = hide_tvar_tab
    val empty  = Symtab.empty:hide_tvar_tab
    val extend = I
    fun merge(t1,t2)  = merge_tvar_tab (t1, t2)
  );


  fun parse_print_mode "print_all" = print_all
    | parse_print_mode "print"     = print
    | parse_print_mode "noprint"   = noprint
    | parse_print_mode s           = error("Print mode not supported: "^s)
 
  fun parse_parse_mode "parse"     = parse
    | parse_parse_mode "noparse"   = noparse
    | parse_parse_mode s           = error("Parse mode not supported: "^s)

  fun update_mode typ_str print_mode parse_mode thy   =
    let       
      val ctx = Toplevel.context_of(Toplevel.theory_toplevel thy)
      val typ   = Syntax.parse_typ ctx typ_str (* no type checking *)
      val name = case typ of 
                   Type(name,_) => name
                 | _ => error("Complex type not (yet) supported.")
      fun update tab =
          let 
            val old_entry = (case Symtab.lookup tab name of 
                               SOME t => t 
                             | NONE   => error ("Type shorthand not registered: "^name))
            val print_m = case print_mode of
                            SOME m => m
                          | NONE   => #print_mode old_entry
            val parse_m = case parse_mode of 
                            SOME m => m
                          | NONE   => #parse_mode old_entry
            val entry = {
                          name        = name,
                          tvars       = #tvars old_entry,
                          typ_syn_tab = #typ_syn_tab old_entry,
                          print_mode  = print_m,
                          parse_mode  = parse_m
                        }
          in 
            Symtab.update (name,entry) tab
          end
    in  
     Context.theory_of ( (Data.map update) (Context.Theory thy))
    end

  fun lookup thy name =
    let
      val tab = (Data.get o Context.Theory) thy
    in 
      Symtab.lookup tab name
    end

  fun obtain_normalized_vname lookup_table vname = 
                           case List.find (fn e => fst e = vname) lookup_table of
                                SOME (_,idx) => (lookup_table, Int.toString idx)
                              | NONE => let 
                                          fun max_idx [] = 0  
                                            | max_idx ((_,idx)::lt) = Int.max(idx,max_idx lt)
  
                                          val idx = (max_idx lookup_table ) + 1
                                        in
                                          ((vname,idx)::lookup_table, Int.toString idx) end
  
  fun normalize_typvar_type lt (Type (a, Ts)) =
             let 
               fun switch (a,b) = (b,a)
               val (Ts', lt') = fold_map (fn t => fn lt => switch (normalize_typvar_type lt t)) Ts lt
             in 
               (lt', Type (a, Ts'))
             end
        | normalize_typvar_type lt (TFree (vname, S)) = 
             let 
               val (lt, vname) = obtain_normalized_vname lt (vname)
             in 
               (lt, TFree( vname, S))
             end
        | normalize_typvar_type lt (TVar (xi, S)) = 
             let 
               val (lt, vname) = obtain_normalized_vname lt (Term.string_of_vname xi)
             in 
               (lt, TFree( vname, S))
             end

  fun normalize_typvar_type' t = snd ( normalize_typvar_type [] t)

  fun mk_p s = s (* "("^s^")" *)

  fun key_of_type (Type(a, TS))      = mk_p (a^String.concat(map key_of_type TS))
    | key_of_type (TFree (vname, _)) = mk_p vname
    | key_of_type (TVar (xi, _ ))    = mk_p (Term.string_of_vname xi)
  val key_of_type' = key_of_type o normalize_typvar_type'


  fun normalize_typvar_term lt (Const (a, t)) = (lt, Const(a, t))
    | normalize_typvar_term lt (Free (a, t))  =  let  
                                 val (lt, vname) = obtain_normalized_vname lt a
                               in
                                 (lt, Free(vname,t))
                               end
    | normalize_typvar_term lt (Var (xi, t))   =
                               let  
                                 val (lt, vname) = obtain_normalized_vname lt (Term.string_of_vname xi)
                               in
                                 (lt, Free(vname,t))
                               end
    | normalize_typvar_term lt (Bound (i))    = (lt, Bound(i))  
    | normalize_typvar_term lt (Abs(s,ty,tr)) = 
                                let 
                                  val (lt,tr) = normalize_typvar_term lt tr
                                in
                                  (lt, Abs(s,ty,tr))
                                end
    | normalize_typvar_term lt (t1$t2) =
                                let 
                                  val (lt,t1) = normalize_typvar_term lt t1
                                  val (lt,t2) = normalize_typvar_term lt t2
                                in
                                  (lt, t1$t2)
                                end


  fun normalize_typvar_term' t = snd(normalize_typvar_term [] t) 

  fun key_of_term (Const(s,_)) = if String.isPrefix "\<^type>" s
                                 then Lexicon.unmark_type s
                                 else ""
    | key_of_term (Free(s,_))  = s
    | key_of_term (Var(xi,_))  = Term.string_of_vname xi 
    | key_of_term (Bound(_))   = error("Bound() not supported in key_of_term")
    | key_of_term (Abs(_,_,_)) = error("Abs() not supported in key_of_term")
    | key_of_term (t1$t2)      = (key_of_term t1)^(key_of_term t2)

  val key_of_term' = key_of_term o normalize_typvar_term'
  

  fun hide_tvar_tr' tname ctx terms =
      let

        val mtyp   = Syntax.parse_typ ctx tname (* no type checking *) 

        val (fq_name, _) = case mtyp of
                     Type(s,ts) => (s,ts)
                    | _        => error("Complex type not (yet) supported.") 

        val local_name_of = hd o rev o String.fields (fn c => c = #".")

        fun hide_type tname = Syntax.const("(_) "^tname) 
 
        val reg_type_as_term = Term.list_comb(Const(Lexicon.mark_type tname,dummyT),terms)
        val key = key_of_term' reg_type_as_term
        val actual_tvars_key = key_of_term reg_type_as_term

      in
        case lookup (Proof_Context.theory_of ctx) fq_name of 
          NONE   => raise Match
        | SOME e => let
                      val (tname,default_tvars_key) = 
                              case Symtab.lookup (#typ_syn_tab e) key  of
                                  NONE       => (local_name_of tname, "")
                                | SOME (s,_,tv) => (local_name_of s,tv)
                    in 
                    case (#print_mode e) of
                      print_all  => hide_type tname
                    | print      => if default_tvars_key=actual_tvars_key
                                    then hide_type tname
                                    else raise Match
                    | noprint    => raise Match
                   end  
      end
  
  fun hide_tvar_ast_tr ctx ast= 
      let 
        val thy = Proof_Context.theory_of ctx

        fun parse_ast ((Ast.Constant const)::[]) = (const,NONE)
          | parse_ast ((Ast.Constant sort)::(Ast.Constant const)::[]) 
                                      = (const,SOME sort)  
          |  parse_ast _ = error("AST type not supported.") 

        val (decorated_name, decorated_sort) = parse_ast ast 

        val name = Lexicon.unmark_type decorated_name
        val default_info = case lookup thy name of 
                              NONE   => error("No default type vars registered: "^name)
                            | SOME e => e
        val _ = if #parse_mode default_info = noparse 
                then error("Default type vars disabled (option noparse): "^name)
                else ()
        fun name_of_tvar tvar = case tvar of (TFree(n,_)) => n 
                                | _ => error("Unsupported type structure.")
        val type_vars_ast = 
            let fun mk_tvar n = 
               case decorated_sort of 
                  NONE      => Ast.Variable(name_of_tvar n)
                | SOME sort =>  Ast.Appl([Ast.Constant("_ofsort"),
                                               Ast.Variable(name_of_tvar n),
                                                   Ast.Constant(sort)])
            in
              map mk_tvar (#tvars default_info)
            end
      in
        Ast.Appl ((Ast.Constant decorated_name)::type_vars_ast)
      end   

  fun register typ_str print_mode parse_mode thy   =
    let   
      val ctx = Toplevel.context_of(Toplevel.theory_toplevel thy)
      val typ   = Syntax.parse_typ ctx typ_str
      val (name,tvars) = case typ  of Type(name,tvars) => (name,tvars)
                             | _             => error("Unsupported type structure.")
      
      val base_typ   = Syntax.read_typ ctx typ_str
      val (base_name,base_tvars) = case base_typ  of Type(name,tvars) => (name,tvars)
                             | _             => error("Unsupported type structure.")

      val base_key = key_of_type' base_typ
      val base_tvar_key = key_of_type base_typ

      val print_m = case print_mode of 
                      SOME m => m
                    | NONE   => print_all
      val parse_m = case parse_mode of 
                      SOME m => m
                    | NONE   => parse
      val entry = {
                    name       = name,
                    tvars        = tvars,
                    typ_syn_tab = Symtab.empty:((string * typ list * string) Symtab.table),
                    print_mode = print_m,
                    parse_mode = parse_m
                  }

      val base_entry = if name = base_name 
                       then 
                         {
                           name       = "",
                           tvars        = [],
                           typ_syn_tab = Symtab.empty:((string * typ list * string) Symtab.table),
                           print_mode = noprint,
                           parse_mode = noparse
                         }
                       else case lookup thy base_name of 
                            SOME e => e
                          | NONE   => error ("No entry found for "^base_name^
                                             " (via "^name^")")

      val base_entry = {
                    name       = #name base_entry,
                    tvars        = #tvars base_entry,
                    typ_syn_tab = Symtab.update (base_key, (name, base_tvars, base_tvar_key))
                                                (#typ_syn_tab (base_entry)), 
                    print_mode = #print_mode base_entry,
                    parse_mode = #parse_mode base_entry
                  }

      fun reg tab = let 
                      val tab = Symtab.update_new(name, entry) tab
                      val tab = if name = base_name 
                                then tab 
                                else Symtab.update(base_name, base_entry) tab
                    in
                      tab
                    end

      val thy =   Sign.print_translation
                      [(Lexicon.mark_type name, hide_tvar_tr' name)] thy

    in  
     Context.theory_of ( (Data.map reg) (Context.Theory thy))
     handle Symtab.DUP _ => error("Type shorthand already registered: "^name)
    end

  fun  hide_tvar_subst_ast_tr hole ctx (ast::[]) =
      let 

        val thy = Proof_Context.theory_of ctx
        val (decorated_name, args) = case ast
               of (Ast.Appl ((Ast.Constant s)::args)) => (s, args)
                | _ =>  error "Error in obtaining type constructor."

        val name = Lexicon.unmark_type decorated_name
        val default_info = case lookup thy name of
                              NONE   => error("No default type vars registered: "^name)
                            | SOME e => e
        val _ = if #parse_mode default_info = noparse 
                then error("Default type vars disabled (option noparse): "^name)
                else ()
        fun name_of_tvar tvar = case tvar of (TFree(n,_)) => n 
                                | _ => error("Unsupported type structure.")
        val type_vars_ast = map (fn n => Ast.Variable(name_of_tvar n)) (#tvars default_info)
        val type_vars_ast =  case hole of 
                right => (List.rev(List.drop(List.rev type_vars_ast, List.length args)))@args
              | left  => args@List.drop(type_vars_ast, List.length args)
      in
        Ast.Appl ((Ast.Constant decorated_name)::type_vars_ast)
      end   
    | hide_tvar_subst_ast_tr _ _ _ = error("hide_tvar_subst_ast_tr: empty AST.")

  fun hide_tvar_subst_return_ast_tr hole ctx (retval::constructor::[]) = 
         hide_tvar_subst_ast_tr hole ctx [Ast.Appl (constructor::retval::[])]
    | hide_tvar_subst_return_ast_tr _ _ _ = 
           error("hide_tvar_subst_return_ast_tr: error in parsing AST")


end
\<close>



subsection\<open>Register Parse Translations\<close>
syntax "_tvars_wildcard" :: "type \<Rightarrow> type" ("'('_') _") 
syntax "_tvars_wildcard_retval" :: "type \<Rightarrow> type \<Rightarrow> type" ("'('_, _') _")
syntax "_tvars_wildcard_sort" :: "sort \<Rightarrow> type \<Rightarrow> type" ("'('_::_') _")
syntax "_tvars_wildcard_right" :: "type \<Rightarrow> type" ("_ '_..")
syntax "_tvars_wildcard_left" :: "type \<Rightarrow> type" ("_ ..'_")

parse_ast_translation\<open>
  [
    (@{syntax_const "_tvars_wildcard_sort"}, Hide_Tvar.hide_tvar_ast_tr),
    (@{syntax_const "_tvars_wildcard"}, Hide_Tvar.hide_tvar_ast_tr),
    (@{syntax_const "_tvars_wildcard_retval"}, Hide_Tvar.hide_tvar_subst_return_ast_tr Hide_Tvar.right),
    (@{syntax_const "_tvars_wildcard_right"}, Hide_Tvar.hide_tvar_subst_ast_tr Hide_Tvar.right),
    (@{syntax_const "_tvars_wildcard_left"}, Hide_Tvar.hide_tvar_subst_ast_tr Hide_Tvar.left)
  ]
\<close>

subsection\<open>Register Top-Level Isar Commands\<close>
ML\<open>
  val modeP = (Parse.$$$ "("
       |-- (Parse.name --| Parse.$$$ ","
         -- Parse.name --| 
            Parse.$$$ ")"))
  val typ_modeP = Parse.typ -- (Scan.optional modeP ("print_all","parse"))

  val _ = Outer_Syntax.command @{command_keyword "register_default_tvars"}
          "Register default variables (and hiding mechanims) for a type."
          (typ_modeP >> (fn (typ,(print_m,parse_m)) => 
                  (Toplevel.theory 
                     (Hide_Tvar.register typ 
                                (SOME (Hide_Tvar.parse_print_mode print_m)) 
                                (SOME (Hide_Tvar.parse_parse_mode parse_m)))))); 

  val _ = Outer_Syntax.command @{command_keyword "update_default_tvars_mode"}
          "Update print and/or parse mode or the default type variables for a certain type."
          (typ_modeP >> (fn (typ,(print_m,parse_m)) => 
                  (Toplevel.theory 
                     (Hide_Tvar.update_mode typ 
                                (SOME (Hide_Tvar.parse_print_mode print_m)) 
                                (SOME (Hide_Tvar.parse_parse_mode parse_m)))))); 
\<close>

section\<open>Examples\<close>
subsection\<open>Print Translation\<close>
datatype ('a, 'b) foobar = foo 'a | bar 'b 
type_synonym ('a, 'b, 'c, 'd) baz = "('a+'b, 'a \<times> 'b) foobar"

definition hide_tvar_f::"('a, 'b) foobar \<Rightarrow> ('a, 'b) foobar \<Rightarrow> ('a, 'b) foobar" 
     where "hide_tvar_f a b = a"
definition hide_tvar_g::"('a, 'b, 'c, 'd) baz \<Rightarrow> ('a, 'b, 'c, 'd) baz \<Rightarrow> ('a, 'b, 'c, 'd) baz" 
     where "hide_tvar_g a b = a"

assert[string_of_thm_equal,
       thm_def="hide_tvar_f_def", 
       str="hide_tvar_f (a::('a, 'b) foobar) (b::('a, 'b) foobar) = a"]
assert[string_of_thm_equal,
       thm_def="hide_tvar_g_def", 
       str="hide_tvar_g (a::('a + 'b, 'a \<times> 'b) foobar) (b::('a + 'b, 'a \<times> 'b) foobar) = a"]

register_default_tvars  "('alpha, 'beta) foobar" (print_all,parse)
register_default_tvars  "('alpha, 'beta, 'gamma, 'delta) baz" (print_all,parse)

update_default_tvars_mode "_ foobar" (noprint,noparse)
assert[string_of_thm_equal,
       thm_def="hide_tvar_f_def",
       str="hide_tvar_f (a::('a, 'b) foobar) (b::('a, 'b) foobar) = a"]
assert[string_of_thm_equal,
       thm_def="hide_tvar_g_def", 
       str="hide_tvar_g (a::('a + 'b, 'a \<times> 'b) foobar) (b::('a + 'b, 'a \<times> 'b) foobar) = a"]

update_default_tvars_mode "_ foobar" (print_all,noparse)

assert[string_of_thm_equal,
       thm_def="hide_tvar_f_def", str="hide_tvar_f (a::(_) foobar) (b::(_) foobar) = a"]
assert[string_of_thm_equal,
       thm_def="hide_tvar_g_def", str="hide_tvar_g (a::(_) baz) (b::(_) baz) = a"]

subsection\<open>Parse Translation\<close>
update_default_tvars_mode "_ foobar" (print_all,parse)

declare [[show_types]]
definition hide_tvar_A :: "'x \<Rightarrow> (('x::linorder) foobar) .._"
  where "hide_tvar_A x = foo x"
assert[string_of_thm_equal,
       thm_def="hide_tvar_A_def", str="hide_tvar_A (x::'x) = foo x"]

definition hide_tvar_A' :: "'x \<Rightarrow> (('x,'b) foobar) .._"
  where "hide_tvar_A' x = foo x"
assert[string_of_thm_equal,
       thm_def="hide_tvar_A'_def", str="hide_tvar_A' (x::'x) = foo x"]

definition hide_tvar_B' :: "(_) foobar \<Rightarrow> (_) foobar \<Rightarrow> (_) foobar" 
  where "hide_tvar_B' x y = x"
assert[string_of_thm_equal,
       thm_def="hide_tvar_A'_def", str="hide_tvar_A' (x::'x) = foo x"]


definition hide_tvar_B :: "(_) foobar \<Rightarrow> (_) foobar \<Rightarrow> (_) foobar" 
  where "hide_tvar_B x y = x"
assert[string_of_thm_equal,
       thm_def="hide_tvar_B_def", str="hide_tvar_B (x::(_) foobar) (y::(_) foobar) = x"]

definition hide_tvar_C :: "(_) baz \<Rightarrow> (_) foobar \<Rightarrow> (_) baz" 
  where "hide_tvar_C x y = x"
assert[string_of_thm_equal,
       thm_def="hide_tvar_C_def", str="hide_tvar_C (x::(_) baz) (y::(_) foobar) = x"]

definition hide_tvar_E :: "(_::linorder) baz \<Rightarrow> (_::linorder) foobar \<Rightarrow> (_::linorder) baz" 
  where "hide_tvar_E x y = x"
assert[string_of_thm_equal,
       thm_def="hide_tvar_C_def", str="hide_tvar_C (x::(_) baz) (y::(_) foobar) = x"]

definition hide_tvar_X :: "(_, 'retval::linorder) baz 
                 \<Rightarrow> (_,'retval) foobar 
                 \<Rightarrow> (_,'retval) baz"
  where "hide_tvar_X x y = x"

end
