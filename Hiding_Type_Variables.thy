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
text\<open>This theory implements a mechanism for declaring default type 
     variables for data types. This comes handy for complex data types 
     with many type variables. The theory sets up both configurable print and 
     parse translations that allows for replacing @{emph \<open>all\<close>} type variables 
     by @{text  \<open>(_)\<close>}, e.g., @{text \<open>('a, 'b, 'c, 'd, 'e) foo\<close>} becomes 
     @{text \<open>(_) foo\<close>}. The use of this shorthand in output (printing) and input 
     (parsing) is, on a per-type basis, user-configurable.\<close>


section\<open>Theory Managed Data Structure\<close>
ML\<open>
signature HIDE_TVAR = sig
  datatype print_mode = always | default_only | noprint
  datatype parse_mode = active | noparse 
  type hide_varT     = {
                         name: string,
                         tvars:  typ list,
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
  val hide_tvar_tr' : string -> Proof.context -> typ -> term list -> term 
  val hide_tvar_ast_tr : Proof.context -> Ast.ast list -> Ast.ast
end

structure Hide_Tvar : HIDE_TVAR = struct
  datatype print_mode = always | default_only | noprint
  datatype parse_mode = active | noparse 
  type hide_varT     = {
                         name: string,
                         tvars:  typ list,
                         print_mode: print_mode,
                         parse_mode: parse_mode
                       } 
  type hide_tvar_tab = (hide_varT) Symtab.table
  fun merge_assert_tab (tab,tab') = Symtab.merge (op =) (tab,tab')

  structure Data = Generic_Data
  (
    type T = hide_tvar_tab
    val empty  = Symtab.empty:hide_tvar_tab
    val extend = I
    fun merge(t1,t2)  = merge_assert_tab (t1, t2)
  );


  fun parse_print_mode "always"       = always
    | parse_print_mode "default_only" = default_only
    | parse_print_mode "noprint"      = noprint
    | parse_print_mode s              = error("Print mode not supported: "^s)
 
  fun parse_parse_mode "active"       = active
    | parse_parse_mode "noparse"      = noparse
    | parse_parse_mode s              = error("Parse mode not supported: "^s)

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
                          name       = name,
                          tvars      = #tvars old_entry,
                          print_mode = print_m,
                          parse_mode = parse_m
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

  fun hide_tvar_tr' tname ctx typ terms =
      let
        val mtyp   = Syntax.parse_typ ctx tname (* no type checking *) 
        val fq_name = case mtyp of 
                     Type(s,_) => s
                    | _        => error("Complex type not (yet) supported.") 
        val local_name_of = hd o rev o String.fields (fn c => c = #".")
        val local_tname = local_name_of tname 
        val hide_type = Syntax.const("(_) "^(local_tname))  
        val reg_type = Term.list_comb(Const(local_tname,typ),terms)
      in
        case lookup (Proof_Context.theory_of ctx) fq_name of 
          NONE   => reg_type
        | SOME e => case (#print_mode e) of
                      always       => hide_type
                    | default_only => hide_type (* TODO *)
                    | noprint      => reg_type 
      end
  
  fun hide_tvar_ast_tr ctx (a::_)= 
      let 
        val thy = Proof_Context.theory_of ctx
        val decorated_name = case a of 
                    Ast.Constant s => s
                   |  _ => error("AST type not supported.") 
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
      in
        Ast.Appl ((Ast.Constant decorated_name)::type_vars_ast)
      end   
  | hide_tvar_ast_tr _ _ = error("Empty AST.")

  fun register typ_str print_mode parse_mode thy   =
    let   
      val ctx = Toplevel.context_of(Toplevel.theory_toplevel thy)
      val typ   = Syntax.parse_typ ctx typ_str
      val (name,tvars) = case typ  of Type(name,tvars) => (name,tvars)
                             | _             => error("Unsupported type structure.")
      val print_m = case print_mode of 
                      SOME m => m
                    | NONE   => always
      val parse_m = case parse_mode of 
                      SOME m => m
                    | NONE   => active
      val entry = {
                    name       = name,
                    tvars        = tvars,
                    print_mode = print_m,
                    parse_mode = parse_m
                  }
      fun reg tab = Symtab.update_new(name, entry) tab
      val thy =   Sign.typed_print_translation 
                      [("\<^type>"^name, hide_tvar_tr' name)] thy

    in  
     Context.theory_of ( (Data.map reg) (Context.Theory thy))
     handle Symtab.DUP _ => error("Type shorthand already registered: "^name)
    end

end
\<close>

section\<open>Register Parse Translation\<close>
syntax "_tvars_wildcard" :: "type \<Rightarrow> type" ("'('_') _")
parse_ast_translation\<open>
  [(@{syntax_const "_tvars_wildcard"}, Hide_Tvar.hide_tvar_ast_tr)]
\<close>

section\<open>Register Top-Level Isar Commands\<close>
ML\<open>
  val modeP = (Parse.$$$ "("
       |-- (Parse.name --| Parse.$$$ ","
         -- Parse.name --| 
            Parse.$$$ ")"))
  val typ_modeP = Parse.typ -- (Scan.optional modeP ("always","active"))

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
datatype ('alpha, 'beta) foobar = foo 'alpha | bar 'beta 
type_synonym ('a, 'b, 'c, 'd) baz = "('a+'b, 'a \<times> 'b) foobar"

definition f::"('a, 'b) foobar \<Rightarrow> ('a, 'b) foobar \<Rightarrow> ('a, 'b) foobar" 
     where "f a b = a"
definition g::"('a, 'b, 'c, 'd) baz \<Rightarrow> ('a, 'b, 'c, 'd) baz \<Rightarrow> ('a, 'b, 'c, 'd) baz" 
     where "g a b = a"

assert[string_of_thm_equal,
       thm_def="f_def", 
       str="f (a::('a, 'b) foobar) (b::('a, 'b) foobar) = a"]
assert[string_of_thm_equal,
       thm_def="g_def", 
       str="g (a::('a + 'b, 'a \<times> 'b) foobar) (b::('a + 'b, 'a \<times> 'b) foobar) = a"]

register_default_tvars  "('alpha, 'beta) foobar" (always,active)
register_default_tvars  "('alpha, 'beta, 'gamma, 'delta) baz" (always,active)

update_default_tvars_mode "_ foobar" (noprint,noparse)
assert[string_of_thm_equal,
       thm_def="f_def", 
       str="f (a::('a, 'b) foobar) (b::('a, 'b) foobar) = a"]
assert[string_of_thm_equal,
       thm_def="g_def", 
       str="g (a::('a + 'b, 'a \<times> 'b) foobar) (b::('a + 'b, 'a \<times> 'b) foobar) = a"]

update_default_tvars_mode "_ foobar" (always,noparse)

assert[string_of_thm_equal,
       thm_def="f_def", str="f (a::(_) foobar) (b::(_) foobar) = a"]
assert[string_of_thm_equal,
       thm_def="g_def", str="g (a::(_) foobar) (b::(_) foobar) = a"]

subsection\<open>Parse Translation\<close>
update_default_tvars_mode "_ foobar" (noprint,active)

declare [[show_types]]
definition A :: "'alpha \<Rightarrow> (_) foobar"
  where "A =  foo"

definition B :: "(_) foobar \<Rightarrow> (_) foobar \<Rightarrow> (_) foobar" 
  where "B x y = x"

definition C :: "(_) baz \<Rightarrow> (_) foobar \<Rightarrow> (_) baz" 
  where "C x y = x"


end
