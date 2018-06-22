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
 * Dependencies: None
 ***********************************************************************************)

chapter\<open>An Assertion Framework for Isabelle\<close>
theory
  "Assert"
imports 
  Main
keywords
  "assert"::thy_decl
begin
text\<open>This theory implements a simple @{emph \<open>assertion framework\<close>} for Isabelle:
     it introduces a new top level command @{bold \<open>assert\<close>} that allows for 
     checking an assertion. The overall idea is very similar to the assert
     annotations that man unit test frameworks provide. New assertion 
     types can be registered on the ML level of Isabelle.\<close>

section\<open>The Core Assertion Framework\<close>

ML\<open>
signature ASSERT = sig
  datatype arg_class = optional | mandatory  
  type arg_parseT    = ((string * Position.T) * string)
  type arg_specT     = (string * arg_class * string option)
  type checkT        = theory -> arg_parseT list -> unit
  type assertionT    = {
                         name : string,
                         description: string,
                         assertion: checkT  
                       }
  val dispatch : string -> arg_parseT list -> theory -> theory
  val register : assertionT-> theory -> theory
end

structure Assert : ASSERT = struct 
  datatype arg_class = optional | mandatory  
  type arg_parseT    = ((string * Position.T) * string)
  type arg_specT     = (string * arg_class * string option)
  type checkT        = theory -> arg_parseT list -> unit
  type assertionT    = {
                         name : string,
                         description: string,
                         assertion: checkT  
                       }
  type assert_tab = (assertionT) Symtab.table

  fun assertion_eq (a, a') = (#name a) = (#name a')

  fun merge_assert_tab (tab,tab') = Symtab.merge assertion_eq (tab,tab')

  structure Data = Generic_Data
  (
    type T = assert_tab
    val empty  = Symtab.empty:assert_tab
    val extend = I
    fun merge(t1,t2)  = merge_assert_tab (t1, t2)
  );

  fun register assertion thy   =
    let       
      val name = #name assertion
      fun reg tab = Symtab.update_new(name, assertion) tab
    in  
     Context.theory_of ( (Data.map reg) (Context.Theory thy))
     handle Symtab.DUP _ => error("Assertion already defined: "^name)
    end

  fun dispatch assertion args thy = 
    let
      val tab = (Data.get o Context.Theory) thy
      val _ = (case Symtab.lookup tab assertion of 
                 SOME a => (#assertion a)  thy args 
               | NONE   => error ("No assertion registered: "^assertion))
    in
      thy
    end
end
\<close>

ML\<open>
  val argP =
      Parse.position Parse.name 
      -- Scan.optional (Parse.$$$ "=" |-- Parse.!!! Parse.string) "";

  val argsP =
      (Parse.$$$ "["
       |-- (Parse.position Parse.name --
           (Scan.optional(Parse.$$$ "," |-- (Parse.enum "," argP))) []))
       --| Parse.$$$ "]"

  val _ = Outer_Syntax.command @{command_keyword "assert"}
          "theory assertion"
          (argsP >> (fn ((assert,_),args) =>
                 (Toplevel.theory (Assert.dispatch assert args)))); 
\<close>

section\<open>Utilities for Writing And Registering Assertions on the ML Level\<close>
ML\<open>
signature ASSERT_UTIL = sig
    val get_mandatory_arg : (string * string) list -> string -> string
    val get_optional_arg  : (string * string) list -> string -> string option
    val parse_args        : Assert.arg_specT list -> Assert.arg_parseT list 
                            -> (string * string) list
    val str_of_thm        : Proof.context -> thm -> string
  end

structure Assert_Util : ASSERT_UTIL = struct
fun clean_ws s = 
  let
    fun ws (c1::c2::cs) = if c1 = #" " andalso c2 = #" " 
                          then  ws (c2::cs) 
                          else c1::(ws (c2::cs))  
      | ws [c] = [c]
      | ws []  = []
  in 
    String.implode (ws (String.explode 
                     (String.map (fn c => if c = #"\n" then #" " else c) s)))
  end 

  fun str_of_thm ctx thm =  clean_ws (Print_Mode.setmp []
                            (Thm.string_of_thm 
                                (ctx |> Config.put show_markup false 
                                     |> Config.put show_question_marks false
                                     |> Config.put show_types true
                                )
                            ) thm) 
  fun parse_args spec (((name,_),value)::args) = 
        let
          val arg = case filter (fn (name',_,_) => (name' = name)) spec of
                      [v] => v
                    | []  => error ("Not a valid argument: "^name)
                    | _   => error ("Argument given multiple times: "^name)
          val spec = filter (fn arg' => arg <> arg') spec
        in
          (name, value)::(parse_args spec args)
        end
    | parse_args spec [] = 
        let
          val default_args = map (fn (n, _, v) => (n, the v))
                                 (filter (fn (_,_,value) => value <> NONE) spec)
          val missing_args = filter (fn (_,typ,value) => typ = Assert.mandatory 
                                                     andalso value = NONE) spec
        in 
          if missing_args = []
          then default_args
          else
            error(List.foldl (fn (s,s') => (s ^"\n"^s')) ""
                   (map (fn (n,_,_) => "Mandatory argument missing: "^n) 
                                            missing_args))
        end
  fun get_mandatory_arg ((name', s)::vals) name = 
        if name = name' then s 
        else get_mandatory_arg vals name
    | get_mandatory_arg [] name = error ("Mandatory arg "^name^" not found.")  
  fun get_optional_arg ((name',s)::vals) name = 
        if name = name' then SOME s 
        else get_optional_arg vals name
    | get_optional_arg [] _ = NONE

end
\<close>

section\<open>Am Example\<close>

ML\<open>
signature STRING_OF_THM_CMP_ASSERT = sig
  val string_of_thm_equal    : Assert.assertionT
  val string_of_thm_notequal : Assert.assertionT 
end

structure String_of_thm_cmp_assert : STRING_OF_THM_CMP_ASSERT = struct
  fun assert_string_of_thm_cmp cmp ctx thm str = 
      let
        val str' = Assert_Util.str_of_thm ctx thm
      in
        if not (cmp(str,str'))
        then error ("assert_string_of_thm_equal failed, got '"
                    ^str'^"' expected '"^str^"'.")
        else ()
      end

  fun assert_string_of_thm_cmp_args cmp thy args  = 
        let 
          val ctx = Toplevel.context_of(Toplevel.theory_toplevel thy)
          val spec = [("thm_def", Assert.mandatory, NONE), 
                      ("str", Assert.mandatory, NONE)]
          val vals = Assert_Util.parse_args spec args
          val thm_name = Assert_Util.get_mandatory_arg vals "thm_def"
          val thm_def = (Global_Theory.get_thm thy thm_name) 
          val str     = Assert_Util.get_mandatory_arg vals "str" 
        in
          assert_string_of_thm_cmp cmp ctx thm_def str
        end

  val string_of_thm_equal = {
        name = "string_of_thm_equal",
        description = "Check that string representation (including types) "
                      ^"matches given string.",
        assertion = (assert_string_of_thm_cmp_args (op =))
     }

  val string_of_thm_notequal = {
        name = "string_of_thm_notequal",
        description = "Check that string representation (including types) "
                      ^"does not match given string.",
        assertion = (assert_string_of_thm_cmp_args (op <>))
     }
end
\<close>
  
setup\<open> 
  fn thy => thy
  |> (Assert.register String_of_thm_cmp_assert.string_of_thm_equal)
  |> (Assert.register String_of_thm_cmp_assert.string_of_thm_notequal)
\<close>

section\<open>Examples\<close>

assert[string_of_thm_equal,
       thm_def="True_def", str="True \<equiv> (\<lambda>x::bool. x) = (\<lambda>x::bool. x)"]
assert[string_of_thm_notequal,
       thm_def="True_def", str="False \<equiv> (\<lambda>x::bool. x) = (\<lambda>x::bool. x)"]
end
