(***********************************************************************************
 * Copyright (c) 2022 Achim D. Brucker
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
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
 ***********************************************************************************)

section\<open>Implementation\<close>
theory
  Simple_Oracle
imports
  Main 
keywords
  "check" :: diag 
begin

subsection\<open>Implementation\<close>

text\<open>The following ML-block defines a configuration attribute named
     oracle\_path that allows for configuring the name and path of the 
     oracle binary (inspect the theory oracle\_example for an example how to use
     this configuration attribute).\<close>
ML\<open>
val oracle_path = let
  val (oracle_path_config, oracle_path_setup) =
      Attrib.config_string (Binding.name "oracle_path") (K "oracle")
in
  Context.>>(Context.map_theory oracle_path_setup);
  oracle_path_config
end
\<close>

text\<open>The following ML-Block defines a structure oracle that provides two methods:
\<^item> `exec' is a basic method that calls oracle on the provided input file and returns
  the output of the oracle process as string
\<^item> `oracle\_cmd' is a method that obtains the formulae from subgoal i of the current 
   proof state, converts it to a string that is written into a temporary file. The
   previously described method exec is invoked on this file name. The output is finally
   written to the output window of Isabelle.
\<close>
ML\<open>
structure oracle = struct
    open OS.FileSys OS.Process
    fun exec {oracle_path, error_detail} filename = let 
        val tmpname = tmpName()
        val err_tmpname = tmpName()      
        fun plural 1 = "" | plural _ = "s"
        val oracle = case oracle_path of 
                         SOME s => s
                       | NONE   => raise error ("oracle_path not specified")
        val cmdline = oracle ^ " \"" ^ filename ^ "\" > " ^ tmpname ^ " 2> " ^ err_tmpname
        val sysres = system cmdline
        val (errmsg, rest) = File.read_lines (Path.explode err_tmpname) |> chop error_detail
        val msg = cat_lines  (File.read_lines (Path.explode tmpname))
        val _ = OS.FileSys.remove err_tmpname
        val _ = OS.FileSys.remove tmpname
        val _ = if isSuccess (sysres) then ()
                else let 
                    val _ = warning ("oracle failed on " ^ filename ^ "\nCommand: " ^ cmdline ^
                                  "\n\nOutput:\n" ^
                                  cat_lines (errmsg @ (if null rest then [] else
                                                    ["(... " ^ string_of_int (length rest) ^
                                                     " more line" ^ plural (length rest) ^ ")"])))
                    in raise error ("oracle failed on " ^ filename) end
      in
        msg 
    end

  fun oracle_cmd args i st = let  
    val thy = Toplevel.theory_of st
    val proof_state = (Toplevel.proof_of st)
    val ctx = Proof.context_of proof_state
    val goal_term = Logic.get_goal (Thm.prop_of (#goal (Proof.goal proof_state))) i
    val goal_str = Sledgehammer_Util.hackish_string_of_term ctx goal_term
    val tmpname = tmpName()
    val _ = File.write (Path.explode tmpname) goal_str
    val oracle =  Config.get_global thy oracle_path
    val res = exec {error_detail=5, oracle_path = SOME oracle}
                (Path.implode (Path.explode tmpname))
    in
      writeln(res)
    end 
end
\<close>

text\<open>This ML-Block provides the high-level Isar command oraclecheck\<close>
ML\<open>
val parse_arg =
  Parse.name --
    (Scan.optional (\<^keyword>\<open>=\<close> |--
      (((Parse.name || Parse.float_number) >> single) ||
        (\<^keyword>\<open>[\<close> |-- Parse.list1 Parse.name --| \<^keyword>\<open>]\<close>))) ["true"]);
val parse_args =
  \<^keyword>\<open>[\<close> |-- Parse.list1 parse_arg --| \<^keyword>\<open>]\<close> || Scan.succeed [];

val _ =
  Outer_Syntax.command \<^command_keyword>\<open>check\<close>
    "search for counter example using an external oracle"
    (parse_args -- Scan.optional Parse.nat 1 >>
      (fn (args, i) => Toplevel.keep_proof (oracle.oracle_cmd args i)));
\<close>

subsection\<open>Example\<close>

text\<open>This is a simple example of a providing a new top-level Isar command that
     analyses the current goal using an external system command. The example
     setup provides a fake implementation of such a program as shell script, 
     named ``bin/oracle''. We use the declare command to configure 
     oracle\_path attribute to point to this shell script: \<close>

declare [[oracle_path="Simple_Oracle/bin/oracle"]]


lemma "True = False \<or> False"
  text\<open>the following command reads, by default, the first subgoal (it also takes
   the subgoal number as an optional argument) and invoked the external tool with 
   this formula. The output is shown in the output window of Isabelle/JEdit. Note that
   the proof state is not modified. The command fails, if the external process returns
   an error (i.e., a non-zero exit code).
  \<close>
  check 
  oops


end
