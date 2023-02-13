(***********************************************************************************
 * Copyright (c) Achim D. Brucker
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

chapter\<open>Code Reflection for Isabelle\<close>
theory
  "Code_Reflection"
imports 
  Main
keywords
  "reflect_ML_exports" :: thy_decl
begin

subsection\<open>Reflecting exported SML code\<close>
ML\<open>
  fun reflect_local_ML_exports args trans =  let
    fun eval_ML_context ctxt = let 
      fun is_sml_file f = String.isSuffix ".ML" (Path.implode (#path f))
      val files = (map (Generated_Files.check_files_in (Context.proof_of ctxt)) args) 
      val ml_files = filter is_sml_file (map #1 (maps Generated_Files.get_files_in files))
      val ml_content = map (fn f => Syntax.read_input (Bytes.content (#content f))) ml_files
      fun eval ml_content   = fold (fn sml => (ML_Context.exec 
                                           (fn () => ML_Context.eval_source ML_Compiler.flags sml))) 
                                   ml_content 
    in 
      (eval ml_content #> Local_Theory.propagate_ml_env) ctxt
    end
  in
    Toplevel.generic_theory eval_ML_context trans
  end


  val files_in_theory =
    (Parse.underscore >> K [] || Scan.repeat1 Parse.path_binding) --
      Scan.option (\<^keyword>\<open>(\<close> |-- Parse.!!! (\<^keyword>\<open>in\<close> 
                     |-- Parse.theory_name --| \<^keyword>\<open>)\<close>));

  val _ =
    Outer_Syntax.command \<^command_keyword>\<open>reflect_ML_exports\<close> 
      "evaluate generated Standard ML files"
      (Parse.and_list1 files_in_theory  >> (fn args => reflect_local_ML_exports args));
\<close>

export_code  Suc in SML   module_name Natural

reflect_ML_exports _

ML\<open>
open Natural
\<close>

end

