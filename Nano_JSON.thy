(***********************************************************************************
 * Copyright (c) 2019 Achim D. Brucker
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

chapter\<open>An Import/Export of JSON-like Formats for Isabelle/HOL\<close>
theory
  "Nano_JSON"
imports 
  Complex_Main
  "Assert" (* Can be removed, after removing all assertions. *)
begin
text\<open>
  This theory implements an import/export format for Isabelle/HOL that is inspired by 
  JSON (JavaScript Object Notation). While the format defined in this theory is inspired 
  by the JSON standard (@{url "https://www.json.org"}), it is not fully compliant. Most 
  notably, 

  \<^item> only basic support for Unicode characters 
  \<^item> numbers are mapped to @{type "real"}, which is not a faithful representation of IEEE floating 
    point numbers, moreover, we extended the abstract syntax to allow for representing integers as 
    @{type "int"}.

  Still, our JSON-like import/expert should work with most real-world JSON files, i.e., simplifying 
  the data exchange between Isabelle/HOL and tools that can read/write JSON. 
\<close>

section\<open>Defining a JSON-like Data Structure\<close>

datatype number = INTEGER int | REAL real
datatype json = OBJECT "(string * json) list"
  | ARRAY "json list"
  | NUMBER "number" 
  | STRING "string"
  | BOOL "bool"
  | NULL

text\<open>
  The translation of the data type @{typ "json"} to ML is straight forward. In addition, we also 
  provide methods for converting JSON instances between the representation as Isabelle terms and 
  the representation as ML data structure.
\<close>
ML\<open>
signature NANO_JSON_TYPE = sig
    datatype number = INTEGER of int | REAL of real
  
    datatype json = OBJECT of (string * json) list
                  | ARRAY of json list
                  | NUMBER of number
                  | STRING of string
                  | BOOL of bool
                  | NULL

    val term_of_json: json -> term
    val json_of_term: term -> json
end
structure Nano_Json_Type : NANO_JSON_TYPE = struct
    datatype number = INTEGER of int | REAL of real
  
    datatype json = OBJECT of (string * json) list
                  | ARRAY of json list
                  | NUMBER of number
                  | STRING of string
                  | BOOL of bool
                  | NULL

    fun real_to_rat_approx r = let
           val _ = warning ("Conversion of real numbers is not IEEE compliant!")
           val rat = Real.toDecimal r 
           fun pow (_, 0) = 1  
             | pow (x, n) = if n mod 2 = 0 then pow (x*x, n div 2)  
                                           else x * pow (x*x, n div 2); 
           fun rat_of_dec rat = let
                 val sign = #sign rat
                 val digits = #digits rat
                 val exp = #exp rat
                 fun numerator_of _ [] = 0
                   | numerator_of c (x::xs) = x*pow(10,c) + (numerator_of (c+1) xs)
                 val numerator_abs = numerator_of 0 (rev digits)
                 val denominator = pow(10, (List.length digits - exp))  
               in
                 (if sign then ~ numerator_abs else numerator_abs, denominator)
               end
        in
          case #class rat of      
             IEEEReal.ZERO      => (0,0)
           | IEEEReal.SUBNORMAL => rat_of_dec rat
           | IEEEReal.NORMAL    => rat_of_dec rat 
           | IEEEReal.INF => error "Real is INF, not yet supported."
           | IEEEReal.NAN => error "Real is NaN, not yet supported."
        end

    fun mk_divide t1 t2 = @{const Rings.divide_class.divide (real)} $ t1 $ t2
    fun mk_real_num i = HOLogic.mk_number @{typ "Real.real"} i
    fun mk_real (p,q) = if q = 1 then mk_real_num p else mk_divide (mk_real_num p) (mk_real_num q)

    fun dest_real (@{const Rings.divide_class.divide (real)} $a$b) = Real.fromInt(HOLogic.dest_number a |> snd) 
                                                                     / Real.fromInt(HOLogic.dest_number b |> snd)
      | dest_real t = Real.fromInt (HOLogic.dest_number t |> snd)


    fun term_of_json (OBJECT l) = @{const "OBJECT"}
                                  $(HOLogic.mk_list ((HOLogic.mk_prodT (HOLogic.stringT,@{typ "json"}))) 
                                    (map (fn (s,j) => HOLogic.mk_tuple[HOLogic.mk_string s, term_of_json j]) l))
      | term_of_json (ARRAY l)  = @{const "ARRAY"}
                                  $(HOLogic.mk_list ( @{typ "json"}) (map term_of_json l))
      | term_of_json (NUMBER (INTEGER i)) = @{const "NUMBER"}
                                            $(@{const "INTEGER"}$(HOLogic.mk_number @{typ "int"} i)) 
      | term_of_json (NUMBER (REAL r)) = @{const "NUMBER"}
                                         $(@{const "REAL"}$(mk_real (real_to_rat_approx r)))
      | term_of_json (STRING s) = @{const "STRING"}$(HOLogic.mk_string s)
      | term_of_json (BOOL v) = @{const "BOOL"}$(if v then @{const "True"} else @{const "False"}) 
      | term_of_json (NULL) = @{const "NULL"} 

    fun json_of_term t = let 
        fun dest_key_value [string, json] = (HOLogic.dest_string string, json_of json)
          | dest_key_value _              = error "dest_key_value: not a key-value pair." 
        and json_of (@{const "OBJECT"}  $ l) = OBJECT (map (dest_key_value o HOLogic.strip_tuple) (HOLogic.dest_list l))
          | json_of (@{const "ARRAY"}  $ l) = ARRAY (map json_of (HOLogic.dest_list l))
          | json_of (@{const "NUMBER"} $ @{const "INTEGER"} $ i) = (NUMBER (INTEGER (HOLogic.dest_numeral i)))
          | json_of (@{const "NUMBER"} $ @{const "REAL"} $ r) = (NUMBER (REAL (dest_real r)))
          | json_of (@{const "STRING"} $ s) = STRING (HOLogic.dest_string s)
          | json_of (@{const "BOOL"} $ @{const "True"}) = BOOL true
          | json_of (@{const "BOOL"} $ @{const "False"}) = BOOL true 
          | json_of @{const "NULL"} = NULL
          | json_of _ = error "Term not supported in json_of_term."
    in
        if type_of t = @{typ "json"} then json_of t 
                                     else error "Term not of type json!"
    end
end
\<close>

section\<open>Serializing Nano JSON\<close>


section\<open>Parsing Nano JSON\<close>




end
