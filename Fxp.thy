(***********************************************************************************
 * Copyright (c) 2021 Achim D. Brucker
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

(***********************************************************************************

# Changelog

This comment documents all notable changes to this file in a format inspired by 
[Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres 
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2021-04-29
- Initial release as part of "Isabelle Hacks". 

***********************************************************************************)

chapter\<open>The Functional XML Parser for Isabelle\<close>
theory Fxp
  imports Main 
begin 


(*
The files in the directory "fxp" are part of fxp (The Functional XML Parser).
It was modified to work with Isabelle/ML by Achim D. Brucker.

It was downloaded from http://www2.cs.tum.edu/projects/Fxp/a

                fxp: A Functional XML Parser
	       Version 2.0, 25.06.2004

	 ------------------------------------------------

COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.

Copyright 1999-2004 by Andreas Neumann and Alexandru Berlea, TU Munich

Permission to use, copy, modify, and distribute this software and 
its documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that 
both the copyright notice and this permission notice and warranty
disclaimer appear in supporting documentation.

The TU Munich disclaims all warranties with regard to this 
software, including all implied warranties of merchantability and 
fitness.  In no event shall the TU Munich be liable for any 
special, indirect or consequential damages or any damages whatsoever 
resulting from loss of use, data or profits, whether in an action of 
contract, negligence or other tortious action, arising out of or in 
connection with the use or performance of this software.

	------------------------------------------------

This software was produced with the help of software, tools, and code
from SML of New Jersey. Therefore we repeat here the copyright notice 
SML of New Jersey:

STANDARD ML OF NEW JERSEY COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.

Copyright (c) 1989-1998 by Lucent Technologies

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both the copyright notice and this permission notice and warranty
disclaimer appear in supporting documentation, and that the name of
Lucent Technologies, Bell Labs or any Lucent entity not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

Lucent disclaims all warranties with regard to this software,
including all implied warranties of merchantability and fitness. In no
event shall Lucent be liable for any special, indirect or
consequential damages or any damages whatsoever resulting from loss of
use, data or profits, whether in an action of contract, negligence or
other tortious action, arising out of or in connection with the use
or performance of this software.

	------------------------------------------------

*)
ML\<open>
structure HashString = 
struct 
fun hashString _ = ()
end

structure HashTable = 
struct 
datatype ('a, 'b) hash_table = HT of {
	 eq_pred : ('a * 'a) -> bool,
	 not_found : exn,
	 table: (('a*'b) list) Unsynchronized.ref
}
fun mkTable (_, eq) (_, notFound) = HT{
	    eq_pred = eq,
	    not_found = notFound,
	    table = Unsynchronized.ref ([])
	  }
fun find    (HT{table,...}) key =
                           Option.map (fn (_,b) => b) ((List.find (fn (x,_) => x =key) (!table))) 
fun insert  (HT{table,...}) (k,v) =  (table:=([(k,v)]@ (!(table))))
end
\<close>

SML_import List.foldl 
SML_import List.foldr 
SML_import Option.valOf
SML_import Option.isSome
SML_import Char.ord
SML_import Char.chr

ML\<open>
val ord = Char.ord;
val chr = Char.chr; 
val print =  writeln;
val foldr=List.foldr;
val foldl=List.foldl;
val valOf = Option.valOf;
val isSome = Option.isSome;
val getOpt = Option.getOpt
\<close>

(* begin fxp imports *)
ML_file "fxp/src/Util/utilString.sml"
ML_file "fxp/src/Util/utilHash.sml"
ML_file "fxp/src/Util/utilCompare.sml"
ML_file "fxp/src/Unicode/Chars/uniChar.sml"
ML_file "fxp/src/Util/SymDict/key.sml"
ML_file "fxp/src/Util/utilError.sml"
ML_file "fxp/src/Util/utilInt.sml"
ML_file "fxp/src/Util/SymDict/dict.sml"
ML_file "fxp/src/Util/SymDict/stringDict.sml"
ML_file "fxp/src/Unicode/encoding.sml"
ML_file "fxp/src/Unicode/Encode/encodeBasic.sml"
ML_file "fxp/src/Unicode/Encode/encodeError.sml"
ML_file "fxp/src/Unicode/Encode/encodeMisc.sml"
ML_file "fxp/src/Unicode/Encode/encode.sml"
ML_file "fxp/src/Unicode/Chars/charClasses.sml"
ML_file "fxp/src/Unicode/Chars/uniRanges.sml"
ML_file "fxp/src/Unicode/Chars/uniClasses.sml"
ML_file "fxp/src/Unicode/Uri/uriDecode.sml"
ML_file "fxp/src/Unicode/Uri/uriEncode.sml"
ML_file "fxp/src/config.sml"
ML_file "fxp/src/Unicode/Uri/uri.sml"
ML_file "fxp/src/Unicode/Decode/decodeFile.sml"
ML_file "fxp/src/genRandom.sml"
ML_file "fxp/src/Util/utilTime.sml"
ML_file "fxp/src/Util/utilList.sml"
ML_file "fxp/src/Util/intLists.sml"
ML_file "fxp/src/Util/SymDict/symbolTable.sml"
ML_file "fxp/src/Util/SymDict/intListDict.sml"
ML_file "fxp/src/Util/SymDict/intDict.sml"
ML_file "fxp/src/Unicode/Chars/testClasses.sml"
ML_file "fxp/src/Unicode/Chars/charVecDict.sml"
ML_file "fxp/src/Parser/version.sml"
ML_file "fxp/src/Unicode/Decode/decodeError.sml"
ML_file "fxp/src/Unicode/Decode/decodeUtil.sml"
ML_file "fxp/src/Unicode/Decode/decodeUtf8.sml"
ML_file "fxp/src/Unicode/Decode/decodeUtf16.sml"
ML_file "fxp/src/Unicode/Decode/decodeUcs4.sml"
ML_file "fxp/src/Unicode/Decode/decodeUcs2.sml"
ML_file "fxp/src/Unicode/Decode/decodeMisc.sml"
ML_file "fxp/src/Unicode/Decode/decode.sml"
ML_file "fxp/src/Parser/Error/errorData.sml"
ML_file "fxp/src/Parser/Error/errorString.sml"
ML_file "fxp/src/Parser/Error/errorMessage.sml"
ML_file "fxp/src/Parser/Error/errorUtil.sml"
ML_file "fxp/src/Parser/Error/expected.sml"
ML_file "fxp/src/Parser/Error/errors.sml"
ML_file "fxp/src/Parser/Dfa/dfaData.sml"
ML_file "fxp/src/Parser/Base/baseData.sml"
ML_file "fxp/src/Parser/Dfa/dfaString.sml"
ML_file "fxp/src/Parser/Base/baseString.sml"
ML_file "fxp/src/Parser/Base/base.sml"
ML_file "fxp/src/Parser/Params/resolve.sml"
ML_file "fxp/src/Catalog/catError.sml"
ML_file "fxp/src/Catalog/catParams.sml"
ML_file "fxp/src/Unicode/Uri/uriDict.sml"
ML_file "fxp/src/Catalog/catData.sml"
ML_file "fxp/src/Util/options.sml"
ML_file "fxp/src/Parser/Dfa/dfaOptions.sml"
ML_file "fxp/src/Parser/Params/parserOptions.sml"
ML_file "fxp/src/Parser/Params/hookData.sml"
ML_file "fxp/src/Parser/Params/hooks.sml"
ML_file "fxp/src/Unicode/Chars/dataDict.sml"
ML_file "fxp/src/Parser/Params/dtd.sml"
ML_file "fxp/src/Parser/Dfa/dfaUtil.sml"
ML_file "fxp/src/Util/intSets.sml"
ML_file "fxp/src/Util/SymDict/intSetDict.sml"
ML_file "fxp/src/Parser/Dfa/dfaPassThree.sml"
ML_file "fxp/src/Parser/Dfa/dfaError.sml"
ML_file "fxp/src/Parser/Dfa/dfaPassOne.sml"
ML_file "fxp/src/Parser/Dfa/dfaPassTwo.sml"
ML_file "fxp/src/Parser/Dfa/dfa.sml"
ML_file "fxp/src/Parser/entities.sml"
ML_file "fxp/src/Parser/Dtd/dtdDeclare.sml"
ML_file "fxp/src/Parser/Dtd/dtdAttributes.sml"
ML_file "fxp/src/Parser/Dtd/dtdManager.sml"
ML_file "fxp/src/Parser/Parse/parseBase.sml"
ML_file "fxp/src/Parser/Parse/parseNames.sml"
ML_file "fxp/src/Parser/Parse/parseMisc.sml"
ML_file "fxp/src/Parser/Parse/parseXml.sml"
ML_file "fxp/src/Parser/Parse/parseRefs.sml"
ML_file "fxp/src/Parser/Parse/parseLiterals.sml"
ML_file "fxp/src/Parser/Parse/parseTags.sml"
ML_file "fxp/src/Parser/Parse/parseDecl.sml"
ML_file "fxp/src/Parser/Parse/parseDtd.sml"
ML_file "fxp/src/Parser/Parse/parseContent.sml"
ML_file "fxp/src/Parser/Parse/parseDocument.sml"
ML_file "fxp/src/Catalog/catFile.sml"
ML_file "fxp/src/Catalog/socatParse.sml"
ML_file "fxp/src/Catalog/catDtd.sml"
ML_file "fxp/src/Parser/Params/ignore.sml"
ML_file "fxp/src/Catalog/catHooks.sml"
ML_file "fxp/src/Catalog/catParse.sml"
ML_file "fxp/src/Catalog/catalog.sml"
ML_file "fxp/src/Catalog/catResolve.sml"
ML_file "fxp/src/Catalog/catOptions.sml"
(* end of fxp imports *)

(* 
  Todo: move configuration of the OS command for resolving URI 
        from config.sml to a proper Isabelle/Isar configuration attribute:
*)
ML\<open>
  val timeout = Attrib.setup_config_string \<^binding>\<open>uri_cmd\<close> (K "wget -qO %2 %1")
\<close>


end

