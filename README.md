# Isabelle Hacks

This project contains small Isabelle "hacks" that provide additional 
functionality to [Isabelle](https://isabelle.in.tum.de) or showcase
specific functionality. The individual hacks usually consist out of 
a single theory file and all documentation is contained in that 
theory file. The master branch should work with the latest official 
release of Isabelle (Isabelle 2017, at time of writing), hacks for 
older versions might be available on a dedicated branch.

## List of Isabelle Hacks

* [assert.thy](assert.thy) provides a new top level command **assert**
  that provides a simple way for specifying assertions that Isabelle
  checks while processing a theory.

* [hiding_type_variables.thy](hiding_type_variables.thy) provides
  print a setup for defining default type variables of type
  constructors. The default type variables can be hidden in output,
  e.g., `('a, 'b, 'c) foo` is shown as `_ foo`. This shorthand
  notation can also be used in input (using a parse translation),
  which (sometimes) helps to focus on the important parts of complex
  type declarations.

## Authors

Main author: [Achim D. Brucker](http://www.brucker.ch/)

## License
If not otherwise stated, all hacks are licensed under a 2-clause 
BSD-style license.

SPDX-License-Identifier: BSD-2-Clause

## Master Repository

The master git repository for this project is hosted by the [Software
Assurance & Security Research Team](https://logicalhacking.com):
[https://git.logicalhacking.com/adbrucker/isabelle-hacks]
