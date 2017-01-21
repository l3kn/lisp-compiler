(include "compile.scm")

(emit "  .intel_syntax noprefix")
(emit "  .text")
(emit-primitives)
