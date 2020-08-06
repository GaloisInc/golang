Golang
======

This is a parser frontend for the Go programming language designed to be used with [goblin](https://github.com/galoisinc/goblin).

The abstract syntax and type definitions are in [AST.hs](https://github.com/GaloisInc/golang/blob/master/src/Language/Go/AST.hs) and [Types.hs](https://github.com/GaloisInc/golang/blob/master/src/Language/Go/Types.hs), respectively. [Rec.hs](https://github.com/GaloisInc/golang/blob/master/src/Language/Go/Rec.hs) contains generic combinators for folding over the syntax.

The frontend proceeds in three phases:
1) The JSON parser ([Parser.hs](https://github.com/GaloisInc/golang/blob/master/src/Language/Go/Parser.hs)) deserializes a JSON-encoded AST produced by goblin.
2) The desugaring pass ([Desugar.hs](https://github.com/GaloisInc/golang/blob/master/src/Language/Go/Desugar.hs)) performs various (purely syntactic) transformations.
3) The renamer ([Renamer.hs](https://github.com/GaloisInc/golang/blob/master/src/Language/Go/Renamer.hs)) fills in missing qualifiers so that all global identifiers become fully qualified.
