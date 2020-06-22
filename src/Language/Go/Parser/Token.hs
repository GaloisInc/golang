-- | Definition of the tokens produced by the lexer
module Language.Go.Parser.Token where

import Data.Text (Text)

data Token = Ident Text -- ^Identifier

           | Break -- ^break
           | Case  -- ^case
           | IChan  -- ^input chan
           | Chan
           | OChan 
           | Const -- ^const
           | Continue -- ^continue
           | Default  -- ^default
           | Defer  -- ^defer
           | Else   -- ^else
           | Fallthrough -- ^fallthrough
           | For -- ^for
           | Func -- ^func
           | Go   -- ^go
           | Goto -- ^goto
           | If   -- ^if
           | Import -- ^import
           | Interface -- ^interface
           | Map  -- ^map
           | Package -- ^package
           | Range   -- ^range
           | Return -- ^return
           | Select -- ^select
           | Struct -- ^struct
           | Switch -- ^switch
           | Type   -- ^type
           | Var    -- ^var
           
           | Plus   -- ^+
           | Minus  -- ^-
           | Star   -- ^*
           | Slash  -- ^/
           | Percent -- ^%
           | Amp -- ^&
           | Pipe -- ^|
           | Hat  -- ^^
           | LShift -- ^<<
           | RShift -- ^>>
           | AmpHat -- ^&^
           | PlusEq -- ^+=
           | MinusEq -- ^-=
           | StarEq -- ^*=
           | SlashEq -- ^/=
           | PercentEq -- ^%/
           | AmpEq -- ^&=
           | PipeEq -- ^|=
           | HatEq  -- ^^=
           | LShiftEq -- ^<<=
           | RShiftEq -- ^>>=
           | AmpHatEq -- ^&^=
           | DblAmp -- ^&&
           | DblPipe -- ^||
           | LeftArr -- ^<-
           | DblPlus -- ^++
           | DblMinus -- ^--
           | DblEq -- ^==
           | LAngle -- ^<
           | RAngle -- ^>
           | Equals -- ^=
           | Bang  -- ^!
           | BangEq -- ^!=
           | LAngleEq -- ^<=
           | RAngleEq -- ^>=
           | ColonEq  -- ^:=
           | Ellipsis -- ^...
           | LParen   -- ^(
           | RParen   -- ^)
           | LBracket -- ^[
           | RBracket -- ^]
           | LBrace   -- ^{
           | RBrace   -- ^}
           | Comma    -- ^,
           | Dot      -- ^.
           | Semicolon -- ^;
           | Colon     -- ^:

           | NilLit
           | BoolLit Bool -- ^integer (decimal, octal or hex) literal
           | IntLit Integer -- ^integer (decimal, octal or hex) literal
           | FloatLit Double -- ^floating-point literal
           | ImaginaryLit Double -- ^imaginary literal
           | RuneLit Char  -- ^rune literal
           | StringLit Text -- ^string literal
           
--           | Newline -- ^new-line (significant white space)
  deriving (Show)
