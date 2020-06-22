{
  
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, OverloadedStrings, BangPatterns #-}

module Language.Go.Parser.Lexer where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read
import Data.Char (ord, generalCategory, GeneralCategory(..),isAscii)
import Data.Maybe (fromJust)
import AlexTools
import Language.Go.Parser.Token
import Control.Applicative hiding (Const)
}

-- character classes as defined in Go spec
$newline = \n
$unicode_char = ~$newline
$whitespace = [\ \t\r]

-- The following six character macros are brittle: they need to
-- correspond to 'classifyChar'.

-- Unicode Ll (letter, lowercase)
$unicodeLl = \x01
-- Unicode Ll (letter, uppercase)
$unicodeLu = \x02
-- Unicode Lt (letter, titlecase)
$unicodeLt = \x03
-- Unicode Lm (letter, modifier)
$unicodeLm = \x04
-- Unicode Lo (letter, other)
$unicodeLo = \x05
-- Unicode Nd (number, decimal)
$unicodeNd = \x06

$ascii_letter = [a-zA-Z]

-- letters are Unicode 8.0 Lu, Ll, Lt, Lm and Lo
$unicode_letter = [$unicodeLu $unicodeLl $unicodeLt $unicodeLm $unicodeLo]
-- digits are Unicode 8.0 Nd
$unicode_digit = [$unicodeNd]

$letter = [$unicode_letter _ $ascii_letter]

$decimal_digit = 0-9
$octal_digit = 0-7
$hex_digit = [0-9a-fA-F]
$sign = [\+ \-]

@decimal_lit = (1-9) ($decimal_digit*)
@octal_lit = 0 ($octal_digit*)
@hex_lit = 0 (x | X) ($hex_digit+)

@decimals = $decimal_digit+
@exponent = ("e" | "E") $sign? @decimals
@tame_float_lit = (@decimals "." ($decimal_digit*) @exponent?) | @decimals exponent
@weird_float_lit = "." @decimals @exponent?


@tame_imaginary_lit = (@decimals | @tame_float_lit) "i"
@weird_imaginary_lit = @weird_float_lit "i"

@octal_byte_value = "\\" $octal_digit $octal_digit $octal_digit
@hex_byte_value = "\\x" $hex_digit $hex_digit
@little_u_value = "\\u" $hex_digit $hex_digit $hex_digit $hex_digit
@big_u_value = "\\U" $hex_digit $hex_digit $hex_digit $hex_digit $hex_digit $hex_digit $hex_digit $hex_digit

@identifier = $letter ([$letter $unicode_digit $decimal_digit])*

tokens :-

  -- white space
  <0> $newline {autosemicolon}
  <0> $whitespace;

  -- comments
  <0> "//" (~$newline)*;
  <0> "/*" {commentMode}
  <multiline_comment> .;
  <multiline_comment> "*/" {normalMode}

  -- keywords
  <0> "break" {mylexeme Break}
  <0> "case" {mylexeme Case}
  <0> "<-" $white* "chan" {mylexeme IChan}
  <0> chan {mylexeme Chan}
  <0> chan $white* "<-" {mylexeme OChan}
  <0> "const" {mylexeme Const}
  <0> "continue" {mylexeme Continue}
  <0> "default" {mylexeme Default}
  <0> "defer" {mylexeme Defer}
  <0> "else" {mylexeme Else}
  <0> "fallthrough" {mylexeme Fallthrough}
  <0> "for" {mylexeme For}
  <0> "func" {mylexeme Func}
  <0> "go" {mylexeme Go}
  <0> "goto" {mylexeme Goto}
  <0> "if" {mylexeme If}
  <0> "import" {mylexeme Import}
  <0> "interface" {mylexeme Interface}
  <0> "map" {mylexeme Map}
  <0> "package" {mylexeme Package}
  <0> "range" {mylexeme Range}
  <0> "return" {mylexeme Return}
  <0> "select" {mylexeme Select}
  <0> "struct" {mylexeme Struct}
  <0> "switch" {mylexeme Switch}
  <0> "type" {mylexeme Type}
  <0> "var" {mylexeme Var}

  -- operators
  <0> "+" {mylexeme Plus}
  <0> "-" {mylexeme Minus}
  <0> "*" {mylexeme Star}
  <0> "/" {mylexeme Slash}
  <0> "%" {mylexeme Percent}
  <0> "&" {mylexeme Amp}
  <0> "|" {mylexeme Pipe}
  <0> "^" {mylexeme Hat}
  <0> "<<" {mylexeme LShift}
  <0> ">>" {mylexeme RShift}
  <0> "&^" {mylexeme AmpHat}
  <0> "+=" {mylexeme PlusEq}
  <0> "-=" {mylexeme MinusEq}
  <0> "*=" {mylexeme StarEq}
  <0> "/=" {mylexeme SlashEq}
  <0> "%=" {mylexeme PercentEq}
  <0> "&=" {mylexeme AmpEq}
  <0> "|=" {mylexeme PipeEq}
  <0> "^=" {mylexeme HatEq}
  <0> "<<=" {mylexeme LShiftEq}
  <0> ">>=" {mylexeme RShiftEq}
  <0> "&^=" {mylexeme AmpHatEq}
  <0> "&&" {mylexeme DblAmp}
  <0> "||" {mylexeme DblPipe}
  <0> "<-" {mylexeme LeftArr}
  <0> "++" {mylexeme DblPlus}
  <0> "--" {mylexeme DblMinus}
  <0> "==" {mylexeme DblEq}
  <0> "<" {mylexeme LAngle}
  <0> ">" {mylexeme RAngle}
  <0> "=" {mylexeme Equals}
  <0> "!" {mylexeme Bang}
  <0> "!=" {mylexeme BangEq}
  <0> "<=" {mylexeme LAngleEq}
  <0> ">=" {mylexeme RAngleEq}
  <0> ":=" {mylexeme ColonEq}
  <0> "..." {mylexeme Ellipsis}
  <0> "(" {mylexeme LParen}
  <0> ")" {mylexeme RParen}
  <0> "[" {mylexeme LBracket}
  <0> "]" {mylexeme RBracket}
  <0> "{" {mylexeme LBrace}
  <0> "}" {mylexeme RBrace}
  <0> "," {mylexeme Comma}
  <0> "." {mylexeme Dot}
  <0> ";" {mylexeme Semicolon}
  <0> ":" {mylexeme Colon}

  -- nil literal
  <0> "nil" {mylexeme NilLit}

  -- bool literals
  <0> "true" {mylexeme (BoolLit True)}
  <0> "false" {mylexeme (BoolLit False)}

  -- integer literals
  <0> @decimal_lit {readText decimal >>= (mylexeme . IntLit)}
  <0> @hex_lit {readText hexadecimal >>= (mylexeme . IntLit)}
  <0> @octal_lit {readText octal >>= (mylexeme . IntLit)}

  --floating point literals
  <0> @tame_float_lit {readText rational >>= (mylexeme . FloatLit)}
  <0> @weird_float_lit {readText (rational . Text.cons '0') >>= (mylexeme . FloatLit)}

  -- imaginary literals
  <0> @tame_imaginary_lit {readText (rational . Text.init) >>= (mylexeme . ImaginaryLit)}
  <0> @weird_imaginary_lit {readText (rational . Text.cons '0' . Text.init) >>= (mylexeme . ImaginaryLit)}

  -- rune literals
  <0> ' {runeMode}
  <rune> @octal_byte_value ' {normalMode >>
         readText (oct . Text.init . Text.drop 1) >>=
         (\c -> mylexeme (RuneLit $ toEnum c))}
  <rune> (@hex_byte_value | @little_u_value | @big_u_value)  ' {
          normalMode >>
          readText (hexadecimal . Text.init . Text.drop 2) >>=
          (\c -> mylexeme (RuneLit $ toEnum c))}
  <rune> \\a' {normalMode >> mylexeme (RuneLit '\x07')}
  <rune> \\b' {normalMode >> mylexeme (RuneLit '\x08')}
  <rune> \\f' {normalMode >> mylexeme (RuneLit '\x0C')}
  <rune> \\n' {normalMode >> mylexeme (RuneLit '\x0A')}
  <rune> \\r' {normalMode >> mylexeme (RuneLit '\x0D')}
  <rune> \\t' {normalMode >> mylexeme (RuneLit '\x09')}
  <rune> \\v' {normalMode >> mylexeme (RuneLit '\x0b')}
  <rune> \\\\' {normalMode >> mylexeme (RuneLit '\x0c')}
  <rune> \\'' {normalMode >> mylexeme (RuneLit '\x27')}
  <rune> [^'\\] ' {normalMode >> matchText >>= mylexeme . RuneLit . fst . fromJust . Text.uncons}

  -- strings
  <0> ` {rawStringMode}
  <raw_string> (~`)* "`" {matchText >>= (\s -> normalMode >> mylexeme (StringLit $ Text.init s))}
  <0> \" {interpretedStringMode >> return []}
  <interpreted_string> @octal_byte_value {
         readText (oct . Text.drop 1) >>= (pushChar . toEnum)  >> return []}
  <interpreted_string> (@hex_byte_value | @little_u_value | @big_u_value) {
          readText (hexadecimal . Text.init . Text.drop 2) >>= (pushChar . toEnum) >> return []}
  <interpreted_string> \\a {pushChar '\x07' >> return []}
  <interpreted_string> \\b {pushChar '\x08' >> return []}
  <interpreted_string> \\f {pushChar '\x0C' >> return []}
  <interpreted_string> \\n {pushChar '\x0A' >> return []}
  <interpreted_string> \\r {pushChar '\x0D' >> return []}
  <interpreted_string> \\t {pushChar '\x09' >> return []}
  <interpreted_string> \\v {pushChar '\x0b' >> return []}
  <interpreted_string> \\\\ {pushChar '\x0c' >> return []}
  <interpreted_string> \\"' {pushChar '\x22' >> return []}
  <interpreted_string> [^\n\"]  {matchText >>= (\t -> pushChar (fst $ fromJust $ Text.uncons t) >> return [])}
  <interpreted_string> \" {collectedString >>= (\s -> normalMode >> mylexeme (StringLit s))}

  -- identifiers
  <0> @identifier {matchText >>= (mylexeme . Ident)}

{

-- ASI rule for newlines
autosemicolon :: Action LexerState [Lexeme Token]
autosemicolon =
   do mlt <- getLastToken
      maybelexeme $ do lt <- mlt
                       case lt of
                         Ident _ -> Just Semicolon
                         IntLit _ -> Just Semicolon
                         FloatLit _ -> Just Semicolon
                         ImaginaryLit _ -> Just Semicolon
                         RuneLit _ -> Just Semicolon
                         StringLit _ -> Just Semicolon
                         Break -> Just Semicolon
                         Continue -> Just Semicolon
                         Fallthrough -> Just Semicolon
                         DblPlus -> Just Semicolon
                         DblMinus -> Just Semicolon
                         RParen -> Just Semicolon
                         RBracket -> Just Semicolon
                         RBrace -> Just Semicolon
                         _      -> Nothing

mylexeme :: Token -> Action LexerState [Lexeme Token]
mylexeme t = setLastToken t >> lexeme t

maybelexeme :: Maybe Token -> Action LexerState [Lexeme Token]
maybelexeme Nothing  = return []
maybelexeme (Just t) = mylexeme t

getLastToken :: Action LexerState (Maybe Token)
getLastToken = lastToken <$> getLexerState

setLastToken :: Token -> Action LexerState ()
setLastToken t = do st <- getLexerState
                    setLexerState st {lastToken = Just t}

classifyChar :: Char -> Word8
classifyChar c | c <= '\x08' = non_graphic
               | isAscii c = fromIntegral (ord c)
               | otherwise = case generalCategory c of
                               LowercaseLetter -> unicodeLl
                               UppercaseLetter -> unicodeLu
                               TitlecaseLetter -> unicodeLt
                               ModifierLetter  -> unicodeLm
                               OtherLetter     -> unicodeLo
                               DecimalNumber   -> unicodeNd
                               _               -> other
   where non_graphic = 0
         unicodeLl = 1
         unicodeLu = 2
         unicodeLt = 3
         unicodeLm = 4
         unicodeLo = 5
         unicodeNd = 6
         other     = 7

data LexerState = LexerState {lastToken :: Maybe Token
                             ,lexingMode :: LexerMode }

data LexerMode = NormalMode
                | RuneMode
                | MultilineCommentMode
                | RawStringMode
                | InterpretedStringMode !String
                  {- Reversed string contents for fast snoc'ing -}

instance Enum LexerMode where
  toEnum = undefined -- \case 0 -> NormalMode
                 -- rune -> RuneMode
                 -- multiline_comment -> MultilineCommentMode
                 -- raw_string -> RawStringMode
                 -- interpreted_string -> InterpretedStringMode ""

  fromEnum = \case NormalMode -> 0
                   RuneMode -> rune
                   MultilineCommentMode -> multiline_comment
                   RawStringMode -> raw_string
                   InterpretedStringMode _ -> interpreted_string

ensureNormalMode = do s <- getLexerState
                      case lexingMode s of
                         NormalMode -> return []
                         _          -> error "Normal lexing mode expected"

normalMode = changeMode NormalMode

runeMode = ensureNormalMode >> changeMode RuneMode

commentMode = ensureNormalMode >> changeMode MultilineCommentMode

rawStringMode = ensureNormalMode >> changeMode RawStringMode

interpretedStringMode = ensureNormalMode >> changeMode (InterpretedStringMode "")

-- Snoc a character to an interpreted string
pushChar :: Char -> Action LexerState ()
pushChar c = do st <- getLexerState
                case lexingMode st of
                   InterpretedStringMode s ->
                     setLexerState (st {lexingMode = InterpretedStringMode $ c:s})
                   _  -> error "Lexer error: attempted to push a char while not lexing an interpreted string"

-- | Take the interpreted string we've collected, reverse it and pack
-- as 'Text'. While this appears expensive, it is ammortized O(1)
-- complexity (hint: assume pushChar debits 1 and reverse/pack credits n).
collectedString :: Action LexerState Text
collectedString = do st <- getLexerState
                     case lexingMode st of
                       InterpretedStringMode s ->
                           return $ Text.pack $ reverse s
                       _ -> error "Lexer error: attempted to consume an interpreted string in an incorrect mode"
                        

changeMode m = do s <- getLexerState
                  setLexerState (s {lexingMode = m})
                  return []
                                           
readText :: Reader a -> Action s a
readText r = matchText >>= \t -> case r t of
  Left err -> error $ "Can't read a literal: " ++ err
  Right (x, rest) -> if Text.null rest then return x
                     else error $ "Nonempty residual after read: " ++ Text.unpack rest

{-# SPECIALIZE octal :: Reader Integer #-}
octal :: Integral a => Reader a
octal txt = case Text.uncons txt of
  Just (prefix, rest) -> if prefix == '0' then oct rest
                         else Left "Invalid octal integer prefix"
  Nothing -> Left "Attempted to read an empty text"

{-# SPECIALIZE octal :: Reader Integer #-}
oct :: Integral a => Reader a
oct txt = Right $ if Text.null txt then (0, txt)
                  else let readme :: (Integral a) => a -> Text -> (a, Text)
                           readme n t =
                             case Text.uncons t of
                               Just (d, rest) ->
                                 case octDigitToInt d of
                                   Just i -> (readme $! (n * 8 + fromIntegral i)) rest
                                   Nothing -> (n, rest)
                               Nothing -> (n, t)
                       in readme 0 txt

octDigitToInt :: Char -> Maybe Int
octDigitToInt c | c >= '0' && c <= '7' = Just $ ord c - ord '0'
                | otherwise = Nothing

alexGetByte = makeAlexGetByte classifyChar


lexer = $makeLexer LexerConfig
  {lexerInitialState = LexerState Nothing NormalMode
  ,lexerStateMode = fromEnum . lexingMode
  ,lexerEOF = (\_ _ -> [])
  }

}
