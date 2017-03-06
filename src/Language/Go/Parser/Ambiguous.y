{
{-# LANGUAGE LambdaCase  #-}
module Language.Go.Parser.Ambiguous where

import Language.Go.AST
import Language.Go.Parser.Token (Token)
import Language.Go.Parser.Util
import Data.List.NonEmpty (NonEmpty (..),(<|))
import qualified Data.List.NonEmpty as NE
import qualified Language.Go.Parser.Token as T
import Language.Go.Parser.Lexer
import AlexTools (SourcePos (..), SourceRange(..), Lexeme (..))
import qualified AlexTools (range)
import Data.Maybe (fromMaybe, maybeToList, isJust, catMaybes)
import Lens.Simple
import Control.Applicative
import Control.Monad (liftM, unless, foldM)
import Data.Traversable (sequence)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
}

%tokentype { Lexeme Token }
%token 'break' { Lexeme {lexemeToken = T.Break} }
       'case' { Lexeme {lexemeToken = T.Case} }
       '<-chan' { Lexeme {lexemeToken = T.IChan}}
       'chan<-' { Lexeme {lexemeToken = T.OChan}}
       'chan' {Lexeme {lexemeToken = T.Chan}}
       'const' {Lexeme {lexemeToken = T.Const}}
       'continue' {Lexeme {lexemeToken = T.Continue}}
       'default' {Lexeme {lexemeToken = T.Default}}
       'defer' {Lexeme {lexemeToken = T.Defer}}
       'else' {Lexeme {lexemeToken = T.Else}}
       'fallthrough' {Lexeme {lexemeToken = T.Fallthrough}}
       'for' {Lexeme {lexemeToken = T.For}}
       'func' {Lexeme {lexemeToken = T.Func}}
       'go' {Lexeme {lexemeToken = T.Go}}
       'goto' {Lexeme {lexemeToken = T.Goto}}
       'if' {Lexeme {lexemeToken = T.If}}
       'import' {Lexeme {lexemeToken = T.Import}}
       'interface' {Lexeme {lexemeToken = T.Interface}}
       'map' {Lexeme {lexemeToken = T.Map}}
       'package' {Lexeme {lexemeToken = T.Package}}
       'range' {Lexeme {lexemeToken = T.Range}}
       'return' {Lexeme {lexemeToken = T.Return}}
       'select' {Lexeme {lexemeToken = T.Select}}
       'struct' {Lexeme {lexemeToken = T.Struct}}
       'switch' {Lexeme {lexemeToken = T.Switch}}
       'type' {Lexeme {lexemeToken = T.Type}}
       'var' {Lexeme {lexemeToken = T.Var}}
  
       '+' {Lexeme {lexemeToken = T.Plus}}
       '-' {Lexeme {lexemeToken = T.Minus}}
       '*' {Lexeme {lexemeToken = T.Star}}
       '/' {Lexeme {lexemeToken = T.Slash}}
       '%' {Lexeme {lexemeToken = T.Percent}}
       '&' {Lexeme {lexemeToken = T.Amp}}
       '|' {Lexeme {lexemeToken = T.Pipe}}
       '^' {Lexeme {lexemeToken = T.Hat}}
       '<<' {Lexeme {lexemeToken = T.LShift}}
       '>>' {Lexeme {lexemeToken = T.RShift}}
       '&^' {Lexeme {lexemeToken = T.AmpHat}}
       '+=' {Lexeme {lexemeToken = T.PlusEq}}
       '-=' {Lexeme {lexemeToken = T.MinusEq}}
       '*=' {Lexeme {lexemeToken = T.StarEq}}
       '/=' {Lexeme {lexemeToken = T.SlashEq}}
       '%=' {Lexeme {lexemeToken = T.PercentEq}}
       '&=' {Lexeme {lexemeToken = T.AmpEq}}
       '|=' {Lexeme {lexemeToken = T.PipeEq}}
       '^=' {Lexeme {lexemeToken = T.HatEq}}
       '<<=' {Lexeme {lexemeToken = T.LShiftEq}}
       '>>=' {Lexeme {lexemeToken = T.RShiftEq}}
       '&^=' {Lexeme {lexemeToken = T.AmpHatEq}}
       '&&' {Lexeme {lexemeToken = T.DblAmp}}
       '||' {Lexeme {lexemeToken = T.DblPipe}}
       '<-' {Lexeme {lexemeToken = T.LeftArr}}
       '++' {Lexeme {lexemeToken = T.DblPlus}}
       'dbl-' {Lexeme {lexemeToken = T.DblMinus}}
       '==' {Lexeme {lexemeToken = T.DblEq}}
       '<' {Lexeme {lexemeToken = T.LAngle}}
       '>' {Lexeme {lexemeToken = T.RAngle}}
       '=' {Lexeme {lexemeToken = T.Equals}}
       '!' {Lexeme {lexemeToken = T.Bang}}
       '!=' {Lexeme {lexemeToken = T.BangEq}}
       '<=' {Lexeme {lexemeToken = T.LAngleEq}}
       '>=' {Lexeme {lexemeToken = T.RAngleEq}}
       ':=' {Lexeme {lexemeToken = T.ColonEq}}
       '...' {Lexeme {lexemeToken = T.Ellipsis}}
       '(' {Lexeme {lexemeToken = T.LParen}}
       ')' {Lexeme {lexemeToken = T.RParen}}
       '[' {Lexeme {lexemeToken = T.LBracket}}
       ']' {Lexeme {lexemeToken = T.RBracket}}
       '{' {Lexeme {lexemeToken = T.LBrace}}
       '}' {Lexeme {lexemeToken = T.RBrace}}
       ',' {Lexeme {lexemeToken = T.Comma}}
       '.' {Lexeme {lexemeToken = T.Dot}}
       ';' {Lexeme {lexemeToken = T.Semicolon}}
       ':' {Lexeme {lexemeToken = T.Colon}}

       int {Lexeme {lexemeToken = T.IntLit _}}
       float {Lexeme {lexemeToken = T.FloatLit _}}
       imaginary {Lexeme {lexemeToken = T.ImaginaryLit _}}
       rune {Lexeme {lexemeToken = T.RuneLit _}}
       string {Lexeme {lexemeToken = T.StringLit _}}

       ident {Lexeme {lexemeToken = T.Ident _}}

%monad { Either (SourceRange, String) }
%error { errorP }

%nonassoc ';'
%nonassoc '<-' '++' 'dbl-' '=' ':='
%nonassoc '('
%nonassoc ','
%left ':'
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-' '|' '^'
%left '*' '/' '%' '<<' '>>' '&' '&^'
%left PLUS MINUS NOT COMPLEMENT DEREF ADDRESS RECV
%left '.'

%name file file

%%

closeparen :: {()}
  : ')' {}
  | ';' ')' {}

closebrace :: {()}
  : '}' {}
  | ';' '}' {}

importdecl :: {ImportDecl SourceRange}
  : 'import' importspecs {pos ($1, $2) ImportDecl $2}

importdecls :: {[ImportDecl SourceRange]}
  : revimportdecls {reverse $1}

revimportdecls :: {[ImportDecl SourceRange]}
  : revimportdecls importdecl ';' {$2 : $1}
  |                                    {[]}

importspecs :: {[ImportSpec SourceRange]}
  : importspec {[$1]}
  | '(' ')' {[]}
  | '(' revimportspecs1 closeparen {reverse $2}

revimportspecs1 :: {[ImportSpec SourceRange]}
  : revimportspecs1 ';' importspec {$3 : $1}
  | importspec {[$1]}

optident :: {Maybe (Text, SourceRange)}
  :       {Nothing}
  | ident {Just (getIdent $1, lexemeRange $1)}
  
importspec :: {ImportSpec SourceRange}
  : '.' string {pos ($1,$2) Import ImportAll (getString $2)}
  | optident string {pos (fmap snd $1, $2) Import (ImportQualified (fmap fst $1)) (getString $2)}

file :: {File SourceRange}
  : 'package' ident ';' importdecls toplevels {
       pos ($1, $5) File T.empty (getIdent $2) $4 $5
    }

toplevels :: {[TopLevel SourceRange]}
  : revtoplevels {reverse $1}

revtoplevels :: {[TopLevel SourceRange]}
  : {[]}
  | revtoplevels toplevel ';' {reverse ($2: $1)}
               
toplevel :: {TopLevel a}
  : declaration {pos $1 TopDecl $1}
  | 'func' identifier parameters optreturns optblock {
      pos ($1, ($4,$5)) FunctionDecl     $2 $3 $4 $5
    }  
  | 'func' receiver identifier parameters optreturns optblock {
      pos ($1, ($5,$6)) MethodDecl $2 $3 $4 $5 $6
    }

optreturns :: {ReturnList SourceRange}
  : {AnonymousReturnList fakeRange []}
  | returns {$1}

receiver :: {Receiver SourceRange}
  : '(' optidentifier optpointer typeName ')' {pos ($1,$5) Receiver $2 $3 $4}

optpointer :: {Bool}
  : '*' {True}
  |     {False}

identifierList :: {NonEmpty (Id SourceRange)}
  : revIdentifierList {NE.reverse $1}

revIdentifierList :: {NonEmpty (Id SourceRange)}
  : identifier {$1 :| []}
  | revIdentifierList  ',' identifier {$3 <| $1}

-- | can be empty
exprList :: {[Expression SourceRange]}
  :                     {[]}
  | revNonEmptyExprList {reverse (NE.toList $1)}

nonEmptyExprList :: {NonEmpty (Expression SourceRange)}
  : revNonEmptyExprList {NE.reverse $1}
  
revNonEmptyExprList :: {NonEmpty (Expression SourceRange)}
  : expr {$1 :| []}
  | revNonEmptyExprList ',' expr {$3 <| $1}

identifier :: {Id SourceRange}
  : ident {if getIdent $1 == T.singleton '_' then pos $1 BlankId
           else pos $1 Id undefined (getIdent $1)
          }

optidentifier :: {Maybe (Id SourceRange)}
  : identifier {Just $1}
  |            {Nothing}

label :: {Label SourceRange}
  : ident {pos $1 Label (getIdent $1)}

optlabel :: {Maybe (Label SourceRange)}
  : label {Just $1}
  |       {Nothing}

type_ :: {Type SourceRange}
  : namedType {$1}
  | typeFirstArg {$1}

opttype :: {Maybe (Type SourceRange)}
  : type_ {Just $1}
  |       {Nothing}

typeNoName :: {Type SourceRange}
  : typeLit {$1}
  | '(' typeNoName ')' {$2}

optTypeFirstArg :: {Maybe (Type SourceRange)}
  : typeFirstArg {Just $1}
  |              {Nothing}

typeFirstArg :: {Type SourceRange}
  : '*' namedType {pos ($1, $2) PointerType $2}
  | '(' namedType ')' {$2}
  | typeNoName {$1}


namedType :: {Type SourceRange}
  : typeName {pos $1 NamedType $1}

typeName :: {TypeName SourceRange}
  : identifier {pos $1 TypeName Nothing $1}
  | identifier '.' identifier {pos ($1, $3) TypeName (Just $1) $3}

typeLit :: {Type SourceRange}
  : arrayType {$1}
  | structType {$1}
  | '*' typeNoName {pos ($1, $2) PointerType $2}
  | 'func' parameters optreturns {pos ($1, $3) FunctionType $2 $3}
  | sliceType {$1}
  | mapType {$1}
  | 'interface' methodspecs {
      pos ($1, $2) InterfaceType $2
    }
  | channeldir type_ {pos ($1,$2) ChannelType $1 $2}

structType :: {Type SourceRange}
  : 'struct' fieldDecls {pos ($1,$2) StructType $2}

fieldDecls :: {[FieldDecl SourceRange]}
  : '{' '}' {[]}
  | '{' revFieldDecls1 closebrace {reverse $2}

revFieldDecls1 :: {[FieldDecl SourceRange]}
  : revFieldDecls1 ';' fieldDecl {$3 : $1}
  | fieldDecl {[$1]}

fieldDecl :: {FieldDecl SourceRange}
  : fieldDeclPart {$1 Nothing}
  | fieldDeclPart tag {let x = $1 (Just $2) in repos (x, $2) x }

fieldDeclPart :: {Maybe (Tag SourceRange) -> FieldDecl SourceRange}
  : '*' typeName  {\mtag -> pos ($1, $2) AnonymousFieldDecl $2 mtag}
  | typeName {\mtag -> pos $1 AnonymousFieldDecl $1 mtag}
  | identifierList type_ {\mtag -> pos ($1, $2) NamedFieldDecl $1 $2 mtag}

arrayType :: {Type SourceRange}
  : '[' expr ']' type_ {pos ($1, $4) ArrayType (Just $2) $4}

sliceType :: {Type SourceRange}
  : '[' ']' type_ {pos ($1, $3) SliceType $3}

mapType :: {Type SourceRange}
  : 'map' '[' type_ ']' type_ {pos ($1, $5) MapType $3 $5}


tag :: {Tag SourceRange}
  : string {pos $1 Tag (getString $1)}

parameters :: {ParameterList SourceRange}
  : '(' revParameterList optcomma ')' {% processReverseParameterList $2}

optcomma :: {Bool}
  : ',' {True}
  |     {False}

revParameterList :: {[Param SourceRange]}
  : revParameterList1 {$1}
  |                   {[]}

revParameterList1 :: {[Param SourceRange]}
  : revParameterList1 ',' param {$3 : $1}
  | param                       {[$1]}

param :: {Param SourceRange}
  : identifier {pos $1 ParamName $1}
  | identifier type_ {pos ($1, $2) TypedParam $1 $2}
  | typeFirstArg {pos $1 TypeNoName $1}
  | '...' type_ {pos ($1, $2) SpreadParam $2}
  | identifier '...' type_ {pos ($1, $3) SpreadNameParam $1 $3}

returns :: {ReturnList SourceRange}
  : parameters {% parametersToReturns $1 }
  | typeNoParens {pos $1 AnonymousReturnList [$1]}

typeNoParens :: {Type SourceRange}
  : namedType {$1}
  | '*' namedType {pos ($1, $2) PointerType $2}
  | typeNoName {$1}


channeldir :: {ChannelDirection SourceRange}
  : 'chan' {pos $1 Duplex}
  | '<-chan' {pos $1 Recv}
  | 'chan<-' {pos $1 Send}

methodspec :: {MethodSpec SourceRange}
  : identifier msrest {
       case $2 of {
          Left (params, returns) -> pos ($1, returns) MethodSpec $1 params returns ;
          Right mid -> pos ($1, mid) (\p -> InterfaceSpec p (TypeName p (fmap (const $1) mid) (fromMaybe $1 mid)))
       }
    }

msrest :: {Either (ParameterList SourceRange, ReturnList SourceRange) (Maybe (Id SourceRange))}
  : '.' identifier {Right (Just $2)}
  | parameters returns {Left ($1, $2)}
  | {Right Nothing}

methodspecs :: {[MethodSpec SourceRange]}
  : '{' '}' {[]}
  | '{' revmethodspecs1 closebrace {reverse $2}

revmethodspecs1 :: {[MethodSpec SourceRange]}
  : revmethodspecs1 ';' methodspec {$3 : $1}
  | methodspec {[$1]}

expr :: {Expression SourceRange}
  : expr binop expr {pos ($1, $3) BinaryExpr $2 $1 $3 }
  | '+' expr %prec PLUS {pos ($1, $2) UnaryExpr Plus $2}
  | '-' expr %prec MINUS {pos ($1, $2) UnaryExpr Minus $2}
  | '!' expr %prec NOT {pos ($1, $2) UnaryExpr Not $2}
  | '^' expr %prec COMPLEMENT {pos ($1, $2) UnaryExpr Complement $2}
  | '&' expr %prec ADDRESS {pos ($1, $2) UnaryExpr Address $2}
  | '<-' expr %prec RECV {pos ($1, $2) UnaryExpr Receive $2}
  | '*' expr %prec DEREF {pos ($1, $2) UnaryExpr Deref $2}
  | primaryExpr {$1}


optexpr :: {Maybe (Expression SourceRange)}
  : expr {Just $1}
  |      {Nothing}

primaryExpr :: {Expression SourceRange}
  : operand {$1}
  | typeNoName '(' expr optcomma ')' {pos ($1, $5) Conversion $1 $3}
  | primaryExpr '.' identifier {pos ($1, $3) FieldSelector $1 $3}
  | primaryExpr '[' expr ']' {pos ($1, $4) IndexExpr $1 $3}
  | primaryExpr '[' optexpr ':' optexpr ']' {pos ($1,$6) SliceExpr $1 $3 $5 Nothing}
  | primaryExpr '[' optexpr ':' expr ':' expr ']' {pos ($1,$8) SliceExpr $1 $3 (Just $5) (Just $7)}
  | primaryExpr '.' '(' type_ ')' {pos ($1, $5) TypeAssertion $1 $4}
  | primaryExpr arguments {let (mt, args, mspread) = $2 in pos ($1, ((mt,args), mspread)) CallExpr $1 mt args mspread}
  | primaryExpr literalValue {%
      case $1 of {
        FieldSelector _ (Name _ Nothing id1) id2 -> return ($2 (pos $1 NamedType (pos $1 TypeName (Just id1) id2)));
        (Name _ mqual tyn) -> return ($2 (pos $1 NamedType (pos $1 TypeName mqual tyn)));
        _ -> unexpected $1 "Syntax error when parsing the type of a composite literal";
      }
    }

arguments :: {(Maybe (Type SourceRange), [Expression SourceRange], Maybe (Expression SourceRange))}
  : '(' optTypeFirstArg optcomma optArgs optellipsis optcomma ')' {%
      let {mtype = $2; c2a = $3; margs = $4; bspread = $5} in
      case (mtype, c2a, margs, bspread) of {
        (Nothing, False, Nothing, False) ->
           return (Nothing, [], Nothing);
        (Nothing, False, Just es, False) ->
           return (Nothing, NE.toList es, Nothing);
        (Nothing, False, Just es, True) ->
           return (Nothing, NE.init es, Just (NE.last es));
        (Just t, False, Nothing, False) ->
           return (Just t, [], Nothing);
        (Just t, True, Just es, False) ->
           return (Just t, NE.toList es, Nothing);
        (Just t, True, Just es, True) ->
           return (Just t, NE.init es, Just (NE.last es));
        (_, _, Nothing, True) -> unexpected ($1, $7) "Spread parameter in a function call when no formal arguments are provided"
      }
    }

operand :: {Expression SourceRange}
  : int {pos $1 IntLit (getInt $1)}
  | float {pos $1 FloatLit (getFloat $1)}
  | imaginary {pos $1 ImaginaryLit (getImaginary $1)}
  | rune {pos $1 RuneLit (getRune $1)}
  | string {pos $1 StringLit (getString $1)}
  | 'func' parameters optreturns block {pos ($1, $4) FunctionLit $2 $3 $4}
  | literalTypeNoName literalValue {$2 $1}
  | identifier {pos $1 Name Nothing $1}
--  | identifier '.' identifier {pos ($1, $3) Name (Just $1) $3}
  | '(' expr ')' {$2}

-- Special form of expression that excludes a composite literal form
-- "<name> {...}" without parentheses. For use in guard conditions of
-- 'if', 'for' and 'switch' statements.
guardExpr :: {Expression SourceRange}
  : guardExpr binop guardExpr {pos ($1, $3) BinaryExpr $2 $1 $3 }
  | '+' guardExpr %prec PLUS {pos ($1, $2) UnaryExpr Plus $2}
  | '-' guardExpr %prec MINUS {pos ($1, $2) UnaryExpr Minus $2}
  | '!' guardExpr %prec NOT {pos ($1, $2) UnaryExpr Not $2}
  | '^' guardExpr %prec COMPLEMENT {pos ($1, $2) UnaryExpr Complement $2}
  | '&' guardExpr %prec ADDRESS {pos ($1, $2) UnaryExpr Address $2}
  | '<-' guardExpr %prec RECV {pos ($1, $2) UnaryExpr Receive $2}
  | '*' guardExpr %prec DEREF {pos ($1, $2) UnaryExpr Deref $2}
  | primaryGuardExpr {$1}

optGuardExpr :: {Maybe (Expression SourceRange)}
  : guardExpr {Just $1}
  |           {Nothing}

guardExprList :: {NonEmpty (Expression SourceRange)}
  : revGuardExprList {NE.reverse $1}

revGuardExprList :: {NonEmpty (Expression SourceRange)}
  : guardExpr {$1 :| []}
  | revGuardExprList ',' guardExpr {$3 <| $1}

primaryGuardExpr :: {Expression SourceRange}
  : guardOperand {$1}
  | typeNoName '(' expr optcomma ')' {pos ($1, $5) Conversion $1 $3}
  | primaryGuardExpr '.' identifier {pos ($1, $3) FieldSelector $1 $3}
  | primaryGuardExpr '[' expr ']' {pos ($1, $4) IndexExpr $1 $3}
  | primaryGuardExpr '[' optexpr ':' optexpr ']' {pos ($1,$6) SliceExpr $1 $3 $5 Nothing}
  | primaryGuardExpr '[' optexpr ':' expr ':' expr ']' {pos ($1,$8) SliceExpr $1 $3 (Just $5) (Just $7)}
  | primaryGuardExpr '.' '(' type_ ')' {pos ($1, $5) TypeAssertion $1 $4}
  | primaryGuardExpr arguments {let (mt, args, mspread) = $2 in pos ($1, ((mt,args), mspread)) CallExpr $1 mt args mspread} 

optArgs :: {Maybe (NonEmpty (Expression SourceRange))}
  : nonEmptyExprList {Just $1}
  |                  {Nothing}

optellipsis :: {Bool}
  : '...' {True}
  |       {False}

guardOperand :: {Expression SourceRange}
  : int {pos $1 IntLit (getInt $1)}
  | float {pos $1 FloatLit (getFloat $1)}
  | imaginary {pos $1 ImaginaryLit (getImaginary $1)}
  | rune {pos $1 RuneLit (getRune $1)}
  | string {pos $1 StringLit (getString $1)}
  | 'func' parameters optreturns block {pos ($1, $4) FunctionLit $2 $3 $4}
  | literalTypeNoName literalValue {$2 $1}
  | identifier {pos $1 Name Nothing $1}
  | '(' expr ')' {$2}

binop :: {BinaryOp}
  : '||' {LogicalOr}
  | '&&' {LogicalAnd}
  | rel_op {$1}
  | add_op {$1}
  | mul_op {$1}

rel_op :: {BinaryOp}
  : '==' {Equals}
  | '!=' {NotEquals}
  | '<' {Less}
  | '<=' {LessEq}
  | '>' {Greater}
  | '>=' {GreaterEq}

add_op :: {BinaryOp}
  : '+' {Add}
  | '-' {Subtract}
  | '|' {BitwiseOr}
  | '^' {BitwiseXOr}

mul_op :: {BinaryOp}
  : '*' {Multiply}
  | '/' {Divide}
  | '%' {Remainder}
  | '&' {BitwiseAnd}
  | '&^' {BitwiseNAnd}
  | '<<' {LeftShift}
  | '>>' {RightShift}

assign_op :: {AssignOp}
  : '=' {Assignment}
  | '+=' {ComplexAssign Add}
  | '-=' {ComplexAssign Subtract}
  | '*=' {ComplexAssign Multiply}
  | '/=' {ComplexAssign Divide}
  | '%=' {ComplexAssign Remainder}
  | '&=' {ComplexAssign BitwiseAnd}
  | '|=' {ComplexAssign BitwiseOr}
  | '^=' {ComplexAssign BitwiseXOr}
  | '<<=' {ComplexAssign LeftShift}
  | '>>=' {ComplexAssign RightShift}
  | '&^=' {ComplexAssign BitwiseNAnd}

literalTypeNoName :: {Type SourceRange}
  : structType {$1}
  | arrayType {$1}
  | '[' '...' ']' type_ {pos ($1, $4) ArrayType Nothing $4}
  | sliceType {$1}
  | mapType {$1}

literalValue :: {Type SourceRange -> Expression SourceRange}
  : '{' '}' {\t -> (pos ($1, $2) CompositeLit t [])}
  | '{' keyedElements optcomma '}' {\t -> pos (t, $4) CompositeLit t ($2 t)}

keyedElements :: {Type SourceRange -> [Element SourceRange]}
  : revKeyedElements1 {reverse . $1}

revKeyedElements1 :: {Type SourceRange -> [Element SourceRange]}
  : keyedElement                       {\t -> [$1 t]}
  | revKeyedElements1 ',' keyedElement {\t -> ($3 t):($1 t)}
  
keyedElement :: {Type SourceRange -> Element SourceRange}
  : elementOrKey {\t -> pos ($1 t) Element ($1 t)}
  | elementOrKey ':' elementOrKey {\t ->
     let k = case ($1 t) of {
        Name p Nothing id -> FieldKey p id;
        e                 -> pos e ExprKey e;
     } in pos (k, $3 t) KeyedEl k ($1 t)}


elementOrKey :: {Type SourceRange -> Expression SourceRange}
  : expr {const $1}
  | literalValue {$1}

blockStmt :: {Statement SourceRange}
  : '{' '}' {pos ($1, $2) BlockStmt []}
  | '{' revstmts1 closebrace {pos ($1,$3) BlockStmt (reverse $2)}
  
block :: {[Statement SourceRange]}
  : blockStmt {let BlockStmt _ stmts = $1 in stmts}

optblock :: {Maybe [Statement SourceRange]}
  : block {Just $1}
  |       {Nothing}

stmts :: {[Statement SourceRange]}
  : {[]}
  | revstmts1 ';' {reverse $1}

revstmts1 :: {[Statement SourceRange]}
  : stmt {[$1]}
  | revstmts1 ';' stmt {$3 : $1}

stmt :: { Statement SourceRange }
  : simpleStmt {$1}
  | declaration {pos $1 DeclStmt $1}
  | label ':' stmt {pos ($1, $3) LabeledStmt $1 $3}
  | ifStmt {$1}
  | 'go' expr {pos ($1, $2) GoStmt $2}
  | 'return' exprList {pos ($1,$2) ReturnStmt $2}
  | 'break' optlabel {pos ($1, $2) BreakStmt $2}
  | 'continue' optlabel {pos ($1, $2) ContinueStmt $2}
  | 'goto' label {pos ($1, $2) GotoStmt $2}
  | 'fallthrough' {pos $1 FallthroughStmt}
  | blockStmt {$1}
  | 'defer' expr {pos ($1, $2) DeferStmt $2}
  | 'switch' optGuardSimpleStmtSemi optGuardExpr '{' exprclauses closebrace  {
       pos ($1, $6) ExprSwitchStmt $2 $3 $5
     }
  | 'switch' optGuardSimpleStmtSemi optShortVarLHS primaryGuardExpr '.' '(' 'type' ')' '{' typeclauses closebrace  {pos ($1,$11) TypeSwitchStmt $2 (pos (($3,$4),$8) TypeSwitchGuard $3 $4) $10}
  | 'for' forclause block {pos ($1,$3) ForStmt $2 $3}
  | 'select' '{' commclauses closebrace {pos ($1,$4) SelectStmt $3}

optShortVarLHS :: {Maybe (Id SourceRange)}
  : identifier ':=' {Just $1}
  |                 {Nothing}

exprclauses :: {[ExprClause SourceRange]}
  : revexprclauses1 {reverse $1}
  |                {[]}

revexprclauses1 :: {[ExprClause SourceRange]}
  : revexprclauses1 exprclause {$2 : $1}
  | exprclause                 { [$1] }

exprclause :: {ExprClause SourceRange}
  : 'case' nonEmptyExprList ':' stmts {pos ($1, $4) ExprClause (NE.toList $2) $4}
  | 'default' ':' stmts {pos ($1,$3) ExprClause [] $3}

typeclauses :: {[TypeClause SourceRange]}
  : revtypeclauses1 {reverse $1}
  |                {[]}

revtypeclauses1 :: {[TypeClause SourceRange]}
  : revtypeclauses1 typeclause {$2 : $1}
  | typeclause                 {[$1]}
  
typeclause :: {TypeClause SourceRange}
  : 'case' typeList ':' stmts {pos ($1, $3) TypeClause (NE.toList $2) $4}
  | 'default' ':' stmts {pos ($1,$3) TypeClause [] $3}

forclause :: {ForClause SourceRange}
  -- condition and forclause case
  : revForClauseGuards block {%
      case $1 of {
        (Just (ExpressionStmt _ e)) :| [] -> return (pos (e, $2) ForClause Nothing (Just e) Nothing) ;
        ms2 :| [Just (ExpressionStmt _ e), ms1] -> return (pos ((ms1, e), $2) ForClause ms1 (Just e) ms2) ;
        ms2 :| [Nothing, ms1] -> return (pos ((ms1, ms2), $2) ForClause ms1 Nothing ms2);
        _ -> unexpected (NE.reverse $1) "Format of the for statement clause"
      }
    }
  -- rangeclause case
  | guardExprList rangeassign 'range' guardExpr  {%
       do {aod <- if $2 then return (Assign (NE.toList $1))
                  else liftM Decl (mapM coerceToId (NE.toList $1));
           return (pos ($1, $4) ForRange aod $4)}
    }
  | 'range' guardExpr {pos ($1, $2) ForRange AODNone $2} 

revForClauseGuards :: {NonEmpty (Maybe (Statement SourceRange))}
  : optGuardSimpleStmt {$1 :| []}
  | revForClauseGuards ';' optGuardSimpleStmt {$3 <| $1}
  
rangeassign :: {Bool}
  : '=' {False}
  | ':=' {True}

commclauses :: {[CommClause SourceRange]}
  : revcommclauses1 {reverse $1}
  |                {[]}

revcommclauses1 :: {[CommClause SourceRange]}
  : revcommclauses1 commclause {$2 : $1}
  | commclause                 {[$1]}
  
commclause :: {CommClause SourceRange}
  : commcase ':' stmts {pos ($1,$3) CommClause $1 $3}

typeList :: {NonEmpty (Type SourceRange)}
  : revTypeList1 {NE.reverse $1}

revTypeList1 :: {NonEmpty (Type SourceRange)}
  : type_ {$1 :| []}
  | revTypeList1 ',' type_ { $3 <| $1 }
  
commcase :: {CommOp SourceRange}
  : 'case' nonEmptyExprList '=' expr {pos ($1,$4) CommReceive (Assign (NE.toList $2)) $4}
  | 'case' nonEmptyExprList ':=' expr {%
       do {ids <- mapM coerceToId (NE.toList $2);
           return (pos ($1, $4) CommReceive (Decl ids) $4)}
     }
  | 'case' expr {pos ($1,$2) CommReceive AODNone $2}
  | 'case' expr '<-' expr {pos ($1, $4) CommSend $2 $4}
  | 'default' {pos $1 CommNone}

simpleStmt :: {Statement SourceRange}
  : ';' {EmptyStmt fakeRange}
  | expr {pos $1 ExpressionStmt $1}
  | expr '<-' expr {pos ($1, $3) SendStmt $1 $3}
  | expr '++' {pos ($1, $2) UnaryAssignStmt $1 Inc}
  | expr 'dbl-' {pos ($1, $2) UnaryAssignStmt $1 Dec}
  | nonEmptyExprList assign_op nonEmptyExprList {pos ($1, $3) AssignStmt $1 $2 $3}
  | nonEmptyExprList ':=' nonEmptyExprList {%
     do {ids <- sequence (NE.map coerceToId $1);
         return (pos ($1, $3) ShortVarDeclStmt ids $3)}
    }

-- A special form of simple statements that uses guardExpr instead of expr
guardSimpleStmt :: {Statement SourceRange}
  : ';' {EmptyStmt fakeRange}
  | guardExpr {pos $1 ExpressionStmt $1}
  | guardExpr '<-' guardExpr {pos ($1, $3) SendStmt $1 $3}
  | guardExpr '++' {pos ($1, $2) UnaryAssignStmt $1 Inc}
  | guardExpr 'dbl-' {pos ($1, $2) UnaryAssignStmt $1 Dec}
  | guardExprList assign_op guardExprList {pos ($1, $3) AssignStmt $1 $2 $3}
  | guardExprList ':=' guardExprList {%
     do {ids <- sequence (NE.map coerceToId $1);
         return (pos ($1, $3) ShortVarDeclStmt ids $3)}
    }

optGuardSimpleStmt :: {Maybe (Statement SourceRange)}
  : guardSimpleStmt {Just $1}
  |                 {Nothing}

optGuardSimpleStmtSemi :: {Maybe (Statement SourceRange)}
  : guardSimpleStmt ';' {Just $1}
  |                          {Nothing}

declaration :: {Declaration SourceRange}
  : 'const' constspecs {pos ($1, $2) ConstDecl $2}
  | 'type'  typespecs {pos ($1, $2) TypeDecl $2}
  | 'var'   varspecs {pos ($1, $2) VarDecl $2}

constspecs :: {[ConstSpec SourceRange]}
  : constspec {[$1]}
  | '(' ')' {[]}
  | '(' revconstspecs1 closeparen {reverse $2}

revconstspecs1 :: {[ConstSpec SourceRange]}
  : revconstspecs1 ';' constspec {$3 : $1}
  | constspec                    { [$1] } 

constspec :: {ConstSpec SourceRange}
  : identifierList constspecRHS {pos $1 ConstSpec $1 $2}

constspecRHS :: {Maybe (Maybe (Type SourceRange), NonEmpty (Expression SourceRange))}
  : type_ '=' nonEmptyExprList {Just (Just $1, $3)}
  |       '=' nonEmptyExprList {Just (Nothing, $2)}
  |                            {Nothing}

typespecs :: {[TypeSpec SourceRange]}
  : typespec {[$1]}
  | '(' ')' {[]}
  | '(' revtypespecs1 closeparen {reverse $2}

revtypespecs1 :: {[TypeSpec SourceRange]}
  : revtypespecs1 ';' typespec  {$3 : $1}
  | typespec                    { [$1] }

typespec :: {TypeSpec SourceRange}
  : identifier type_ {pos ($1, $2) TypeSpec $1 $2}

varspecs :: {[VarSpec SourceRange]}
  : varspec {[$1]}
  | '(' ')' {[]}
  | '(' revvarspecs1 closeparen {reverse $2}

revvarspecs1 :: {[VarSpec SourceRange]}
  : revvarspecs1 ';' varspec {$3 : $1}
  | varspec                 { [] }

varspec :: {VarSpec SourceRange}
  : identifierList opttype varspecRHS {
    case ($2, $3) of {
      (Just ty, _) -> pos ($1, $2) TypedVarSpec $1 ty (fromMaybe [] (fmap NE.toList $3)) ;
      (Nothing, Just els) -> pos ($1, $3) UntypedVarSpec $1 els ;
    }
  }

varspecRHS :: {Maybe (NonEmpty (Expression SourceRange))}
  : '=' nonEmptyExprList {Just $2}
  |                      {Nothing}

ifStmt :: {Statement SourceRange}
  : 'if' guardSimpleStmt optsemi optGuardExpr block optElseClause {%
     do {(mstmt, expr) <- case ($2, $3, $4) of {
              ((ExpressionStmt _ expr), False, Nothing) -> return (Nothing, expr);
              (_, False, Nothing) -> unexpected $2 "No if guard expression";
              (_, True, Just e) -> return (Just $2, e);
              (_, False, Just e) -> unexpected e "Missing semicolon";
              (_, True, Nothing) -> unexpected $2 "Extra Semicolon";
         };
         return (pos ($1, ($5,$6)) IfStmt mstmt expr $5 $6);
        }
    }

optsemi :: {Bool}
  : ';' {True}
  |     {False}

optElseClause :: {[Statement SourceRange]}
  : 'else' ifOrBlock {$2}
  |                  {[]}

ifOrBlock :: {[Statement SourceRange]}
  : ifStmt {[$1]}
  | block  {$1}

{

getIdent :: Lexeme Token -> Text
getIdent lex = let T.Ident t = lexemeToken lex in t
getInt lex = let T.IntLit i = lexemeToken lex in i
getFloat lex = let T.FloatLit d = lexemeToken lex in d
getImaginary lex = let T.ImaginaryLit d = lexemeToken lex in d
getRune lex  = let T.RuneLit c = lexemeToken lex in c
getString lex = let T.StringLit t = lexemeToken lex in t

errorP :: [Lexeme Token] -> Either (SourceRange,String) a
errorP ts =
  case ts of
    [] -> Left (fakeRange, "unexpected end of file")
    Lexeme { lexemeRange = rng, lexemeToken = t }:rest ->
      Left (rng, "Parse error: unexpected " ++ show t)

-- Coerce an expression to an identifier, returning an unexpected
-- error if not possible.
coerceToId :: Expression SourceRange -> Either (SourceRange, String) (Id SourceRange)
coerceToId (Name _ Nothing id) = return id
coerceToId e           = unexpected e "Expecting an identifier"

simple2maybeStmt :: Statement a -> Maybe (Statement a)
simple2maybeStmt (EmptyStmt _) = Nothing
simple2maybeStmt s             = Just s

-- | Convert a parameter list to a return list
parametersToReturns :: ParameterList SourceRange -> Either (SourceRange, String) (ReturnList SourceRange)
parametersToReturns pl = case pl of {
  NamedParameterList r nps Nothing -> return (NamedReturnList r nps);
  AnonymousParameterList r aps Nothing -> return (AnonymousReturnList r aps);
  NamedParameterList _ _ (Just variadic) -> unexpected variadic "Illegal spread parameter in return list";
  AnonymousParameterList _ _ (Just variadic) -> unexpected variadic "Illegal spread parameter in return list";
  }

-- | Intermediate AST structure to represent comma-separated
-- components of the parameter list
data Param a = ParamName a (Id a)
             | TypedParam a (Id a) (Type a)
             | TypeNoName a (Type a)
             | SpreadParam a (Type a)
             | SpreadNameParam a (Id a) (Type a)

instance Ranged (Param SourceRange) where
  getRange p = case p of {
    ParamName r _         -> Just r;
    TypedParam r _ _      -> Just r;
    TypeNoName r _        -> Just r;
    SpreadParam r _       -> Just r;
    SpreadNameParam r _ _ -> Just r;
  }

processReverseParameterList :: [Param SourceRange] -> Either (SourceRange, String) (ParameterList SourceRange)
processReverseParameterList ps =
  case ps of {
    [] -> return (pos ps AnonymousParameterList [] Nothing);
    (SpreadParam _ typ):rest -> do {prePars <- toAnonymousParameterList rest;
                                    return (pos ps AnonymousParameterList prePars (Just typ))};
    (SpreadNameParam snprng name typ):rest ->
        do {unless (case rest of {[] -> True; (TypedParam {}):_ -> True; _ -> False})
            (unexpected (head rest) "Mixing named and anonymous arguments");
            prePars <- toNamedParameterList rest;
            return (pos ps NamedParameterList prePars (Just (NamedParameter snprng name typ)))};
    (TypedParam {}):_ ->
        do {prePars <- toNamedParameterList ps;
            return (pos ps NamedParameterList prePars Nothing);
           };
    _ -> do {prePars <- toAnonymousParameterList ps;
             return (pos ps AnonymousParameterList prePars Nothing);
            }
  }

-- |This function will assume all the params are anonymous and in
-- reverse order, meaning treating all names as types. It also doesn't
-- expect any spread params (could only be last params, already
-- processed by `processReverseParameterList`), or typed
-- params. Because this is an anonymous parameter list, every param
-- will correspond to an AnonymousParameter.
toAnonymousParameterList :: [Param SourceRange] -> Either (SourceRange, String) [AnonymousParameter SourceRange]
toAnonymousParameterList = foldM toAnonymousParam []
  where toAnonymousParam accum p =
          do {ap <- case p of
               {ParamName _ ident -> return (pos ident NamedType (pos ident TypeName Nothing ident));
                TypeNoName _ ty       -> return ty;
                p@(SpreadParam {})    -> unexpected p "Spread parameter can only be the last";
                p@(SpreadNameParam {})-> unexpected p "Spread parameter can only be the last";
                p@(TypedParam {})     -> unexpected p "Mixing named and anonymous arguments";
               };
              return (ap:accum)
             }

-- | This function will assume all the params are named and in reverse
-- order, meaning treating all names as variables. It also doesn't
-- expect any spread params (could only be last params, already
-- processed by `processReverseParameterList`), or anonymous params
-- (TypeNoName). Because this is a named parameter list, several
-- params migh be converted to one NamedParameter. Specifically, the
-- sequences of identifiers between TypedParam. Because params are in
-- reverse order, every ParamName we see will need to look up and use
-- the type of the previous named parameter. Because of that, the
-- first element should be a `TypedParam` (already checked by
-- `processReverseParameterList`)
toNamedParameterList :: [Param SourceRange]
                     -> Either (SourceRange, String) [NamedParameter SourceRange]
toNamedParameterList = foldM toNamedParam []
  where {getType (NamedParameter _ _ typ) = typ;
         toNamedParam nps p =
          do {nextNamedParam <-
              case p of {TypedParam rng ident typ -> return (NamedParameter rng ident typ);
                         ParamName rng ident      ->
                            if null nps then unexpected p "toNamedParameterList: I see a ParamName, but previous named parameters are empty (this is not supposed to happen)"
                            else return (NamedParameter rng ident (getType (head nps)));
                         TypeNoName {}  -> unexpected p "Mixing named and anonymous arguments";
                         SpreadParam {}    -> unexpected p "Spread parameter can only be the last";
                         SpreadNameParam {}-> unexpected p "Spread parameter can only be the last";
                        };
              return (nextNamedParam:nps)
             }
        }
}
