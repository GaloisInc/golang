{-|
Module      : Language.Go.Parser
Description : JSON AST deserializer
Maintainer  : abagnall@galois.com
Stability   : experimental

Parse JSON-encoded Go ASTs.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Go.Parser (parseMain, SourcePos) where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)

import           Language.Go.AST
import           Language.Go.Rec
import           Language.Go.Types as T

-- | Entry point.
parseMain :: ByteString -> Either String (Node SourcePos Main)
parseMain txt = eitherDecode txt

data SourcePos =
  SourcePos { pos_filename :: Text
            , pos_column :: Int
            , pos_line :: Int
            , pos_offset :: Int
            }
  deriving (Eq, Show)

noPos :: SourcePos
noPos = SourcePos { pos_filename = ""
                  , pos_column = 0
                  , pos_line = 0
                  , pos_offset = 0
                  }

-- | Type synonym for a single unfolding of the Node type. It's easier
-- to define FromJSON instances for this and derive the corresponding
-- Node instances automatically.
type N (i :: NodeType) = NodeF SourcePos (Fix (NodeF SourcePos)) i

instance FromJSON SourcePos where
  parseJSON = withObject "SourcePos" $ \v -> SourcePos <$>
    v .: "filename" <*> v .: "column" <*> v .: "line" <*> v .: "offset"

instance FromJSON (N i) => FromJSON (Node SourcePos i) where
  parseJSON x = In <$> parseJSON x

instance FromJSON (N Main) where
  parseJSON = withObject "Main" $ \v ->
    MainNode <$> v .: "name" <*> v .: "package" <*> v .: "imports"

instance FromJSON (N Package) where
  parseJSON = withObject "Package" $ \v ->
    PackageNode <$> v .: "name" <*> v .: "path" <*>
    v .: "imports" <*> v .:? "file-paths" .!= [] <*>
    v .: "files" <*> v .: "initializers"

instance FromJSON (N File) where
  parseJSON = withObject "File" $ \v ->
    FileNode <$> v .: "path" <*> v .: "package-name"
    <*> v .: "declarations" <*> v .: "imports"

instance FromJSON Ident where
  parseJSON = withObject "Ident" $ \v ->
    Ident <$> v .: "ident-kind" <*> v .: "value"

instance FromJSON T.IdentKind where
  parseJSON = withText "IdentKind" $ \txt ->
    case lookup txt [("Builtin", T.IdentBuiltin), ("Const", T.IdentConst),
                     ("Func", T.IdentFunc), ("Label", T.IdentLabel),
                     ("Nil", T.IdentNil), ("PkgName", T.IdentPkgName),
                     ("TypeName", T.IdentTypeName), ("Var", T.IdentVar),
                     ("NoKind", T.IdentNoKind)] of
      Just k -> return k
      Nothing ->
        fail $ "FromJSON IdentKind: unknown identifer kind " ++ show txt

instance FromJSON (N Decl) where
  parseJSON = withObject "Decl" $ \v -> do
    tp <- v .: "type"
    pos <- v .: "position"
    case tp :: Text of
      x | x `elem` ["function", "method"] ->
            FuncDecl pos <$> v .:? "receiver" <*> v .: "name"
            <*> v .: "params" <*> v .:? "variadic" <*> v .:? "results" .!= []
            <*> v .:? "body"
      "type-alias" -> TypeAliasDecl pos <$> v .: "binds"
      _ -> GenDecl pos <$> v .: "specs"


instance FromJSON (N Bind) where
  parseJSON = withObject "Binding" $ \v ->
    Binding <$> v .: "name" <*> v .: "value"

instance FromJSON (N Field) where
  parseJSON = withObject "Field" $ \v -> FieldNode <$>
    v .:? "names" .!= [] <*> v .: "declared-type" <*> v .:? "tag"

instance FromJSON BasicLit where
  parseJSON = withObject "BasicLit" $ \v ->
    BasicLit <$> v .: "type" <*> v .: "value"

instance FromJSON BasicLitType where
  parseJSON = withText "BasicLitType" $ \txt ->
    case lookup txt [("BOOL", LiteralBool), ("INT", LiteralInt),
                     ("FLOAT", LiteralFloat), ("COMPLEX", LiteralComplex),
                     ("IMAG", LiteralImag), ("CHAR", LiteralChar),
                     ("STRING", LiteralString)] of
      Just tp -> return tp
      _ ->
        fail $ "FromJSON BasicLitType: unknown basic literal type " ++ show txt

instance FromJSON BasicConst where
  parseJSON = withObject "BasicConst" $ \v -> do
    exprType <- v .: "type"
    case exprType :: Text of
      "BOOL" -> BasicConstBool . readBool <$> v .: "value"
      "STRING" -> BasicConstString <$> v .: "value"
      "INT" -> BasicConstInt . read <$> v .: "value"
      "FLOAT" -> BasicConstFloat <$> v .: "numerator" <*> v .: "denominator"
      "COMPLEX" -> BasicConstFloat <$> v .: "real" <*> v .: "imaginary"
      _t ->
        fail $ "FromJSON BasicConst: unknown constant type: " ++ show exprType

instance FromJSON ChanDir where
  parseJSON = withText "ChanDir" $ \txt -> case txt of
    "send" -> return ChanDirSend
    "recv" -> return ChanDirRecv
    "both" -> return ChanDirBoth
    _ -> fail $ "FromJSON ChanDir: unknown channel direction " ++ show txt

instance FromJSON (N Expr) where
  parseJSON = withObject "Expr" $ \v -> do
    exprKind <- v .: "kind"
    pos <- v .:? "position" .!= noPos
    go_tp <- v .:? "go-type" .!= T.NoType
    case exprKind :: Text of
      "type" -> do
        exprType <- v .: "type"
        case exprType :: Text of
          tp | tp `elem` ["slice", "array"] ->
               ArrayTypeExpr pos go_tp <$> v .:? "length" <*> v .: "element"
          "pointer" -> PointerTypeExpr pos go_tp <$> v .: "contained"
          "interface" ->
            InterfaceTypeExpr pos go_tp <$> v .: "methods" <*> v .: "incomplete"
          "map" -> MapTypeExpr pos go_tp <$> v .: "key" <*> v .: "value"
          "chan" -> ChanTypeExpr pos go_tp <$> v .: "direction" <*> v .: "value"
          "struct" -> StructTypeExpr pos go_tp <$> v .: "fields"
          "function" -> FuncTypeExpr pos go_tp <$> v .: "params" <*>
                        v .:? "variadic" <*> v .:? "results" .!= []
          "ellipsis" -> EllipsisExpr pos go_tp <$> v .:? "value"
          "identifier" ->
            IdentExpr pos go_tp <$> v .:? "qualifier" <*> v .: "value"
          _ -> fail $ "FromJSON Expr: unknown expression of kind 'type': "
               ++ show exprType
      "literal" -> do
        exprType <- v .: "type"
        case exprType :: Text of
          "function" -> FuncLitExpr pos go_tp <$> v .: "params" <*>
                        v .:? "results" .!= [] <*> v .: "body"
          "composite" ->
            CompositeLitExpr pos go_tp <$> v .:? "declared" <*> v .: "values"
          _ -> BasicLitExpr pos go_tp <$> parseJSON (Object v)
      "constant" -> BasicConstExpr pos go_tp <$> v .: "value"
      "expression" -> do
        exprType <- v .: "type"
        case exprType :: Text of
          "index" -> IndexExpr pos go_tp <$> v .: "target" <*> v .: "index"
          "star" -> StarExpr pos go_tp <$> v .: "target"
          "call" -> CallExpr pos go_tp <$>
                    v .: "ellipsis" <*> v .: "function" <*> v .: "arguments"
          "cast" -> CastExpr pos go_tp <$> v .: "target" <*> v .: "coerced-to"
          -- Special cases for 'make' and 'new' builtins
          "make" -> do
            type_arg <- v .: "argument"
            rest_args <- v .: "rest"
            return $ CallExpr pos go_tp False
              (In $ IdentExpr pos (T.makeType $ typeOf' type_arg) Nothing $
                Ident T.IdentFunc "make") $
              type_arg : rest_args
          "new" -> do
            type_arg <- v .: "argument"
            return $ CallExpr pos go_tp False
              (In $ IdentExpr pos (T.newType $ typeOf' type_arg) Nothing $
                Ident T.IdentFunc "new") [type_arg]
          "paren" -> ParenExpr pos go_tp <$> v .: "target"
          "selector" ->
            SelectorExpr pos go_tp <$> v .: "target" <*> v .: "field"
          "type-assert" ->
            TypeAssertExpr pos go_tp <$> v .: "target" <*> v .:? "asserted"
          "identifier" -> do
            v' <- v .: "value"
            mAsIOTA <- v' .:? "type"

            -- Special case for IOTA: whenever the type is "IOTA"
            -- the value should be "IOTA" as well.
            case mAsIOTA of
              Just ("IOTA" :: Text) -> return $ IdentExpr pos go_tp Nothing $
                               Ident T.IdentNoKind "IOTA"
              _ -> IdentExpr pos go_tp <$> v .:? "qualifier" <*> v .: "value"
          "slice" ->
            SliceExpr pos go_tp <$> v .: "target" <*>
            v .: "low" <*> v .: "high" <*> v .: "max" <*> v .: "three"
          "key-value" -> KeyValueExpr pos go_tp <$> v .: "key" <*> v .: "value"
          "ellipsis" -> EllipsisExpr pos go_tp <$> v .:? "value"
          "binary" -> BinaryExpr pos go_tp <$>
                      v .: "left" <*> v .: "operator" <*> v .: "right"
          "unary" -> UnaryExpr pos go_tp <$> v .: "operator" <*> v .: "target"
          _ -> fail $ "FromJSON Expr: unknown expression of kind 'expression': "
               ++ show exprType
      _ -> fail $ "FromJSON Expr: unknown kind " ++ show exprKind

instance FromJSON BinaryOp where
  parseJSON = withText "BinaryOp" $ \txt ->
    case lookup txt [("+", BPlus), ("-", BMinus), ("*", BMult), ("/", BDiv),
                     ("%", BMod), ("&", BAnd), ("|", BOr), ("^", BXor),
                     ("<<", BShiftL), (">>", BShiftR), ("&^", BAndNot),
                     ("&&", BLAnd), ("||", BLOr), ("==", BEq), ("<", BLt),
                     (">", BGt), ("!=", BNeq), ("<=", BLeq), (">=", BGeq)] of
      Just op -> return op
      _ -> fail $ "FromJSON BinaryOP: unknown binary operator " ++ show txt

instance FromJSON UnaryOp where
  parseJSON = withText "UnaryOp" $ \txt ->
    case lookup txt [("+", UPlus), ("-", UMinus), ("!", UNot),
                     ("^", UBitwiseNot), ("*", UStar), ("&", UAddress),
                     ("<-", UArrow)] of
      Just op -> return op
      _ -> fail $ "FromJSON UnaryOp: unknown unary operator " ++ show txt

instance FromJSON BranchType where
  parseJSON = withText "BranchType" $ \txt -> case txt of
    "break" -> return Break
    "continue" -> return Continue
    "goto" -> return Goto
    "fallthrough" -> return Fallthrough
    _ -> fail $ "FromJSON BranchType: unknown branch type " ++ show txt

instance FromJSON (N Block) where
  parseJSON o = BlockNode <$> parseJSON o

instance FromJSON (N Stmt) where
  parseJSON = withObject "Stmt" $ \v -> do
    stmtType <- v .: "type"
    pos <- v .:? "position" .!= noPos
    case stmtType :: Text of
      "return" -> ReturnStmt pos <$> v .:? "values" .!= []
      "empty" -> return $ EmptyStmt pos
      "expression" -> ExprStmt pos <$> v .: "value"
      "labeled" -> LabeledStmt pos <$> v .: "label" <*> v .: "statement"
      "branch" -> BranchStmt pos <$> v .: "type" <*> v .:? "label"
      "range" -> RangeStmt pos <$> v .: "key" <*> v .: "value" <*>
                 v .: "target" <*> v .: "body" <*> v .: "is-assign"
      "declaration" -> DeclStmt pos <$> v .: "target"
      "defer" -> DeferStmt pos <$> v .: "target"
      "if" -> IfStmt pos <$> v .:? "init" <*>
              v .: "condition" <*> v .: "body" <*> v .:? "else"
      "block" -> BlockStmt pos <$> v .: "body"
      "for" -> ForStmt pos <$> v .:? "init" <*>
               v .:? "condition" <*> v .:? "post" <*> v .: "body"
      "go" -> GoStmt pos <$> v .: "target"
      "send" -> SendStmt pos <$> v .: "channel" <*> v .: "value"
      "select" -> SelectStmt pos <$> v .: "body"
      "crement" -> do
        op <- v .: "operation"
        case op :: Text of
          "++" -> IncDecStmt pos <$> v .: "target" <*> pure True
          "--" -> IncDecStmt pos <$> v .: "target" <*> pure False
          _ -> fail $ "FromJSON IncDecStatement: unknown operator " ++ show op
      "switch" ->
        SwitchStmt pos <$> v .:? "init" <*> v .:? "condition" <*> v .: "body"
      "type-switch" ->
        TypeSwitchStmt pos <$> v .:? "init" <*> v .: "assign" <*> v .: "body"
      "select-clause" ->
        CommClauseStmt pos <$> v .:? "statement" <*> v .: "body"
      "case-clause" -> CaseClauseStmt pos <$> v .: "expressions" <*> v .: "body"
      tp | tp `elem` ["assign", "define", "assign-operator"] -> do
             assignType <- v .: "type"
             case assignType :: Text of
               "assign" ->
                 AssignStmt pos Assign Nothing <$> v .: "left" <*> v .: "right"
               "define" ->
                 AssignStmt pos Define Nothing <$> v .: "left" <*> v .: "right"
               "assign-operator" ->
                 AssignStmt pos AssignOperator <$> (Just <$> v .: "operator")
                 <*> v .: "left" <*> v .: "right"
               _ -> fail $ "FromJSON AssignStatement: unknown assign type "
                    ++ show assignType
      "initializer" ->
        InitializerStmt <$> v .: "vars" <*> ((:[]) <$> v .: "value")
      tp | tp `elem` ["break", "continue", "goto", "fallthrough"] ->
           BranchStmt pos <$> v .: "type" <*> v .:? "label"
      _ -> fail $ "FromJSON Stmt: unknown statement type " ++ show stmtType

instance FromJSON (N Spec) where
  parseJSON = withObject "Spec" $ \v -> do
    pos <- v .: "position"
    specType <- v .: "type"
    case specType :: Text of
      "import" -> ImportSpec pos <$> v .:? "name" <*> v .: "path"
      "const" -> ConstSpec pos <$> v .: "names" <*>
                 v .:? "declared-type" <*> v .: "values"
      "var" -> VarSpec pos <$> v .: "names" <*>
               v .:? "declared-type" <*> v .: "values"
      "type" -> TypeSpec pos <$> v .: "name" <*> v .: "value"
      _ -> fail $ "FromJSON Spec: unknown spec type " ++ show specType

instance FromJSON T.BasicKind where
  parseJSON = withText "BasicKind" $ \txt -> case lookup txt
    [("Invalid", BasicInvalid), ("Bool", BasicBool),
     ("Int", BasicInt Nothing), ("Int8", BasicInt $ Just 8),
     ("Int16", BasicInt $ Just 16), ("Int32", BasicInt $ Just 32),
     ("Int64", BasicInt $ Just 64), ("UInt", BasicUInt Nothing),
     ("UInt8", BasicUInt $ Just 8), ("UInt16", BasicUInt $ Just 16),
     ("UInt32", BasicUInt $ Just 32), ("UInt64", BasicUInt $ Just 64),
     ("UIntptr", BasicUIntptr), ("Float32", BasicFloat 32),
     ("Float64", BasicFloat 64), ("Complex64", BasicComplex 64),
     ("Complex128", BasicComplex 128), ("String", BasicString),
     ("UnsafePointer", BasicUnsafePointer),
     ("UntypedBool", BasicUntyped UntypedBool),
     ("UntypedInt", BasicUntyped UntypedInt),
     ("UntypedRune", BasicUntyped UntypedRune),
     ("UntypedFloat", BasicUntyped UntypedFloat),
     ("UntypedComplex", BasicUntyped UntypedComplex),
     ("UntypedString", BasicUntyped UntypedString),
     ("UntypedNil", BasicUntyped UntypedNil)] of
    Just k -> return k
    _ -> fail $ "FromJSON BasicKind: unknown basic kind " ++ show txt

instance FromJSON T.NameType where
  parseJSON = withObject "NameType" $ \v ->
    T.NameType <$> v .: "name" <*> v .: "type"

instance FromJSON T.Type where
  parseJSON = withObject "Type" $ \v -> do
    tp <- v .: "type"
    case tp :: Text of
      "Array" -> T.ArrayType <$> v .: "len" <*> v .: "elem"
      "Basic" -> T.BasicType <$> v .: "kind"
      "Chan" -> T.ChanType <$> v .: "direction" <*> v .: "elem"
      "Interface" -> T.InterfaceType <$> v .: "methods"
      "Map" -> T.MapType <$> v .: "key" <*> v .: "elem"
      "Named" -> T.NamedType <$> v .: "underlying"
      "Pointer" -> T.PointerType <$> v .: "elem"
      "Signature" -> do
        params <- v .: "params"
        -- Desugar 1-tuple result types to the inner type.
        result <- v .: "results" >>= \rs -> case rs of
          T.TupleType [rt] -> return $ T.typeOfNameType rt
          _ -> return rs
        case params of
          T.TupleType ts ->
            T.FuncType <$> v .:? "recv" <*> pure (T.typeOfNameType <$> ts)
            <*> pure result <*> v .: "variadic"
          _ -> fail $ "FromJSON Type: expected TupleType, got " ++ show params
      "Slice" -> T.SliceType <$> v .: "elem"
      "Struct" -> T.StructType <$> v .: "fields"
      "Tuple" -> T.TupleType <$> v .: "fields"
      _ -> fail $ "FromJSON Type: unknown type " ++ show v
