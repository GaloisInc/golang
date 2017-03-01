{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, MultiParamTypeClasses, LambdaCase, FlexibleInstances, TemplateHaskell, StandaloneDeriving #-}
-- | The grammar of Go has the unfortunate property of having
-- non-nested overlapping categories: that is categories that have
-- both common and different elements. The examples of this are 1)
-- Types, TypeLiterals and LiteralTypes and 2) TopLevelDeclarations
-- and Statements.
--
-- Encoding this with algebraic data-types is challenging and requires
-- either deeply nested trees with some duplication of constructors
-- for the categories at the intersections, or usage of GADTs and type
-- families or kinds to restrict the shape of flatter trees. Both
-- approaches are involved and cumbersome, so instead we design ASTs
-- to represent a superset of actual Go syntax and a run-time
-- predicate `isValid` to restrict the valid AST instances.
--
-- In future we might look into pushing the restriction more into the
-- type system, but for now things are guaranteed to behave iff
-- `isValid` holds true for a given AST. Of course, the parser is
-- guaranteed to produce only valid trees.
module Language.Go.AST (SourceRange (..)
                       ,SourcePos (..)
                       ,Package (..)
                       ,File (..)
                       ,fileName
                       ,ImportDecl (..)
                       ,ImportSpec (..)
                       ,ImportType (..)
                       ,Path
                       ,Declaration (..)
                       ,TopLevel (..)
                       ,Statement (..)
                       ,IncDec (..)
                       ,AssignOp (..)
                       ,ExprClause (..)
                       ,TypeSwitchGuard (..)
                       ,TypeClause (..)
                       ,ForClause (..)
                       ,AssignOrDecl (..)
                       ,CommClause (..)
                       ,CommOp (..)
                       ,ConstSpec (..)
                       ,VarSpec (..)
                       ,TypeSpec (..)
                       ,Type (..)
                       ,TypeName (..)
                       ,ParameterList (..)
                       ,ReturnList (..)
                       ,NamedParameter (..)
                       ,AnonymousParameter (..)
                       ,Label (..)
                       ,FieldDecl (..)
                       ,Tag (..)
                       ,MethodSpec (..)
                       ,ChannelDirection (..)
                       ,Receiver (..)
                       ,Expression (..)
                       ,Element (..)
                       ,Key (..)
                       ,BinaryOp (..)
                       ,UnaryOp (..)
                       ,Id (..)
                       ,Binding(..)
                       ,bindingImported
                       ,bindingKind
                       ,bindingDeclLoc
                       ,bindingThisScope
                       ,BindingKind(..)
                       ,SemanticType (..)
                       ,ann
                       ,Annotated) where

import Data.Data (Data)
import Data.Data (Typeable)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Data.Maybe
import Lens.Simple
import Data.Either (Either)
import Language.Annotated
import Data.Semigroup
import AlexTools (SourceRange(..), SourcePos (..))
import Data.Map (Map)
import Data.HashMap.Lazy (HashMap)

data Package a = Package a Text {- package name -} (NonEmpty (File a))
  deriving (Show, Data, Typeable, Functor)

data File a = File a Text {- File name -} Text {- package name -} [ImportDecl a] [TopLevel a]
  deriving (Show, Data, Typeable, Functor)

fileName :: File a -> Text
fileName (File _ fname _ _ _ ) = fname

data ImportDecl a = ImportDecl a [ImportSpec a]
  deriving (Show, Data, Typeable, Functor)

data ImportSpec a = Import a ImportType Path
  deriving (Show, Data, Typeable, Functor)

data ImportType = ImportAll
                | ImportQualified (Maybe Text)
  deriving (Show, Data, Typeable)                  

type Path = Text

data Declaration a = TypeDecl a [TypeSpec a]
                   | ConstDecl a [ConstSpec a]
                   | VarDecl a [VarSpec a]
  deriving (Show, Data, Typeable, Functor)

data TopLevel a = FunctionDecl a (Id a) (ParameterList a) (ReturnList a) (Maybe [Statement a])
                | MethodDecl a (Receiver a) (Id a) (ParameterList a) (ReturnList a) (Maybe [Statement a])
                | TopDecl a (Declaration a)
  deriving (Show, Data, Typeable, Functor)

data Statement a = DeclStmt a (Declaration a)
                 | ExpressionStmt a (Expression a)
                 | LabeledStmt a (Label a) (Statement a)
                 | EmptyStmt a
                 | SendStmt a (Expression a) {- ^Channel-} (Expression a) {- ^Value-}
                 | BlockStmt a [Statement a]
                 | UnaryAssignStmt a (Expression a) IncDec
                 | AssignStmt a (NonEmpty (Expression a)) AssignOp (NonEmpty (Expression a))
                 | IfStmt a (Maybe (Statement a)) {- ^ guard prefix, a simple statement -}
                   (Expression a) {- ^guard -}
                   [Statement a] {- ^then -} [Statement a] {- ^else -}
                 | ExprSwitchStmt a (Maybe (Statement a)) {- ^guard prefix, a simple statement -}
                   (Maybe (Expression a)) {- ^guard -} [ExprClause a]
                 | TypeSwitchStmt a (Maybe (Statement a))  {- ^guard prefix, a simple statement -}
                   (TypeSwitchGuard a) [TypeClause a]
                 | ForStmt a (ForClause a) [Statement a]
                 | GoStmt a (Expression a) -- ^ Go statement: fork a function
                 | SelectStmt a [CommClause a] -- ^ select statement
                 | BreakStmt a (Maybe (Label a))
                 | ContinueStmt a (Maybe (Label a))
                 | ReturnStmt a [Expression a]
                 | GotoStmt a (Label a)
                 | FallthroughStmt a
                 | DeferStmt a (Expression a) -- ^ Deferred function call
                 | ShortVarDeclStmt a (NonEmpty (Id a)) (NonEmpty (Expression a))
                 deriving (Show, Data, Typeable, Functor)

-- | Increment or decrement operation
data IncDec = Inc | Dec
  deriving (Show, Data, Typeable)

-- | Assignment operation
data AssignOp = Assignment | ComplexAssign BinaryOp
  deriving (Show, Data, Typeable)

-- | Expression switch statement clause
data ExprClause a = ExprClause a [Expression a] {- Case values; if empty then it's a default case -} [Statement a]
  deriving (Show, Data, Typeable, Functor)

-- | Guard of a type switch statement
data TypeSwitchGuard a = TypeSwitchGuard a (Maybe (Id a)) (Expression a)
  deriving (Show, Data, Typeable, Functor)

-- | Type switch case clause
data TypeClause a = TypeClause a [Type a] {- type case values; if empty then it's a default case -} [Statement a]
  deriving (Show, Data, Typeable, Functor)

-- | For statement clauses. Note we overload this name from the spec
-- to also include conditional and range clauses.
data ForClause a = ForClause a (Maybe (Statement a)) (Maybe (Expression a)) (Maybe (Statement a))
                 -- ^C-style for-clause with an initializer, iteration
                 -- condition and post-iteration action. This also
                 -- includes the "Condition" form of the clause in the
                 -- spec, which is equivalent to a clause without the
                 -- pre- and post-statements.
                 | ForRange a (AssignOrDecl a) (Expression a)
  deriving (Show, Data, Typeable, Functor)

-- | An assignment ('=') or declaration ('=') for ForRange and
-- CommReceive. Because the Functor instance for Either doesn't work for us.
data AssignOrDecl a = Assign [Expression a]
                    | Decl [Id a]
                    | AODNone
  deriving (Show, Data, Typeable, Functor)


-- | Communication clause for the select statement
data CommClause a = CommClause a (CommOp a) {- ^if CommNone, it's a default clause -} [Statement a]
  deriving (Show, Data, Typeable, Functor)

data CommOp a = CommSend a (Expression a) (Expression a)
              | CommReceive a (AssignOrDecl a) (Expression a)
              | CommNone a
  deriving (Show, Data, Typeable, Functor)

data ConstSpec a = ConstSpec a (NonEmpty (Id a)) (Maybe ((Maybe (Type a)), (NonEmpty (Expression a))))
  deriving (Show, Data, Typeable, Functor)

data VarSpec a = TypedVarSpec a (NonEmpty (Id a)) (Type a) [Expression a]
               | UntypedVarSpec a (NonEmpty (Id a)) (NonEmpty (Expression a))
  deriving (Show, Data, Typeable, Functor)

-- | Type specification
data TypeSpec a = TypeSpec a (Id a) (Type a)
  deriving (Show, Data, Typeable, Functor)

-- | Types
data Type a = NamedType a (TypeName a)
            | ArrayType a (Maybe (Expression a))
              -- ^Length. If Nothing, then the length is to be
              -- determined from that of the literal. Hence, Nothing
              -- is only valid for Literal Types
              (Type a) -- ^Element type
            | StructType a [FieldDecl a]
            | PointerType a (Type a)
            | FunctionType a (ParameterList a) (ReturnList a)
            | InterfaceType a [MethodSpec a]
            | SliceType a (Type a)
            -- ^A slice type denotes the set of all slices of arrays
            -- of its element type.
            | MapType a (Type a) (Type a)
            | ChannelType a (ChannelDirection a) (Type a)
  deriving (Show, Data, Typeable, Functor)

-- | Named types
data TypeName a = TypeName a (Maybe (Id a)) -- ^Package id for qualified names
                             (Id a) -- ^Type identifier
  deriving (Show, Data, Typeable, Functor)

-- | Parameter lists
data ParameterList a = NamedParameterList a [NamedParameter a] (Maybe (NamedParameter a)) -- ^The last one is the optional variadic parameter
                     | AnonymousParameterList a [AnonymousParameter a] (Maybe (AnonymousParameter a))
  deriving (Show, Data, Typeable, Functor)


-- | Return lists
data ReturnList a = NamedReturnList a [NamedParameter a]
                  | AnonymousReturnList a [AnonymousParameter a]
  deriving (Show, Data, Typeable, Functor)

-- | Named parameters. Unzipped so that identifiers are paired with types. (As opposed to the grammar where every)
data NamedParameter a = NamedParameter a (Id a) (Type a)
  deriving (Show, Data, Typeable, Functor)

type AnonymousParameter = Type

-- | Identifiers
data Id a = Id a Binding Text -- ^are either names
          | BlankId a -- ^or blank ("_")
  deriving (Show, Data, Typeable, Functor)

-- | Statement labels
data Label a = Label a Text
  deriving (Show, Data, Typeable, Functor)

-- | Field declaration
data FieldDecl a = NamedFieldDecl a (NonEmpty (Id a)) (Type a) (Maybe (Tag a))
                 | AnonymousFieldDecl a (TypeName a) (Maybe (Tag a))
  deriving (Show, Data, Typeable, Functor)
                 

data Tag a = Tag a Text
  deriving (Show, Data, Typeable, Functor)

-- | Method specification
data MethodSpec a = MethodSpec a (Id a) (ParameterList a) (ReturnList a) | InterfaceSpec a (TypeName a)
  deriving (Show, Data, Typeable, Functor)

data ChannelDirection a = Send a | Recv a | Duplex a
  deriving (Show, Data, Typeable, Functor)

data Receiver a = Receiver a (Maybe (Id a)) Bool {- Pointed? -} (TypeName a)
  deriving (Show, Data, Typeable, Functor)

data Expression a = IntLit a Integer
                  | FloatLit a Double
                  | ImaginaryLit a Double
                  | RuneLit a Char
                  | StringLit a Text
                  | FunctionLit a (ParameterList a) (ReturnList a) [Statement a]
                  | CompositeLit a (Type a) [Element a]
                  | MethodExpr a (Receiver a) (Id a)
                  | CallExpr a (Expression a) (Maybe (Type a)) [Expression a] (Maybe (Expression a)) -- ^The last is the optional variadic spread parameter
                  | Name a (Id a)
                  | Qualified a (Id a) (Id a)
                  | BinaryExpr a BinaryOp (Expression a) (Expression a)
                  | UnaryExpr a UnaryOp (Expression a)
                  | Conversion a (Type a) (Expression a) -- ^Type casts (conversions)
                  | FieldSelector a (Expression a) (Id a)
                  | IndexExpr a (Expression a) (Expression a)
                  | SliceExpr a (Expression a) (Maybe (Expression a)) (Maybe (Expression a)) (Maybe (Expression a))
                  | TypeAssertion a (Expression a) (Type a)
  deriving (Show, Data, Typeable, Functor)

-- | Elements of container literals
data Element a = Element a (Expression a)
               | KeyedEl a (Key a) (Expression a)
  deriving (Show, Data, Typeable, Functor)

data Key a = FieldKey a (Id a)
           | ExprKey a (Expression a)
  deriving (Show, Data, Typeable, Functor)

-- | Binary operators
data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
              | BitwiseAnd | BitwiseNAnd | BitwiseOr | BitwiseXOr
              | LeftShift  | RightShift
              | LogicalAnd | LogicalOr
              | Equals | NotEquals | Less | Greater | LessEq | GreaterEq
  deriving (Show, Data, Typeable)

-- | Unary operators
data UnaryOp = Plus | Minus | Not | Complement | Receive | Address | Deref
  deriving (Show, Data, Typeable)

deriving instance Data SourceRange
deriving instance Typeable SourceRange
deriving instance Data SourcePos
deriving instance Typeable SourcePos

data SemanticType = Int (Maybe Int) {- ^ Bitwidth. Architecture-dependent if `Nothing` -} Bool {- ^ Signed? -}
               | Boolean
               | Float (Maybe Int) {- ^ Bitwidth, if nothing it's an "untyped constant" -}
               | Complex (Maybe Int)  {- ^ Bitwidth, if nothing it's an "untyped constant" -}
               | Iota
               | Nil
               | String
               | Function (Maybe SemanticType) -- ^ Method receiver type
                 [SemanticType] -- ^ Parameter types
                 (Maybe  SemanticType) -- ^ Spread parameter
                 [SemanticType] -- ^ Return types
               | Array (Maybe (Expression (Maybe Binding))) SemanticType
               | Struct (Map Text (SemanticType, Maybe Text))
               | Pointer SemanticType
               | Interface (Map Text SemanticType)
               | Map SemanticType SemanticType
               | Slice SemanticType
               | Channel (ChannelDirection (Maybe Binding)) SemanticType
               | Alias (TypeName (Maybe Binding))
               | Tuple [SemanticType]
               | BuiltIn Text
  deriving (Data, Typeable, Show)

data BindingKind = TypeB SemanticType
                 | VarB SemanticType
                 | ConstB SemanticType
                 | PackageB (Maybe Text) (HashMap Text Binding)
                 | FieldOrMethodB SemanticType
  deriving (Data, Typeable, Show)

data Binding = Binding {_bindingDeclLoc :: SourceRange
                       ,_bindingKind :: BindingKind
                       ,_bindingImported :: Bool
                       ,_bindingThisScope :: Bool
                       }
  deriving (Data, Typeable, Show)

makeLenses ''Binding

makeAnnotationLenses [''Package
                     ,''File
                     ,''ImportDecl
                     ,''ImportSpec
                     ,''Statement
                     ,''ExprClause
                     ,''TypeSwitchGuard
                     ,''TypeClause
                     ,''ForClause
                     ,''CommClause
                     ,''CommOp
                     ,''ConstSpec
                     ,''VarSpec
                     ,''TypeSpec
                     ,''Type
                     ,''TypeName
                     ,''ParameterList
                     ,''ReturnList
                     ,''NamedParameter
                     ,''Id
                     ,''Label
                     ,''FieldDecl
                     ,''Tag
                     ,''MethodSpec
                     ,''Receiver
                     ,''Expression
                     ,''Element
                     ,''Key
                     ,''ChannelDirection
                     ,''TopLevel
                     ,''Declaration
                     ]

