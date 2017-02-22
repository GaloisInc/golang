{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}

module Language.Go.Bindings.Types where

import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as M
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Strict as HM
--import Language.Go.Types
import Data.Text (Text)
import AlexTools hiding (range)
import Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import Data.Default.Class
import Data.Graph.Inductive
import Language.Go.AST
import Lens.Simple
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Int

data ValueType = Int (Maybe Int32) {- ^ Bitwidth. Architecture-dependent if `Nothing` -} Bool {- ^ Signed? -}
               | Boolean
               | Float Int32 {- ^ Bitwidth -}
               | Complex Int32 {- ^ Bitwidth -}
               | Iota
               | Nil
               | String
               | Function (Maybe ValueType) -- ^ Method receiver type
                 [ValueType] -- ^ Parameter types
                 (Maybe  ValueType) -- ^ Spread parameter
                 [ValueType] -- ^ Return types
               | Array (Maybe (Expression (Maybe Binding))) ValueType
               | Struct (Map Text (ValueType, Maybe Text))
               | Pointer ValueType
               | Interface (Map Text ValueType)
               | Map ValueType ValueType
               | Slice ValueType
               | Channel (ChannelDirection (Maybe Binding)) ValueType
               | BuiltIn Text -- ^ built-in function
               | Alias (TypeName (Maybe Binding))
               | Tuple [ValueType]
  deriving (Data, Typeable, Show)

data BindingKind = TypeB ValueType
                 | VarB ValueType
                 | ConstB ValueType
                 | PackageB (Maybe Text) (Map Text Binding)
                 | FieldOrMethodB ValueType
  deriving (Data, Typeable, Show)

data Binding = Binding {_bindingDeclLoc :: SourceRange
                       ,_bindingKind :: BindingKind
                       ,_bindingImported :: Bool
                       ,_bindingThisScope :: Bool
                       }
  deriving (Data, Typeable, Show)

type Scope    = HashMap Text Binding
type Bindings = NonEmpty Scope

data TypeBindingGraph a = TBG {_tbGraph :: Gr (Type a, Bool {- root? -}, Bool {- resolved? -}) Bool {- pointer? -}
                              ,_tbgRoots :: HashMap Text Node}

instance Default (TypeBindingGraph a) where
  def = TBG empty HM.empty

makeLenses ''Binding
makeLenses ''TypeBindingGraph
