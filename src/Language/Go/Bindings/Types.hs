{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}

module Language.Go.Bindings.Types where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Strict as HM
import Language.Go.Types
import Data.Text (Text)
import AlexTools hiding (range)
import Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import Data.Default.Class
import Data.Graph.Inductive
import Language.Go.AST
import Lens.Simple
import Data.Data (Data)
import Data.Typeable (Typeable)

data BindingKind = TypeB ValueType
                 | VarB ValueType
                 | ConstB ValueType
                 | PackageB (Maybe Text) (HashMap Text Binding)
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
