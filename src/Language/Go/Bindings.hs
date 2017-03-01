{-# LANGUAGE OverloadedStrings, ExistentialQuantification, RankNTypes, FlexibleContexts #-}

-- | Identifier bindings and operations on them
module Language.Go.Bindings where

import Language.Go.Types
import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Strict as HM
import AlexTools hiding (range)
import Lens.Simple hiding ((&))
import Control.Arrow
import Language.Go.Parser.Util
import Language.Go.AST (Id(..), Type(..), TypeName(..))
import Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Default.Class
import Control.Applicative
import Data.Graph.Inductive hiding (mkNode)
import Data.Graph.Inductive.Query.DFS
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data
import Data.List (nub)
import Control.Monad.State
import Language.Go.Bindings.Types
import Data.Tree
import Data.Graph.Inductive.Basic
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe (fromJust)

defaultBindings = predeclaredBindings :| []

declareBindingId :: Id SourceRange -> BindingKind -> Bindings -> Bindings
declareBindingId (BlankId _) _ = id
declareBindingId id@(Id _ _ ident) bk = declareBinding ident id bk

declareBinding :: Ranged r => Text -> r -> BindingKind -> Bindings -> Bindings
declareBinding ident r bk (b :| bs) = HM.insert ident (Binding (range r) bk False True) b :| bs

lookupBinding :: Text -> Bindings -> Maybe BindingKind
lookupBinding id (b :| _) = _bindingKind <$> HM.lookup id b

pushScope :: Bindings -> Bindings
pushScope (b :| rest) = (HM.map notThisContext b) :| b:rest
  where notThisContext bind = bind {_bindingThisScope = False}

popScope :: Bindings -> Maybe Bindings
popScope = snd . NE.uncons
                         
predeclaredBindings :: HashMap Text Binding
predeclaredBindings = HM.fromList $ map (second $ \k -> Binding fakeRange k True True) $  [("bool", TypeB Boolean)
  ,("uint8", TypeB $ Int (Just 8) False)
  ,("uint16", TypeB $ Int (Just 16) False)
  ,("uint32", TypeB $ Int (Just 32) False)
  ,("uint64", TypeB $ Int (Just 64) False)
  ,("int8", TypeB $ Int (Just 8) True)
  ,("int16", TypeB $ Int (Just 16) True)
  ,("int32", TypeB $ Int (Just 32) True)
  ,("int64", TypeB $ Int (Just 64) True)
  ,("float32", TypeB $ Float 32)
  ,("float64", TypeB $ Float 64)
  ,("complex64", TypeB $ Complex 64)
  ,("complex128", TypeB $ Complex 128)
  ,("byte", TypeB $ Int (Just 8) False)
  ,("rune", TypeB $ Int (Just 32) True)
  ,("uint", TypeB $ Int (Just 32) False)
  ,("int", TypeB $ Int Nothing True)
  ,("uintptr", TypeB $ Int (Just 64) False)
  ,("string", TypeB String)
  ,("iota", ConstB Iota)
  ,("nil", ConstB Nil)
  ,("true", ConstB Boolean)
  ,("false", ConstB Boolean)
  ]
  ++ map (id &&& (ConstB . BuiltIn))
  ["append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make",
   "new", "panic", "print", "println", "real", "recover"
  ]
