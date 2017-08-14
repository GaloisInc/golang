{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveDataTypeable, OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
-- | Utilities for the first stage (ambiguous) parser
module Language.Go.Parser.Util where

import Language.Go.AST
import Language.Go.Parser.Token
import AlexTools
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Lens.Simple
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Monad.Error.Class
import Data.Text (Text)
import Data.Default.Class

unexpected :: (Ranged a, MonadError (SourceRange,String) m)
           => a -> String -> m b
unexpected a msg = throwError (fromMaybe fakeRange (getRange a), msg)

fakePos = SourcePos (-1)(-1)(-1)
fakeRange = SourceRange "<internal>" fakePos fakePos

pos :: Ranged a => a -> (SourceRange -> b) -> b
pos a ctor = ctor $ fromMaybe fakeRange $ getRange a

repos :: (Ranged p, Annotated x) => p -> x SourceRange ->  x SourceRange
repos p t = t&ann .~ fromMaybe fakeRange (getRange p)

consMaybe :: Maybe a -> [a] -> [a]
consMaybe Nothing as = as
consMaybe (Just a) as = a:as

range :: Ranged a => a -> SourceRange
range = fromMaybe fakeRange . getRange

class Ranged a where
  getRange :: a -> Maybe SourceRange

instance Ranged SourceRange where
  getRange = Just

instance Ranged (Lexeme a) where
  getRange = Just . AlexTools.range

instance Ranged a => Ranged (Maybe a) where
  getRange x = getRange =<< x

instance (Ranged a, Ranged b) => Ranged (a,b) where
  getRange (x,y) =
    case (getRange x, getRange y) of
      (Nothing,Nothing) -> Nothing
      (Just a, Nothing) -> Just a
      (Nothing, Just a) -> Just a
      (Just a, Just b)  ->
        Just $! SourceRange { sourceFile = sourceFile a, sourceFrom = sourceFrom a, sourceTo = sourceTo b }

instance Ranged () where
  getRange () = Nothing

instance Ranged a => Ranged [a] where
  getRange (x : xs) = getRange (x,xs)
  getRange []       = Nothing

instance Ranged a => Ranged (NonEmpty a) where
  getRange (x :| xs) = getRange (x, xs)

instance Ranged (Id SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Expression SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Statement SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Type SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (ConstSpec SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (NamedParameter SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (ParameterList SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Tag SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (TypeName SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (ReturnList SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (FieldDecl SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (ImportSpec SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Package SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (File SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (VarSpec SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (TypeSpec SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (ChannelDirection SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Key SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Label SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (CommOp SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (TopLevel SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (Declaration SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged (MethodSpec SourceRange) where
  getRange x = Just $ x^.ann

instance Ranged Binding where
  getRange b = Just $ b^.bindingDeclLoc
