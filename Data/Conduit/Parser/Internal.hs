{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
module Data.Conduit.Parser.Internal (module Data.Conduit.Parser.Internal) where

-- {{{ Imports
import qualified Conduit

import           Control.Applicative
import           Control.Exception.Safe    as Exception
import           Control.Monad
import           Control.Monad.Except

import           Data.Conduit              hiding (leftover)
import qualified Data.Conduit.List         as Conduit

import           Text.Parser.Combinators   as Parser

import Text.Parsec hiding ((<?>))
-- }}}
--

data CStream i = CStream

instance Monad m => Stream (CStream i) (ConduitT i o m) i where
  uncons x = fmap (,x) <$> Conduit.await

newtype ConduitParser i m a = ConduitParser (ParsecT (CStream i) () (ConduitT i Void m) a)
  deriving (Monad,Functor,Applicative, Alternative,MonadFail, MonadPlus, MonadIO, Parsing)

instance MonadTrans (ConduitParser i) where
  lift x = ConduitParser (lift (lift x))

runConduitParser :: MonadThrow m => ConduitParser i m a -> ConduitT i Void m a
runConduitParser (ConduitParser p) = either (throwM . ConduitParserException) return =<< runParserT p () "lsp-test" CStream

data ConduitParserException = ConduitParserException ParseError

deriving instance Eq ConduitParserException
deriving instance Show ConduitParserException
instance Exception ConduitParserException

named :: (Monad m, Show i) => String -> ConduitParser i m a -> ConduitParser i m a
named = flip (<?>)

await :: (Monad m, Show i) => ConduitParser i m i
await = ConduitParser $ tokenPrim show (\p _ _ -> incSourceLine p 1) Just

peek :: (Monad m) => ConduitParser i m (Maybe i)
peek = ConduitParser $ lift Conduit.peek

leftover :: i -> ConduitParser i m ()
leftover = ConduitParser . lift . Conduit.leftover
