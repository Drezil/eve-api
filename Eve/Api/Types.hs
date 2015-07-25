-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Types
-- Copyright   :
-- License     :  GPL-2
--
-- Maintainer  :  sdressel@techfak.uni-bielefeld.de
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Eve.Api.Types where

import Data.Int (Int64)
import Data.Text
import Control.Lens.Getter
import Generics.Deriving hiding (to)


data CharacterId = CharacterId
                 { _charId :: Int64
                 } deriving (Eq,Show,Generic)

data VCode       = VCode Text
                   deriving (Eq,Show,Generic)

data KeyId       = KeyId Int64
                   deriving (Eq,Show,Generic)

data ApiKey      = ApiKey
                 { _vCode :: VCode
                 , _keyId :: KeyId
                 } deriving (Show, Eq)

data ApiComplete = ApiComplete
                 { _apiKey :: ApiKey
                 , _characterId ::CharacterId
                 } deriving (Show, Eq)

class HasCharacter t where
  charId :: Getter t CharacterId

instance HasCharacter CharacterId where
  charId = id

class HasInfo t where
  vCode :: Getter t VCode
  keyId :: Getter t KeyId

instance HasInfo ApiKey where
  vCode = to _vCode
  keyId = to _keyId

instance HasInfo ApiComplete where
  vCode = to _apiKey . vCode
  keyId = to _apiKey . keyId

instance HasCharacter ApiComplete where
  charId = to _characterId

data QueryResult a = QueryResult a
                   | HTTPError
                   | ParseError
                   deriving (Show,Eq)

instance Functor QueryResult where
  fmap f (QueryResult a) = QueryResult (f a)
  fmap _ HTTPError = HTTPError
  fmap _ ParseError  = ParseError

instance Applicative QueryResult where
  pure = QueryResult
  (QueryResult f) <*> a = f <$> a
  HTTPError <*> _ = HTTPError
  ParseError <*> _ = ParseError

mkComplete :: Int64 -> Text -> Int64 -> ApiComplete
mkComplete k v c = ApiComplete (mkKey k v) (CharacterId c)

mkKey :: Int64 -> Text -> ApiKey
mkKey k v = ApiKey (VCode v) (KeyId k)
