-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Char
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

module Eve.Api.Char where

import Control.Lens.TH
import Control.Monad
import Data.Int
import Data.Text as T
import Network.HTTP.Conduit as HTTP
import Prelude as P
import Text.XML.Lens
import Safe

import Eve.Api.Types
import Eve.Api.Internal

import Debug.Trace as Debug

data Account = Account
             { _accountId :: Int64
             , _accountKey :: Int64
             , _centbalance :: Integer
             } deriving (Show,Eq)

$(makeLenses ''Account)

getAccountBalance :: (HasInfo k, HasCharacter k) => Manager -> k -> IO (QueryResult Account)
getAccountBalance man k = do
    res <- callAPIwithChar man k "/char/AccountBalance.xml.aspx"
    case res of
      Left _ -> return HTTPError
      Right xml ->
        case headMay (xml ^.. root . el "eveapi" ./ el "result" ./ el "rowset" ./ el "row") >>= getAccount of
          Nothing -> return ParseError
          Just acc -> return . pure $ acc

getAccount :: Element -> Maybe Account
getAccount acc = Debug.trace (show acc) $
  Account <$> (liftM unpack (acc ^. attribute "accountID") >>= readMay)
          <*> (liftM unpack (acc ^. attribute "accountKey") >>= readMay)
          <*> (liftM (P.filter (/= '.') . unpack) (acc ^. attribute "balance") >>= readMay)



