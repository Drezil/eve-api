-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Char.AccountBalance
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

module Eve.Api.Char.AccountBalance (
    Account,
    accountId,
    accountKey,
    centbalance,
    getAccountBalance
) where

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
        case do
            b <- headMay (xml ^.. root . el "eveapi" ./ el "result" ./ el "rowset" ./ el "row") >>= getAccount
            t <- getCachedUntil xml
            return (b,t)
          of
          Nothing -> return ParseError
          Just (acc,t) -> return . QueryResult t $ acc

getAccount :: Element -> Maybe Account
getAccount acc =
  Account <$> getAttr    acc                     "accountID"
          <*> getAttr    acc                     "accountKey"
          <*> getAttrMod acc (P.filter (/= '.')) "balance"
