{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Char.WalletTransactions
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

module Eve.Api.Char.WalletTransactions where

import Control.Lens.TH
import Control.Lens.Operators
import Control.Monad
import Data.Int
import Data.Text as T
import Data.Time.Clock
import Network.HTTP.Conduit as HTTP
import Prelude as P
import Text.XML.Lens
import Safe

import Eve.Api.Types
import Eve.Api.Internal

import Debug.Trace as Debug

data Transaction = Transaction
              { _transactionDateTime :: UTCTime
              , _transactionId :: Int64
              , _quantity :: Int64
              , _typeName :: Text
              , _typeId :: Int64
              , _pricecents :: Integer
              , _clientId :: Int64
              , _clientName :: Text
              , _stationId :: Int64
              , _stationName :: Text
              , _transactionType :: TransactionType
              , _transactionFor :: TransactionFor
              , _journalTransactionId :: Int64
              } deriving (Show, Eq)

instance Ord Transaction where
  compare t1 t2 = _transactionId t1 `compare` _transactionId t2

data TransactionType = Buy | Sell deriving (Eq)

instance Read TransactionType where
  readsPrec _ s = case s of
                  ('b':'u':'y':xs) -> [(Buy,xs)]
                  ('s':'e':'l':'l':xs) -> [(Sell,xs)]
                  _ -> []

instance Show TransactionType where
  show Buy  = "buy"
  show Sell = "sell"

data TransactionFor = Personal | Corporation deriving (Eq)

instance Read TransactionFor where
  readsPrec _ s = case s of
                  ('p':'e':'r':'s':'o':'n':'a':'l':xs) -> [(Personal,xs)]
                  ('c':'o':'r':'p':'o':'r':'a':'t':'i':'o':'n':xs) -> [(Corporation, xs)]
                  _ -> []

instance Show TransactionFor where
  show Personal    = "personal"
  show Corporation = "corporation"

getWalletTransactions :: (HasInfo k, HasCharacter k) => Manager -> k -> IO (QueryResult [Transaction])
getWalletTransactions man k = getWalletTransactionsPart man k Nothing

getWalletTransactionsBackTo :: (HasInfo k, HasCharacter k) => Manager -> k -> Int64 -> IO (QueryResult [Transaction])
getWalletTransactionsBackTo man k tid = getWalletTransactionsBackTo' man k tid Nothing
  where
    getWalletTransactionsBackTo' :: (HasInfo k, HasCharacter k) => Manager -> k -> Int64 -> Maybe Int64 -> IO (QueryResult [Transaction])
    getWalletTransactionsBackTo' man k tid cid = do
        res <- case cid of
                 Nothing -> getWalletTransactionsPart man k Nothing
                 Just a -> getWalletTransactionsPart man k (Just a)
        case res of
          QueryResult l -> let (t1,l1,t2) = spanFind (\t -> _transactionId t > tid) l in
                           case t2 of
                           [] -> do
                                 res' <- getWalletTransactionsBackTo' man k tid (Just $ _transactionId l1)
                                 case res' of
                                   QueryResult l' -> return $ QueryResult (l ++ l')
                                   a -> return a
                           _  -> return $ QueryResult t1
          a -> return a


spanFind :: (a -> Bool) -> [a] -> ([a],a,[a])
spanFind p [x]    = if p x then ([x],x,[])
                        else ([],x,[x])
spanFind p (x:xs) = if p x then (x:a,b,c)
                           else ([],x,x:xs)
                    where (a,b,c) = spanFind p xs

getWalletTransactionsPart :: (HasInfo k, HasCharacter k) => Manager -> k -> Maybe Int64 -> IO (QueryResult [Transaction])
getWalletTransactionsPart man k tid = do
    res <- case tid of
             Nothing -> callAPIwithChar man k "/char/WalletTransactions.xml.aspx"
             Just transid -> callAPIwithCharExtra man k "/char/WalletTransactions.xml.aspx" ("&fromID="++ show transid)
    case res of
      Left _ -> return HTTPError
      Right xml ->
        case mapM getTransaction (xml ^.. root .  el "eveapi"
                                               ./ el "result"
                                               ./ el "rowset"
                                               ./ el "row")
        of
          Nothing -> return ParseError
          Just ret -> return . pure . sortByDescending compare $ ret



getTransaction :: Element -> Maybe Transaction
getTransaction acc = Debug.trace (show acc) $
  Transaction <$> getTime acc "transactionDateTime"
              <*> getAttr acc "transactionID"
              <*> getAttr acc "quantity"
              <*> acc ^. attribute "typeName"
              <*> getAttr acc "typeID"
              <*> getAttrMod acc (P.filter (/= '.')) "price"
              <*> getAttr acc "clientID"
              <*> acc ^. attribute "clientName"
              <*> getAttr acc "stationID"
              <*> acc ^. attribute "stationName"
              <*> getAttr acc "transactionType"
              <*> getAttr acc "transactionFor"
              <*> getAttr acc "journalTransactionID"

