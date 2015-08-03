-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Char.MarketOrders
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

module Eve.Api.Char.MarketOrders (
    OrderState(..),
    Range(..),
    Bid(..),
    Order(..),
    getMarketOrders
) where

import Control.Lens.TH
import Control.Lens.Operators
import Data.Int
import Data.Text as T
import Data.Time.Clock
import Network.HTTP.Conduit as HTTP
import Prelude as P
import Text.XML.Lens

import Eve.Api.Types
import Eve.Api.Internal

import Debug.Trace as Debug

data OrderState = Open
                | Closed
                | Fulfilled
                | Cancelled
                | Pending
                | CharacterDeleted
                deriving (Show, Eq, Enum)

data Range = Station
           | SolarSystem
           | Jumps5
           | Jumps10
           | Jumps20
           | Jumps40
           | Region
           deriving (Show, Eq)

instance Enum Range where
  toEnum (-1) = Station
  toEnum 0  = SolarSystem
  toEnum 5  = Jumps5
  toEnum 10 = Jumps10
  toEnum 20 = Jumps20
  toEnum 40 = Jumps40
  toEnum 32767 = Region
  toEnum a = error ("Range: "++show a)

  fromEnum Station = -1
  fromEnum SolarSystem = 0
  fromEnum Jumps5 = 5
  fromEnum Jumps10 = 10
  fromEnum Jumps20 = 20
  fromEnum Jumps40 = 40
  fromEnum Region = 32767

data Bid = Sell
         | Buy
         deriving (Show,Eq,Enum)



data Order = Order
           { _orderId :: Int64
           , _charId :: Int64
           , _stationId :: Int64
           , _volEntered :: Int64
           , _volRemaining :: Int64
           , _minVolume :: Int64
           , _orderState :: OrderState
           , _typeId :: Int64
           , _range :: Range
           , _accountKey :: Int
           , _duration :: Int
           , _escrowcents :: Integer
           , _pricecents :: Integer
           , _bid :: Bid
           , _issued :: UTCTime
           } deriving (Show, Eq)

$(makeLenses ''Order)

getMarketOrders :: (HasInfo k, HasCharacter k) => Manager -> k -> IO (QueryResult [Order])
getMarketOrders man k = do
    res <- callAPIwithChar man k "/char/MarketOrders.xml.aspx"
    case res of
      Left _ -> return HTTPError
      Right xml ->
        case mapM getOrder (xml ^.. root . el "eveapi" ./ el "result" ./ el "rowset" ./ el "row") of
          Nothing -> return ParseError
          Just acc -> return . pure $ acc

getOrder :: Element -> Maybe Order
getOrder acc =
  Order <$> getAttr     acc "orderID"
        <*> getAttr     acc "charID"
        <*> getAttr     acc "stationID"
        <*> getAttr     acc "volEntered"
        <*> getAttr     acc "volRemaining"
        <*> getAttr     acc "minVolume"
        <*> getAttrEnum acc "orderState"
        <*> getAttr     acc "typeID"
        <*> getAttrEnum acc "range"
        <*> getAttr     acc "accountKey"
        <*> getAttr     acc "duration"
        <*> getAttrMod  acc (P.filter (/= '.')) "escrow"
        <*> getAttrMod  acc (P.filter (/= '.')) "price"
        <*> getAttrEnum acc "bid"
        <*> getTime     acc "issued"
