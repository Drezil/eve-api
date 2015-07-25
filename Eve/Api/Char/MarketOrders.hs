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
  Order <$> (liftM unpack (acc ^. attribute "orderID") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "charID") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "stationID") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "volEntered") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "volRemaining") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "minVolume") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "orderState") >>= readMay <&> toEnum)
        <*> (liftM unpack (acc ^. attribute "typeID") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "range") >>= readMay <&> toEnum)
        <*> (liftM unpack (acc ^. attribute "accountKey") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "duration") >>= readMay)
        <*> (liftM (P.filter (/= '.') . unpack) (acc ^. attribute "escrow") >>= readMay)
        <*> (liftM (P.filter (/= '.') . unpack) (acc ^. attribute "price") >>= readMay)
        <*> (liftM unpack (acc ^. attribute "bid") >>= readMay <&> toEnum)
        <*> (liftM unpack (acc ^. attribute "issued") >>= parseTime)
         -- <&> == flip fmap, infixed

