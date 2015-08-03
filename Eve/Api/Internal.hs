-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Internal
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

module Eve.Api.Internal where

import Eve.Api.Types
import Control.Lens.Getter
import Control.Monad
import Network.HTTP.Conduit as HTTP
import Text.XML.Lens
import Text.XML (parseText,def)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text as T
import Control.Exception
import Data.Time
import Data.Time.Clock
import Data.List
import Safe

import Debug.Trace as Debug


callAPIwithChar :: (HasInfo k, HasCharacter k) => Manager -> k -> String -> IO (Either SomeException Document)
callAPIwithChar man k endpoint = callAPIwithCharExtra man k endpoint ""

callAPIwithCharExtra :: (HasInfo k, HasCharacter k) => Manager -> k -> String -> String -> IO (Either SomeException Document)
callAPIwithCharExtra man k endpoint extra = do
    let KeyId key = k ^. keyId
        VCode code = k ^. vCode
        CharacterId cid = k ^. charId
    url <- parseUrl $ "https://api.eveonline.com"++endpoint++"?keyID="++ show key ++"&vCode="++ T.unpack code ++"&characterID="++ show cid ++ extra
    response <- httpLbs url man
    return . parseText def . TL.pack . BL.unpack . responseBody $ response
  `catch` (\e -> Debug.trace (show e) (return (Left e)))

-- | Parser for Timestamps in the XML-API (Format: "%F %X" or "YYYY-MM-DD hh:mm:ss")
parseTime :: ParseTime a => String -> Maybe a
parseTime = parseTimeM True defaultTimeLocale "%F %X"

-- | Gets an Attribute out of an XML-Element
getAttr                 :: Read a => Element -> Name -> Maybe a
getAttr acc             = getAttrMod acc id

-- | Gets an Attribute out of an XML-Element and applys modification before parsing
getAttrMod              :: Read a => Element -> (String -> String) -> Name -> Maybe a
getAttrMod acc mod attr = liftM (mod . unpack) (acc ^. attribute attr) >>= readMay

-- | Gets an Attribute out of an XML-Element and turns it into an enum
getAttrEnum             :: Enum a => Element -> Name -> Maybe a
getAttrEnum acc attr    = liftM unpack (acc ^. attribute attr) >>= readMay <&> toEnum

-- | Gets a Time out of an XML with the use of parseTime
getTime                 :: ParseTime a => Element -> Name -> Maybe a
getTime acc attr        = liftM unpack (acc ^. attribute attr) >>= Eve.Api.Internal.parseTime

-- | Helper-Function to extract the cached-until out of an xml-tree
getCachedUntil :: Document -> Maybe UTCTime
getCachedUntil xml = headMay (xml ^.. root . el "eveapi" ./ el "currentTime") >>= extractTime
    where
      extractTime :: Element -> Maybe UTCTime
      extractTime el = Eve.Api.Internal.parseTime . T.unpack $ el ^. text

-- | Sorts in descending order
sortByDescending :: (a -> a -> Ordering) -> [a] -> [a]
sortByDescending cmp = sortBy (flip cmp)
