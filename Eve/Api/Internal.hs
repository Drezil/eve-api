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
import Network.HTTP.Conduit as HTTP
import Text.XML.Lens
import Text.XML (parseText,def)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text as T
import Control.Exception

import Debug.Trace as Debug


callAPIwithChar :: (HasInfo k, HasCharacter k) => Manager -> k -> String -> IO (Either SomeException Document)
callAPIwithChar man k endpoint = do
    let KeyId key = k ^. keyId
        VCode code = k ^. vCode
        CharacterId cid = k ^. charId
    url <- parseUrl $ "https://api.eveonline.com"++endpoint++"?keyID="++ show key ++"&vCode="++ T.unpack code ++"&characterID="++ show cid
    response <- httpLbs url man
    return . parseText def . TL.pack . BL.unpack . responseBody $ response
  `catch` (\e -> Debug.trace (show e) (return (Left e)))
