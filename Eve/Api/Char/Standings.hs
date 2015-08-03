-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Char.Standings
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

module Eve.Api.Char.Standings (
    Standing(..),
    AgentStandings,
    CorpStandings,
    FactionStandings,
    getStandings
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

data Standing = Standing
              { _objectId :: Int64
              , _standingName :: Text
              , _standingStanding :: Double
              } deriving (Show, Eq)

type AgentStandings = [Standing]
type CorpStandings = [Standing]
type FactionStandings = [Standing]

getStandings :: (HasInfo k, HasCharacter k) => Manager -> k -> IO (QueryResult (AgentStandings, CorpStandings, FactionStandings))
getStandings man k = do
    res <- callAPIwithChar man k "/char/Standings.xml.aspx"
    case res of
      Left _ -> return HTTPError
      Right xml ->
        case do
               agent   <- mapM getStanding (xml ^.. root .  el "eveapi"
                                                         ./ el "result"
                                                         ./ el "characterNPCStandings"
                                                         ./ el "rowset" . attributeIs "name" "agents"
                                                         ./ el "row")
               corp    <- mapM getStanding (xml ^.. root .  el "eveapi"
                                                         ./ el "result"
                                                         ./ el "characterNPCStandings"
                                                         ./ el "rowset" . attributeIs "name" "NPCCorporations"
                                                         ./ el "row")
               faction <- mapM getStanding (xml ^.. root .  el "eveapi"
                                                         ./ el "result"
                                                         ./ el "characterNPCStandings"
                                                         ./ el "rowset" . attributeIs "name" "factions"
                                                         ./ el "row")
               t <- getCachedUntil xml
               return (agent,corp,faction,t)
        of
          Nothing -> return ParseError
          Just (a,c,f,t) -> return . QueryResult t $ (a,c,f)

getStanding :: Element -> Maybe Standing
getStanding acc = Debug.trace (show acc) $
  Standing <$> (liftM unpack (acc ^. attribute "fromID") >>= readMay)
           <*> acc ^. attribute "fromName"
           <*> (liftM unpack (acc ^. attribute "standing") >>= readMay)
