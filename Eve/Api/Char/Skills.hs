-----------------------------------------------------------------------------
--
-- Module      :  Eve.Api.Char.Skills
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

module Eve.Api.Char.Skills where

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

data Skill = Skill
             { _typeId :: Int64
             , _skillpoints :: Int64
             , _level :: Int
             , _published :: Bool
             } deriving (Show,Eq)

getSkills :: (HasInfo k, HasCharacter k) => Manager -> k -> IO (QueryResult [Skill])
getSkills man k = do
    res <- callAPIwithChar man k "/char/CharacterSheet.xml.aspx"
    case res of
      Left _ -> return HTTPError
      Right xml ->
        case mapM getSkill (xml ^.. root .  el "eveapi"
                                         ./ el "result"
                                         ./ el "rowset" . attributeIs "name" "skills"
                                         ./ el "row") of
          Nothing -> return ParseError
          Just acc -> return . pure $ acc

getSkill :: Element -> Maybe Skill
getSkill acc =
  Skill <$> getAttr     acc "typeID"
        <*> getAttr     acc "skillpoints"
        <*> getAttr     acc "level"
        <*> getAttrEnum acc "published"




