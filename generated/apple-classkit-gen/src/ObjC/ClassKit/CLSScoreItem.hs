{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CLSScoreItem represents user generated score information.
--
-- Generated bindings for @CLSScoreItem@.
module ObjC.ClassKit.CLSScoreItem
  ( CLSScoreItem
  , IsCLSScoreItem(..)
  , initWithIdentifier_title_score_maxScore
  , score
  , setScore
  , maxScore
  , setMaxScore
  , initWithIdentifier_title_score_maxScoreSelector
  , maxScoreSelector
  , scoreSelector
  , setMaxScoreSelector
  , setScoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a score item with identifiers, title, score and maximum score.
--
-- @identifier@ — An identifier that is unique within activity.
--
-- @title@ — Title of score. Ex /Biology-/ Cellular Division Quiz
--
-- @score@ — The score the user received.
--
-- @maxScore@ — The maximum score possible.
--
-- ObjC selector: @- initWithIdentifier:title:score:maxScore:@
initWithIdentifier_title_score_maxScore :: (IsCLSScoreItem clsScoreItem, IsNSString identifier, IsNSString title) => clsScoreItem -> identifier -> title -> CDouble -> CDouble -> IO (Id CLSScoreItem)
initWithIdentifier_title_score_maxScore clsScoreItem identifier title score maxScore =
  sendOwnedMessage clsScoreItem initWithIdentifier_title_score_maxScoreSelector (toNSString identifier) (toNSString title) score maxScore

-- | Score out of @maxScore.@
--
-- Should be between zero and @maxScore@ [0.0,maxScore].
--
-- ObjC selector: @- score@
score :: IsCLSScoreItem clsScoreItem => clsScoreItem -> IO CDouble
score clsScoreItem =
  sendMessage clsScoreItem scoreSelector

-- | Score out of @maxScore.@
--
-- Should be between zero and @maxScore@ [0.0,maxScore].
--
-- ObjC selector: @- setScore:@
setScore :: IsCLSScoreItem clsScoreItem => clsScoreItem -> CDouble -> IO ()
setScore clsScoreItem value =
  sendMessage clsScoreItem setScoreSelector value

-- | Total score possible.
--
-- Must be greater than zero.
--
-- ObjC selector: @- maxScore@
maxScore :: IsCLSScoreItem clsScoreItem => clsScoreItem -> IO CDouble
maxScore clsScoreItem =
  sendMessage clsScoreItem maxScoreSelector

-- | Total score possible.
--
-- Must be greater than zero.
--
-- ObjC selector: @- setMaxScore:@
setMaxScore :: IsCLSScoreItem clsScoreItem => clsScoreItem -> CDouble -> IO ()
setMaxScore clsScoreItem value =
  sendMessage clsScoreItem setMaxScoreSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:score:maxScore:@
initWithIdentifier_title_score_maxScoreSelector :: Selector '[Id NSString, Id NSString, CDouble, CDouble] (Id CLSScoreItem)
initWithIdentifier_title_score_maxScoreSelector = mkSelector "initWithIdentifier:title:score:maxScore:"

-- | @Selector@ for @score@
scoreSelector :: Selector '[] CDouble
scoreSelector = mkSelector "score"

-- | @Selector@ for @setScore:@
setScoreSelector :: Selector '[CDouble] ()
setScoreSelector = mkSelector "setScore:"

-- | @Selector@ for @maxScore@
maxScoreSelector :: Selector '[] CDouble
maxScoreSelector = mkSelector "maxScore"

-- | @Selector@ for @setMaxScore:@
setMaxScoreSelector :: Selector '[CDouble] ()
setMaxScoreSelector = mkSelector "setMaxScore:"

