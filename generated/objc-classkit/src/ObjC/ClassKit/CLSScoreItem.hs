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
  , scoreSelector
  , setScoreSelector
  , maxScoreSelector
  , setMaxScoreSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
initWithIdentifier_title_score_maxScore clsScoreItem  identifier title score maxScore =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
      sendMsg clsScoreItem (mkSelector "initWithIdentifier:title:score:maxScore:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCDouble (fromIntegral score), argCDouble (fromIntegral maxScore)] >>= ownedObject . castPtr

-- | Score out of @maxScore.@
--
-- Should be between zero and @maxScore@ [0.0,maxScore].
--
-- ObjC selector: @- score@
score :: IsCLSScoreItem clsScoreItem => clsScoreItem -> IO CDouble
score clsScoreItem  =
  sendMsg clsScoreItem (mkSelector "score") retCDouble []

-- | Score out of @maxScore.@
--
-- Should be between zero and @maxScore@ [0.0,maxScore].
--
-- ObjC selector: @- setScore:@
setScore :: IsCLSScoreItem clsScoreItem => clsScoreItem -> CDouble -> IO ()
setScore clsScoreItem  value =
  sendMsg clsScoreItem (mkSelector "setScore:") retVoid [argCDouble (fromIntegral value)]

-- | Total score possible.
--
-- Must be greater than zero.
--
-- ObjC selector: @- maxScore@
maxScore :: IsCLSScoreItem clsScoreItem => clsScoreItem -> IO CDouble
maxScore clsScoreItem  =
  sendMsg clsScoreItem (mkSelector "maxScore") retCDouble []

-- | Total score possible.
--
-- Must be greater than zero.
--
-- ObjC selector: @- setMaxScore:@
setMaxScore :: IsCLSScoreItem clsScoreItem => clsScoreItem -> CDouble -> IO ()
setMaxScore clsScoreItem  value =
  sendMsg clsScoreItem (mkSelector "setMaxScore:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:score:maxScore:@
initWithIdentifier_title_score_maxScoreSelector :: Selector
initWithIdentifier_title_score_maxScoreSelector = mkSelector "initWithIdentifier:title:score:maxScore:"

-- | @Selector@ for @score@
scoreSelector :: Selector
scoreSelector = mkSelector "score"

-- | @Selector@ for @setScore:@
setScoreSelector :: Selector
setScoreSelector = mkSelector "setScore:"

-- | @Selector@ for @maxScore@
maxScoreSelector :: Selector
maxScoreSelector = mkSelector "maxScore"

-- | @Selector@ for @setMaxScore:@
setMaxScoreSelector :: Selector
setMaxScoreSelector = mkSelector "setMaxScore:"

