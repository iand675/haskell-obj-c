{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSampleBufferRequest
--
-- An AVSampleBufferRequest describes a CMSampleBuffer creation request.
--
-- Generated bindings for @AVSampleBufferRequest@.
module ObjC.AVFoundation.AVSampleBufferRequest
  ( AVSampleBufferRequest
  , IsAVSampleBufferRequest(..)
  , init_
  , new
  , initWithStartCursor
  , startCursor
  , direction
  , setDirection
  , limitCursor
  , setLimitCursor
  , preferredMinSampleCount
  , setPreferredMinSampleCount
  , maxSampleCount
  , setMaxSampleCount
  , mode
  , setMode
  , directionSelector
  , initSelector
  , initWithStartCursorSelector
  , limitCursorSelector
  , maxSampleCountSelector
  , modeSelector
  , newSelector
  , preferredMinSampleCountSelector
  , setDirectionSelector
  , setLimitCursorSelector
  , setMaxSampleCountSelector
  , setModeSelector
  , setPreferredMinSampleCountSelector
  , startCursorSelector

  -- * Enum types
  , AVSampleBufferRequestDirection(AVSampleBufferRequestDirection)
  , pattern AVSampleBufferRequestDirectionForward
  , pattern AVSampleBufferRequestDirectionNone
  , pattern AVSampleBufferRequestDirectionReverse
  , AVSampleBufferRequestMode(AVSampleBufferRequestMode)
  , pattern AVSampleBufferRequestModeImmediate
  , pattern AVSampleBufferRequestModeScheduled
  , pattern AVSampleBufferRequestModeOpportunistic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO (Id AVSampleBufferRequest)
init_ avSampleBufferRequest =
  sendOwnedMessage avSampleBufferRequest initSelector

-- | @+ new@
new :: IO (Id AVSampleBufferRequest)
new  =
  do
    cls' <- getRequiredClass "AVSampleBufferRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithStartCursor:@
initWithStartCursor :: (IsAVSampleBufferRequest avSampleBufferRequest, IsAVSampleCursor startCursor) => avSampleBufferRequest -> startCursor -> IO (Id AVSampleBufferRequest)
initWithStartCursor avSampleBufferRequest startCursor =
  sendOwnedMessage avSampleBufferRequest initWithStartCursorSelector (toAVSampleCursor startCursor)

-- | @- startCursor@
startCursor :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO (Id AVSampleCursor)
startCursor avSampleBufferRequest =
  sendMessage avSampleBufferRequest startCursorSelector

-- | @- direction@
direction :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO AVSampleBufferRequestDirection
direction avSampleBufferRequest =
  sendMessage avSampleBufferRequest directionSelector

-- | @- setDirection:@
setDirection :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> AVSampleBufferRequestDirection -> IO ()
setDirection avSampleBufferRequest value =
  sendMessage avSampleBufferRequest setDirectionSelector value

-- | @- limitCursor@
limitCursor :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO (Id AVSampleCursor)
limitCursor avSampleBufferRequest =
  sendMessage avSampleBufferRequest limitCursorSelector

-- | @- setLimitCursor:@
setLimitCursor :: (IsAVSampleBufferRequest avSampleBufferRequest, IsAVSampleCursor value) => avSampleBufferRequest -> value -> IO ()
setLimitCursor avSampleBufferRequest value =
  sendMessage avSampleBufferRequest setLimitCursorSelector (toAVSampleCursor value)

-- | @- preferredMinSampleCount@
preferredMinSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO CLong
preferredMinSampleCount avSampleBufferRequest =
  sendMessage avSampleBufferRequest preferredMinSampleCountSelector

-- | @- setPreferredMinSampleCount:@
setPreferredMinSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> CLong -> IO ()
setPreferredMinSampleCount avSampleBufferRequest value =
  sendMessage avSampleBufferRequest setPreferredMinSampleCountSelector value

-- | @- maxSampleCount@
maxSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO CLong
maxSampleCount avSampleBufferRequest =
  sendMessage avSampleBufferRequest maxSampleCountSelector

-- | @- setMaxSampleCount:@
setMaxSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> CLong -> IO ()
setMaxSampleCount avSampleBufferRequest value =
  sendMessage avSampleBufferRequest setMaxSampleCountSelector value

-- | @- mode@
mode :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO AVSampleBufferRequestMode
mode avSampleBufferRequest =
  sendMessage avSampleBufferRequest modeSelector

-- | @- setMode:@
setMode :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> AVSampleBufferRequestMode -> IO ()
setMode avSampleBufferRequest value =
  sendMessage avSampleBufferRequest setModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSampleBufferRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSampleBufferRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithStartCursor:@
initWithStartCursorSelector :: Selector '[Id AVSampleCursor] (Id AVSampleBufferRequest)
initWithStartCursorSelector = mkSelector "initWithStartCursor:"

-- | @Selector@ for @startCursor@
startCursorSelector :: Selector '[] (Id AVSampleCursor)
startCursorSelector = mkSelector "startCursor"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] AVSampleBufferRequestDirection
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[AVSampleBufferRequestDirection] ()
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @limitCursor@
limitCursorSelector :: Selector '[] (Id AVSampleCursor)
limitCursorSelector = mkSelector "limitCursor"

-- | @Selector@ for @setLimitCursor:@
setLimitCursorSelector :: Selector '[Id AVSampleCursor] ()
setLimitCursorSelector = mkSelector "setLimitCursor:"

-- | @Selector@ for @preferredMinSampleCount@
preferredMinSampleCountSelector :: Selector '[] CLong
preferredMinSampleCountSelector = mkSelector "preferredMinSampleCount"

-- | @Selector@ for @setPreferredMinSampleCount:@
setPreferredMinSampleCountSelector :: Selector '[CLong] ()
setPreferredMinSampleCountSelector = mkSelector "setPreferredMinSampleCount:"

-- | @Selector@ for @maxSampleCount@
maxSampleCountSelector :: Selector '[] CLong
maxSampleCountSelector = mkSelector "maxSampleCount"

-- | @Selector@ for @setMaxSampleCount:@
setMaxSampleCountSelector :: Selector '[CLong] ()
setMaxSampleCountSelector = mkSelector "setMaxSampleCount:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] AVSampleBufferRequestMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[AVSampleBufferRequestMode] ()
setModeSelector = mkSelector "setMode:"

