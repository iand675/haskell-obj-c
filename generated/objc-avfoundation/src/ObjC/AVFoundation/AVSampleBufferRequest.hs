{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithStartCursorSelector
  , startCursorSelector
  , directionSelector
  , setDirectionSelector
  , limitCursorSelector
  , setLimitCursorSelector
  , preferredMinSampleCountSelector
  , setPreferredMinSampleCountSelector
  , maxSampleCountSelector
  , setMaxSampleCountSelector
  , modeSelector
  , setModeSelector

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO (Id AVSampleBufferRequest)
init_ avSampleBufferRequest  =
  sendMsg avSampleBufferRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSampleBufferRequest)
new  =
  do
    cls' <- getRequiredClass "AVSampleBufferRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithStartCursor:@
initWithStartCursor :: (IsAVSampleBufferRequest avSampleBufferRequest, IsAVSampleCursor startCursor) => avSampleBufferRequest -> startCursor -> IO (Id AVSampleBufferRequest)
initWithStartCursor avSampleBufferRequest  startCursor =
withObjCPtr startCursor $ \raw_startCursor ->
    sendMsg avSampleBufferRequest (mkSelector "initWithStartCursor:") (retPtr retVoid) [argPtr (castPtr raw_startCursor :: Ptr ())] >>= ownedObject . castPtr

-- | @- startCursor@
startCursor :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO (Id AVSampleCursor)
startCursor avSampleBufferRequest  =
  sendMsg avSampleBufferRequest (mkSelector "startCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- direction@
direction :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO AVSampleBufferRequestDirection
direction avSampleBufferRequest  =
  fmap (coerce :: CLong -> AVSampleBufferRequestDirection) $ sendMsg avSampleBufferRequest (mkSelector "direction") retCLong []

-- | @- setDirection:@
setDirection :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> AVSampleBufferRequestDirection -> IO ()
setDirection avSampleBufferRequest  value =
  sendMsg avSampleBufferRequest (mkSelector "setDirection:") retVoid [argCLong (coerce value)]

-- | @- limitCursor@
limitCursor :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO (Id AVSampleCursor)
limitCursor avSampleBufferRequest  =
  sendMsg avSampleBufferRequest (mkSelector "limitCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLimitCursor:@
setLimitCursor :: (IsAVSampleBufferRequest avSampleBufferRequest, IsAVSampleCursor value) => avSampleBufferRequest -> value -> IO ()
setLimitCursor avSampleBufferRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSampleBufferRequest (mkSelector "setLimitCursor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- preferredMinSampleCount@
preferredMinSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO CLong
preferredMinSampleCount avSampleBufferRequest  =
  sendMsg avSampleBufferRequest (mkSelector "preferredMinSampleCount") retCLong []

-- | @- setPreferredMinSampleCount:@
setPreferredMinSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> CLong -> IO ()
setPreferredMinSampleCount avSampleBufferRequest  value =
  sendMsg avSampleBufferRequest (mkSelector "setPreferredMinSampleCount:") retVoid [argCLong (fromIntegral value)]

-- | @- maxSampleCount@
maxSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO CLong
maxSampleCount avSampleBufferRequest  =
  sendMsg avSampleBufferRequest (mkSelector "maxSampleCount") retCLong []

-- | @- setMaxSampleCount:@
setMaxSampleCount :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> CLong -> IO ()
setMaxSampleCount avSampleBufferRequest  value =
  sendMsg avSampleBufferRequest (mkSelector "setMaxSampleCount:") retVoid [argCLong (fromIntegral value)]

-- | @- mode@
mode :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> IO AVSampleBufferRequestMode
mode avSampleBufferRequest  =
  fmap (coerce :: CLong -> AVSampleBufferRequestMode) $ sendMsg avSampleBufferRequest (mkSelector "mode") retCLong []

-- | @- setMode:@
setMode :: IsAVSampleBufferRequest avSampleBufferRequest => avSampleBufferRequest -> AVSampleBufferRequestMode -> IO ()
setMode avSampleBufferRequest  value =
  sendMsg avSampleBufferRequest (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithStartCursor:@
initWithStartCursorSelector :: Selector
initWithStartCursorSelector = mkSelector "initWithStartCursor:"

-- | @Selector@ for @startCursor@
startCursorSelector :: Selector
startCursorSelector = mkSelector "startCursor"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @limitCursor@
limitCursorSelector :: Selector
limitCursorSelector = mkSelector "limitCursor"

-- | @Selector@ for @setLimitCursor:@
setLimitCursorSelector :: Selector
setLimitCursorSelector = mkSelector "setLimitCursor:"

-- | @Selector@ for @preferredMinSampleCount@
preferredMinSampleCountSelector :: Selector
preferredMinSampleCountSelector = mkSelector "preferredMinSampleCount"

-- | @Selector@ for @setPreferredMinSampleCount:@
setPreferredMinSampleCountSelector :: Selector
setPreferredMinSampleCountSelector = mkSelector "setPreferredMinSampleCount:"

-- | @Selector@ for @maxSampleCount@
maxSampleCountSelector :: Selector
maxSampleCountSelector = mkSelector "maxSampleCount"

-- | @Selector@ for @setMaxSampleCount:@
setMaxSampleCountSelector :: Selector
setMaxSampleCountSelector = mkSelector "setMaxSampleCount:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

