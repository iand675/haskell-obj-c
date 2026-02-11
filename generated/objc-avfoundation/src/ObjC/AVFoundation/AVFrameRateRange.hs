{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVFrameRateRange
--
-- An AVFrameRateRange expresses a range of valid frame rates as min and max rate and min and max duration.
--
-- An AVCaptureDevice exposes an array of formats, and its current activeFormat may be queried. The payload for the formats property is an array of AVCaptureDeviceFormat objects and the activeFormat property payload is an AVCaptureDeviceFormat. AVCaptureDeviceFormat wraps a CMFormatDescription and expresses a range of valid video frame rates as an NSArray of AVFrameRateRange objects. AVFrameRateRange expresses min and max frame rate as a rate in frames per second and duration (CMTime). An AVFrameRateRange object is immutable. Its values do not change for the life of the object.
--
-- Generated bindings for @AVFrameRateRange@.
module ObjC.AVFoundation.AVFrameRateRange
  ( AVFrameRateRange
  , IsAVFrameRateRange(..)
  , init_
  , new
  , minFrameRate
  , maxFrameRate
  , initSelector
  , newSelector
  , minFrameRateSelector
  , maxFrameRateSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVFrameRateRange avFrameRateRange => avFrameRateRange -> IO (Id AVFrameRateRange)
init_ avFrameRateRange  =
  sendMsg avFrameRateRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVFrameRateRange)
new  =
  do
    cls' <- getRequiredClass "AVFrameRateRange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | minFrameRate
--
-- A Float64 indicating the minimum frame rate supported by this range.
--
-- This read-only property indicates the minimum frame rate supported by this range in frames per second.
--
-- ObjC selector: @- minFrameRate@
minFrameRate :: IsAVFrameRateRange avFrameRateRange => avFrameRateRange -> IO CDouble
minFrameRate avFrameRateRange  =
  sendMsg avFrameRateRange (mkSelector "minFrameRate") retCDouble []

-- | maxFrameRate
--
-- A Float64 indicating the maximum frame rate supported by this range.
--
-- This read-only property indicates the maximum frame rate supported by this range in frames per second.
--
-- ObjC selector: @- maxFrameRate@
maxFrameRate :: IsAVFrameRateRange avFrameRateRange => avFrameRateRange -> IO CDouble
maxFrameRate avFrameRateRange  =
  sendMsg avFrameRateRange (mkSelector "maxFrameRate") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @minFrameRate@
minFrameRateSelector :: Selector
minFrameRateSelector = mkSelector "minFrameRate"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector
maxFrameRateSelector = mkSelector "maxFrameRate"

