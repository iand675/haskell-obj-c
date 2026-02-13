{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that defines a frame-based cadence for processing the video stream.
--
-- Generated bindings for @VNVideoProcessorFrameRateCadence@.
module ObjC.Vision.VNVideoProcessorFrameRateCadence
  ( VNVideoProcessorFrameRateCadence
  , IsVNVideoProcessorFrameRateCadence(..)
  , init_
  , initWithFrameRate
  , frameRate
  , frameRateSelector
  , initSelector
  , initWithFrameRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNVideoProcessorFrameRateCadence vnVideoProcessorFrameRateCadence => vnVideoProcessorFrameRateCadence -> IO (Id VNVideoProcessorFrameRateCadence)
init_ vnVideoProcessorFrameRateCadence =
  sendOwnedMessage vnVideoProcessorFrameRateCadence initSelector

-- | @- initWithFrameRate:@
initWithFrameRate :: IsVNVideoProcessorFrameRateCadence vnVideoProcessorFrameRateCadence => vnVideoProcessorFrameRateCadence -> CLong -> IO (Id VNVideoProcessorFrameRateCadence)
initWithFrameRate vnVideoProcessorFrameRateCadence frameRate =
  sendOwnedMessage vnVideoProcessorFrameRateCadence initWithFrameRateSelector frameRate

-- | @- frameRate@
frameRate :: IsVNVideoProcessorFrameRateCadence vnVideoProcessorFrameRateCadence => vnVideoProcessorFrameRateCadence -> IO CLong
frameRate vnVideoProcessorFrameRateCadence =
  sendMessage vnVideoProcessorFrameRateCadence frameRateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNVideoProcessorFrameRateCadence)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFrameRate:@
initWithFrameRateSelector :: Selector '[CLong] (Id VNVideoProcessorFrameRateCadence)
initWithFrameRateSelector = mkSelector "initWithFrameRate:"

-- | @Selector@ for @frameRate@
frameRateSelector :: Selector '[] CLong
frameRateSelector = mkSelector "frameRate"

