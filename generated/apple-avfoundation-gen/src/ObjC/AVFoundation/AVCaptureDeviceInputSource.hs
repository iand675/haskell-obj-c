{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeviceInputSource
--
-- An AVCaptureDeviceInputSource represents a distinct input source on an AVCaptureDevice object.
--
-- An AVCaptureDevice may optionally present an array of inputSources, representing distinct mutually exclusive inputs to the device, for example, an audio AVCaptureDevice might have ADAT optical and analog input sources. A video AVCaptureDevice might have an HDMI input source, or a component input source.
--
-- Generated bindings for @AVCaptureDeviceInputSource@.
module ObjC.AVFoundation.AVCaptureDeviceInputSource
  ( AVCaptureDeviceInputSource
  , IsAVCaptureDeviceInputSource(..)
  , init_
  , new
  , inputSourceID
  , localizedName
  , initSelector
  , inputSourceIDSelector
  , localizedNameSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureDeviceInputSource avCaptureDeviceInputSource => avCaptureDeviceInputSource -> IO (Id AVCaptureDeviceInputSource)
init_ avCaptureDeviceInputSource =
  sendOwnedMessage avCaptureDeviceInputSource initSelector

-- | @+ new@
new :: IO (Id AVCaptureDeviceInputSource)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDeviceInputSource"
    sendOwnedClassMessage cls' newSelector

-- | inputSourceID
--
-- An ID unique among the inputSources exposed by a given AVCaptureDevice.
--
-- An AVCaptureDevice's inputSources array must contain AVCaptureInputSource objects with unique inputSourceIDs.
--
-- ObjC selector: @- inputSourceID@
inputSourceID :: IsAVCaptureDeviceInputSource avCaptureDeviceInputSource => avCaptureDeviceInputSource -> IO (Id NSString)
inputSourceID avCaptureDeviceInputSource =
  sendMessage avCaptureDeviceInputSource inputSourceIDSelector

-- | localizedName
--
-- A localized human-readable name for the receiver.
--
-- This property can be used for displaying the name of the capture device input source in a user interface.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAVCaptureDeviceInputSource avCaptureDeviceInputSource => avCaptureDeviceInputSource -> IO (Id NSString)
localizedName avCaptureDeviceInputSource =
  sendMessage avCaptureDeviceInputSource localizedNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureDeviceInputSource)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureDeviceInputSource)
newSelector = mkSelector "new"

-- | @Selector@ for @inputSourceID@
inputSourceIDSelector :: Selector '[] (Id NSString)
inputSourceIDSelector = mkSelector "inputSourceID"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

