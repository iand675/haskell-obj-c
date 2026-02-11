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
  , newSelector
  , inputSourceIDSelector
  , localizedNameSelector


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
init_ :: IsAVCaptureDeviceInputSource avCaptureDeviceInputSource => avCaptureDeviceInputSource -> IO (Id AVCaptureDeviceInputSource)
init_ avCaptureDeviceInputSource  =
  sendMsg avCaptureDeviceInputSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureDeviceInputSource)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDeviceInputSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | inputSourceID
--
-- An ID unique among the inputSources exposed by a given AVCaptureDevice.
--
-- An AVCaptureDevice's inputSources array must contain AVCaptureInputSource objects with unique inputSourceIDs.
--
-- ObjC selector: @- inputSourceID@
inputSourceID :: IsAVCaptureDeviceInputSource avCaptureDeviceInputSource => avCaptureDeviceInputSource -> IO (Id NSString)
inputSourceID avCaptureDeviceInputSource  =
  sendMsg avCaptureDeviceInputSource (mkSelector "inputSourceID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedName
--
-- A localized human-readable name for the receiver.
--
-- This property can be used for displaying the name of the capture device input source in a user interface.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAVCaptureDeviceInputSource avCaptureDeviceInputSource => avCaptureDeviceInputSource -> IO (Id NSString)
localizedName avCaptureDeviceInputSource  =
  sendMsg avCaptureDeviceInputSource (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @inputSourceID@
inputSourceIDSelector :: Selector
inputSourceIDSelector = mkSelector "inputSourceID"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

