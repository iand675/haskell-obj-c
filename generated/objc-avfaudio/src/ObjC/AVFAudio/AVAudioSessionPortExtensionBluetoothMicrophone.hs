{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that describes capabilities of Bluetooth microphone ports.
--
-- Generated bindings for @AVAudioSessionPortExtensionBluetoothMicrophone@.
module ObjC.AVFAudio.AVAudioSessionPortExtensionBluetoothMicrophone
  ( AVAudioSessionPortExtensionBluetoothMicrophone
  , IsAVAudioSessionPortExtensionBluetoothMicrophone(..)
  , highQualityRecording
  , farFieldCapture
  , highQualityRecordingSelector
  , farFieldCaptureSelector


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

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Describes whether this port supports Bluetooth high-quality recording.
--
-- Please see ``AVAudioSessionCategoryOptions/AVAudioSessionCategoryOptionBluetoothHighQualityRecording`` for details.
--
-- ObjC selector: @- highQualityRecording@
highQualityRecording :: IsAVAudioSessionPortExtensionBluetoothMicrophone avAudioSessionPortExtensionBluetoothMicrophone => avAudioSessionPortExtensionBluetoothMicrophone -> IO (Id AVAudioSessionCapability)
highQualityRecording avAudioSessionPortExtensionBluetoothMicrophone  =
  sendMsg avAudioSessionPortExtensionBluetoothMicrophone (mkSelector "highQualityRecording") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Describes whether this port supports far-field input capture.
--
-- ObjC selector: @- farFieldCapture@
farFieldCapture :: IsAVAudioSessionPortExtensionBluetoothMicrophone avAudioSessionPortExtensionBluetoothMicrophone => avAudioSessionPortExtensionBluetoothMicrophone -> IO (Id AVAudioSessionCapability)
farFieldCapture avAudioSessionPortExtensionBluetoothMicrophone  =
  sendMsg avAudioSessionPortExtensionBluetoothMicrophone (mkSelector "farFieldCapture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @highQualityRecording@
highQualityRecordingSelector :: Selector
highQualityRecordingSelector = mkSelector "highQualityRecording"

-- | @Selector@ for @farFieldCapture@
farFieldCaptureSelector :: Selector
farFieldCaptureSelector = mkSelector "farFieldCapture"

