{-# LANGUAGE DataKinds #-}
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
  , farFieldCaptureSelector
  , highQualityRecordingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
highQualityRecording avAudioSessionPortExtensionBluetoothMicrophone =
  sendMessage avAudioSessionPortExtensionBluetoothMicrophone highQualityRecordingSelector

-- | Describes whether this port supports far-field input capture.
--
-- ObjC selector: @- farFieldCapture@
farFieldCapture :: IsAVAudioSessionPortExtensionBluetoothMicrophone avAudioSessionPortExtensionBluetoothMicrophone => avAudioSessionPortExtensionBluetoothMicrophone -> IO (Id AVAudioSessionCapability)
farFieldCapture avAudioSessionPortExtensionBluetoothMicrophone =
  sendMessage avAudioSessionPortExtensionBluetoothMicrophone farFieldCaptureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @highQualityRecording@
highQualityRecordingSelector :: Selector '[] (Id AVAudioSessionCapability)
highQualityRecordingSelector = mkSelector "highQualityRecording"

-- | @Selector@ for @farFieldCapture@
farFieldCaptureSelector :: Selector '[] (Id AVAudioSessionCapability)
farFieldCaptureSelector = mkSelector "farFieldCapture"

