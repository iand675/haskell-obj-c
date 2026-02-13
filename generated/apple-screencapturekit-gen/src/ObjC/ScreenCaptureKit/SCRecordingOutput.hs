{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCRecordingOutput@.
module ObjC.ScreenCaptureKit.SCRecordingOutput
  ( SCRecordingOutput
  , IsSCRecordingOutput(..)
  , initWithConfiguration_delegate
  , recordedFileSize
  , initWithConfiguration_delegateSelector
  , recordedFileSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithConfiguration:delegate:
--
-- initialize SCRecordingOutput object with SCRecordingOutputConfiguration and SCRecordingOutputDelegate
--
-- @recordingOutputConfiguration@ — the requested recording configuration to be applied to the SCRecordingOutput
--
-- @delegate@ — object conforming SCRecordingOutputDelegate protocol. Clients must specify a delegate so that they can be notified about recording event.
--
-- Client can create a SCRecordingOutput with this initializer and add to SCStream to record all captured media into one recording file given output url specified in recordingOutputConfig. The recording will be using H264 and file format is MPEG-4.
--
-- ObjC selector: @- initWithConfiguration:delegate:@
initWithConfiguration_delegate :: (IsSCRecordingOutput scRecordingOutput, IsSCRecordingOutputConfiguration recordingOutputConfiguration) => scRecordingOutput -> recordingOutputConfiguration -> RawId -> IO (Id SCRecordingOutput)
initWithConfiguration_delegate scRecordingOutput recordingOutputConfiguration delegate =
  sendOwnedMessage scRecordingOutput initWithConfiguration_delegateSelector (toSCRecordingOutputConfiguration recordingOutputConfiguration) delegate

-- | Indicates current size, in bytes, of the data recorded to the output file.
--
-- ObjC selector: @- recordedFileSize@
recordedFileSize :: IsSCRecordingOutput scRecordingOutput => scRecordingOutput -> IO CLong
recordedFileSize scRecordingOutput =
  sendMessage scRecordingOutput recordedFileSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:delegate:@
initWithConfiguration_delegateSelector :: Selector '[Id SCRecordingOutputConfiguration, RawId] (Id SCRecordingOutput)
initWithConfiguration_delegateSelector = mkSelector "initWithConfiguration:delegate:"

-- | @Selector@ for @recordedFileSize@
recordedFileSizeSelector :: Selector '[] CLong
recordedFileSizeSelector = mkSelector "recordedFileSize"

