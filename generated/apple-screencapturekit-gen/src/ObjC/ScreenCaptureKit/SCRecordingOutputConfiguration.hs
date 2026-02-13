{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCRecordingOutputConfiguration
--
-- SCRecordingOutputConfiguration is an object that encapsulates the configuration for recording.
--
-- Generated bindings for @SCRecordingOutputConfiguration@.
module ObjC.ScreenCaptureKit.SCRecordingOutputConfiguration
  ( SCRecordingOutputConfiguration
  , IsSCRecordingOutputConfiguration(..)
  , outputURL
  , setOutputURL
  , videoCodecType
  , setVideoCodecType
  , outputFileType
  , setOutputFileType
  , availableVideoCodecTypes
  , availableOutputFileTypes
  , availableOutputFileTypesSelector
  , availableVideoCodecTypesSelector
  , outputFileTypeSelector
  , outputURLSelector
  , setOutputFileTypeSelector
  , setOutputURLSelector
  , setVideoCodecTypeSelector
  , videoCodecTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies output URL to save the recording.
--
-- ObjC selector: @- outputURL@
outputURL :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSURL)
outputURL scRecordingOutputConfiguration =
  sendMessage scRecordingOutputConfiguration outputURLSelector

-- | Specifies output URL to save the recording.
--
-- ObjC selector: @- setOutputURL:@
setOutputURL :: (IsSCRecordingOutputConfiguration scRecordingOutputConfiguration, IsNSURL value) => scRecordingOutputConfiguration -> value -> IO ()
setOutputURL scRecordingOutputConfiguration value =
  sendMessage scRecordingOutputConfiguration setOutputURLSelector (toNSURL value)

-- | Specifies video codec for the recording output, default is AVVideoCodecTypeH264, supported values can be obtained using availableVideoCodecTypes
--
-- ObjC selector: @- videoCodecType@
videoCodecType :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSString)
videoCodecType scRecordingOutputConfiguration =
  sendMessage scRecordingOutputConfiguration videoCodecTypeSelector

-- | Specifies video codec for the recording output, default is AVVideoCodecTypeH264, supported values can be obtained using availableVideoCodecTypes
--
-- ObjC selector: @- setVideoCodecType:@
setVideoCodecType :: (IsSCRecordingOutputConfiguration scRecordingOutputConfiguration, IsNSString value) => scRecordingOutputConfiguration -> value -> IO ()
setVideoCodecType scRecordingOutputConfiguration value =
  sendMessage scRecordingOutputConfiguration setVideoCodecTypeSelector (toNSString value)

-- | Specifies file type for the recording output, default is AVFileTypeMPEG4, supported values can be obtained using availableOutputFileTypes
--
-- ObjC selector: @- outputFileType@
outputFileType :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSString)
outputFileType scRecordingOutputConfiguration =
  sendMessage scRecordingOutputConfiguration outputFileTypeSelector

-- | Specifies file type for the recording output, default is AVFileTypeMPEG4, supported values can be obtained using availableOutputFileTypes
--
-- ObjC selector: @- setOutputFileType:@
setOutputFileType :: (IsSCRecordingOutputConfiguration scRecordingOutputConfiguration, IsNSString value) => scRecordingOutputConfiguration -> value -> IO ()
setOutputFileType scRecordingOutputConfiguration value =
  sendMessage scRecordingOutputConfiguration setOutputFileTypeSelector (toNSString value)

-- | Returns an array of supported video codec formats that can be specified in SCRecordingOutputConfiguration for videoCodecType
--
-- ObjC selector: @- availableVideoCodecTypes@
availableVideoCodecTypes :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSArray)
availableVideoCodecTypes scRecordingOutputConfiguration =
  sendMessage scRecordingOutputConfiguration availableVideoCodecTypesSelector

-- | Returns an array of supported file types that can be specified in SCRecordingOutputConfiguration for outputFileType    Provides the file types AVCaptureAudioFileOutput can write.
--
-- ObjC selector: @- availableOutputFileTypes@
availableOutputFileTypes :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSArray)
availableOutputFileTypes scRecordingOutputConfiguration =
  sendMessage scRecordingOutputConfiguration availableOutputFileTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector '[] (Id NSURL)
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @setOutputURL:@
setOutputURLSelector :: Selector '[Id NSURL] ()
setOutputURLSelector = mkSelector "setOutputURL:"

-- | @Selector@ for @videoCodecType@
videoCodecTypeSelector :: Selector '[] (Id NSString)
videoCodecTypeSelector = mkSelector "videoCodecType"

-- | @Selector@ for @setVideoCodecType:@
setVideoCodecTypeSelector :: Selector '[Id NSString] ()
setVideoCodecTypeSelector = mkSelector "setVideoCodecType:"

-- | @Selector@ for @outputFileType@
outputFileTypeSelector :: Selector '[] (Id NSString)
outputFileTypeSelector = mkSelector "outputFileType"

-- | @Selector@ for @setOutputFileType:@
setOutputFileTypeSelector :: Selector '[Id NSString] ()
setOutputFileTypeSelector = mkSelector "setOutputFileType:"

-- | @Selector@ for @availableVideoCodecTypes@
availableVideoCodecTypesSelector :: Selector '[] (Id NSArray)
availableVideoCodecTypesSelector = mkSelector "availableVideoCodecTypes"

-- | @Selector@ for @availableOutputFileTypes@
availableOutputFileTypesSelector :: Selector '[] (Id NSArray)
availableOutputFileTypesSelector = mkSelector "availableOutputFileTypes"

