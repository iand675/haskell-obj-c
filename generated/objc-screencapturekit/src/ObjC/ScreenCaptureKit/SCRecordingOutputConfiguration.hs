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
  , outputURLSelector
  , setOutputURLSelector
  , videoCodecTypeSelector
  , setVideoCodecTypeSelector
  , outputFileTypeSelector
  , setOutputFileTypeSelector
  , availableVideoCodecTypesSelector
  , availableOutputFileTypesSelector


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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies output URL to save the recording.
--
-- ObjC selector: @- outputURL@
outputURL :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSURL)
outputURL scRecordingOutputConfiguration  =
  sendMsg scRecordingOutputConfiguration (mkSelector "outputURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies output URL to save the recording.
--
-- ObjC selector: @- setOutputURL:@
setOutputURL :: (IsSCRecordingOutputConfiguration scRecordingOutputConfiguration, IsNSURL value) => scRecordingOutputConfiguration -> value -> IO ()
setOutputURL scRecordingOutputConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg scRecordingOutputConfiguration (mkSelector "setOutputURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies video codec for the recording output, default is AVVideoCodecTypeH264, supported values can be obtained using availableVideoCodecTypes
--
-- ObjC selector: @- videoCodecType@
videoCodecType :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSString)
videoCodecType scRecordingOutputConfiguration  =
  sendMsg scRecordingOutputConfiguration (mkSelector "videoCodecType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies video codec for the recording output, default is AVVideoCodecTypeH264, supported values can be obtained using availableVideoCodecTypes
--
-- ObjC selector: @- setVideoCodecType:@
setVideoCodecType :: (IsSCRecordingOutputConfiguration scRecordingOutputConfiguration, IsNSString value) => scRecordingOutputConfiguration -> value -> IO ()
setVideoCodecType scRecordingOutputConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg scRecordingOutputConfiguration (mkSelector "setVideoCodecType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies file type for the recording output, default is AVFileTypeMPEG4, supported values can be obtained using availableOutputFileTypes
--
-- ObjC selector: @- outputFileType@
outputFileType :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSString)
outputFileType scRecordingOutputConfiguration  =
  sendMsg scRecordingOutputConfiguration (mkSelector "outputFileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies file type for the recording output, default is AVFileTypeMPEG4, supported values can be obtained using availableOutputFileTypes
--
-- ObjC selector: @- setOutputFileType:@
setOutputFileType :: (IsSCRecordingOutputConfiguration scRecordingOutputConfiguration, IsNSString value) => scRecordingOutputConfiguration -> value -> IO ()
setOutputFileType scRecordingOutputConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg scRecordingOutputConfiguration (mkSelector "setOutputFileType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Returns an array of supported video codec formats that can be specified in SCRecordingOutputConfiguration for videoCodecType
--
-- ObjC selector: @- availableVideoCodecTypes@
availableVideoCodecTypes :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSArray)
availableVideoCodecTypes scRecordingOutputConfiguration  =
  sendMsg scRecordingOutputConfiguration (mkSelector "availableVideoCodecTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an array of supported file types that can be specified in SCRecordingOutputConfiguration for outputFileType    Provides the file types AVCaptureAudioFileOutput can write.
--
-- ObjC selector: @- availableOutputFileTypes@
availableOutputFileTypes :: IsSCRecordingOutputConfiguration scRecordingOutputConfiguration => scRecordingOutputConfiguration -> IO (Id NSArray)
availableOutputFileTypes scRecordingOutputConfiguration  =
  sendMsg scRecordingOutputConfiguration (mkSelector "availableOutputFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @setOutputURL:@
setOutputURLSelector :: Selector
setOutputURLSelector = mkSelector "setOutputURL:"

-- | @Selector@ for @videoCodecType@
videoCodecTypeSelector :: Selector
videoCodecTypeSelector = mkSelector "videoCodecType"

-- | @Selector@ for @setVideoCodecType:@
setVideoCodecTypeSelector :: Selector
setVideoCodecTypeSelector = mkSelector "setVideoCodecType:"

-- | @Selector@ for @outputFileType@
outputFileTypeSelector :: Selector
outputFileTypeSelector = mkSelector "outputFileType"

-- | @Selector@ for @setOutputFileType:@
setOutputFileTypeSelector :: Selector
setOutputFileTypeSelector = mkSelector "setOutputFileType:"

-- | @Selector@ for @availableVideoCodecTypes@
availableVideoCodecTypesSelector :: Selector
availableVideoCodecTypesSelector = mkSelector "availableVideoCodecTypes"

-- | @Selector@ for @availableOutputFileTypes@
availableOutputFileTypesSelector :: Selector
availableOutputFileTypesSelector = mkSelector "availableOutputFileTypes"

