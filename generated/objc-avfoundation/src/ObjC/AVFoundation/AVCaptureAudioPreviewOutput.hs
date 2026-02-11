{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureAudioPreviewOutput
--
-- AVCaptureAudioPreviewOutput is a concrete subclass of AVCaptureOutput that can be used to preview the audio being captured.
--
-- Instances of AVCaptureAudioPreviewOutput have an associated Core Audio output device that can be used to play audio being captured by the capture session. The unique ID of a Core Audio device can be obtained from its kAudioDevicePropertyDeviceUID property.
--
-- Generated bindings for @AVCaptureAudioPreviewOutput@.
module ObjC.AVFoundation.AVCaptureAudioPreviewOutput
  ( AVCaptureAudioPreviewOutput
  , IsAVCaptureAudioPreviewOutput(..)
  , init_
  , new
  , outputDeviceUniqueID
  , setOutputDeviceUniqueID
  , volume
  , setVolume
  , initSelector
  , newSelector
  , outputDeviceUniqueIDSelector
  , setOutputDeviceUniqueIDSelector
  , volumeSelector
  , setVolumeSelector


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
init_ :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> IO (Id AVCaptureAudioPreviewOutput)
init_ avCaptureAudioPreviewOutput  =
  sendMsg avCaptureAudioPreviewOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureAudioPreviewOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureAudioPreviewOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | outputDeviceUniqueID
--
-- Specifies the unique ID of the Core Audio output device being used to play preview audio.
--
-- The value of this property is an NSString containing the unique ID of the Core Audio device to be used for output, or nil if the default system output should be used.
--
-- ObjC selector: @- outputDeviceUniqueID@
outputDeviceUniqueID :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> IO (Id NSString)
outputDeviceUniqueID avCaptureAudioPreviewOutput  =
  sendMsg avCaptureAudioPreviewOutput (mkSelector "outputDeviceUniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputDeviceUniqueID
--
-- Specifies the unique ID of the Core Audio output device being used to play preview audio.
--
-- The value of this property is an NSString containing the unique ID of the Core Audio device to be used for output, or nil if the default system output should be used.
--
-- ObjC selector: @- setOutputDeviceUniqueID:@
setOutputDeviceUniqueID :: (IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput, IsNSString value) => avCaptureAudioPreviewOutput -> value -> IO ()
setOutputDeviceUniqueID avCaptureAudioPreviewOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptureAudioPreviewOutput (mkSelector "setOutputDeviceUniqueID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | volume
--
-- Specifies the preview volume of the output.
--
-- The value of this property is the preview volume of the receiver, where 1.0 is the maximum volume and 0.0 is muted.
--
-- ObjC selector: @- volume@
volume :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> IO CFloat
volume avCaptureAudioPreviewOutput  =
  sendMsg avCaptureAudioPreviewOutput (mkSelector "volume") retCFloat []

-- | volume
--
-- Specifies the preview volume of the output.
--
-- The value of this property is the preview volume of the receiver, where 1.0 is the maximum volume and 0.0 is muted.
--
-- ObjC selector: @- setVolume:@
setVolume :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> CFloat -> IO ()
setVolume avCaptureAudioPreviewOutput  value =
  sendMsg avCaptureAudioPreviewOutput (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @outputDeviceUniqueID@
outputDeviceUniqueIDSelector :: Selector
outputDeviceUniqueIDSelector = mkSelector "outputDeviceUniqueID"

-- | @Selector@ for @setOutputDeviceUniqueID:@
setOutputDeviceUniqueIDSelector :: Selector
setOutputDeviceUniqueIDSelector = mkSelector "setOutputDeviceUniqueID:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

