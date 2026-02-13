{-# LANGUAGE DataKinds #-}
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
  , setVolumeSelector
  , volumeSelector


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
init_ :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> IO (Id AVCaptureAudioPreviewOutput)
init_ avCaptureAudioPreviewOutput =
  sendOwnedMessage avCaptureAudioPreviewOutput initSelector

-- | @+ new@
new :: IO (Id AVCaptureAudioPreviewOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureAudioPreviewOutput"
    sendOwnedClassMessage cls' newSelector

-- | outputDeviceUniqueID
--
-- Specifies the unique ID of the Core Audio output device being used to play preview audio.
--
-- The value of this property is an NSString containing the unique ID of the Core Audio device to be used for output, or nil if the default system output should be used.
--
-- ObjC selector: @- outputDeviceUniqueID@
outputDeviceUniqueID :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> IO (Id NSString)
outputDeviceUniqueID avCaptureAudioPreviewOutput =
  sendMessage avCaptureAudioPreviewOutput outputDeviceUniqueIDSelector

-- | outputDeviceUniqueID
--
-- Specifies the unique ID of the Core Audio output device being used to play preview audio.
--
-- The value of this property is an NSString containing the unique ID of the Core Audio device to be used for output, or nil if the default system output should be used.
--
-- ObjC selector: @- setOutputDeviceUniqueID:@
setOutputDeviceUniqueID :: (IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput, IsNSString value) => avCaptureAudioPreviewOutput -> value -> IO ()
setOutputDeviceUniqueID avCaptureAudioPreviewOutput value =
  sendMessage avCaptureAudioPreviewOutput setOutputDeviceUniqueIDSelector (toNSString value)

-- | volume
--
-- Specifies the preview volume of the output.
--
-- The value of this property is the preview volume of the receiver, where 1.0 is the maximum volume and 0.0 is muted.
--
-- ObjC selector: @- volume@
volume :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> IO CFloat
volume avCaptureAudioPreviewOutput =
  sendMessage avCaptureAudioPreviewOutput volumeSelector

-- | volume
--
-- Specifies the preview volume of the output.
--
-- The value of this property is the preview volume of the receiver, where 1.0 is the maximum volume and 0.0 is muted.
--
-- ObjC selector: @- setVolume:@
setVolume :: IsAVCaptureAudioPreviewOutput avCaptureAudioPreviewOutput => avCaptureAudioPreviewOutput -> CFloat -> IO ()
setVolume avCaptureAudioPreviewOutput value =
  sendMessage avCaptureAudioPreviewOutput setVolumeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureAudioPreviewOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureAudioPreviewOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @outputDeviceUniqueID@
outputDeviceUniqueIDSelector :: Selector '[] (Id NSString)
outputDeviceUniqueIDSelector = mkSelector "outputDeviceUniqueID"

-- | @Selector@ for @setOutputDeviceUniqueID:@
setOutputDeviceUniqueIDSelector :: Selector '[Id NSString] ()
setOutputDeviceUniqueIDSelector = mkSelector "setOutputDeviceUniqueID:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

