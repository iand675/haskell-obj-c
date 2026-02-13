{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHLivePhotoEditingContext@.
module ObjC.Photos.PHLivePhotoEditingContext
  ( PHLivePhotoEditingContext
  , IsPHLivePhotoEditingContext(..)
  , initWithLivePhotoEditingInput
  , init_
  , saveLivePhotoToOutput_options_completionHandler
  , cancel
  , frameProcessor
  , setFrameProcessor
  , audioVolume
  , setAudioVolume
  , orientation
  , audioVolumeSelector
  , cancelSelector
  , frameProcessorSelector
  , initSelector
  , initWithLivePhotoEditingInputSelector
  , orientationSelector
  , saveLivePhotoToOutput_options_completionHandlerSelector
  , setAudioVolumeSelector
  , setFrameProcessorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializer from the specified live photo input Return nil if the specified input is not for a live photo
--
-- ObjC selector: @- initWithLivePhotoEditingInput:@
initWithLivePhotoEditingInput :: (IsPHLivePhotoEditingContext phLivePhotoEditingContext, IsPHContentEditingInput livePhotoInput) => phLivePhotoEditingContext -> livePhotoInput -> IO (Id PHLivePhotoEditingContext)
initWithLivePhotoEditingInput phLivePhotoEditingContext livePhotoInput =
  sendOwnedMessage phLivePhotoEditingContext initWithLivePhotoEditingInputSelector (toPHContentEditingInput livePhotoInput)

-- | @- init@
init_ :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO (Id PHLivePhotoEditingContext)
init_ phLivePhotoEditingContext =
  sendOwnedMessage phLivePhotoEditingContext initSelector

-- | Asynchronously process and save the edited live photo to the specified content editing output Options dictionary should be nil, reserved for future expansion
--
-- ObjC selector: @- saveLivePhotoToOutput:options:completionHandler:@
saveLivePhotoToOutput_options_completionHandler :: (IsPHLivePhotoEditingContext phLivePhotoEditingContext, IsPHContentEditingOutput output, IsNSDictionary options) => phLivePhotoEditingContext -> output -> options -> Ptr () -> IO ()
saveLivePhotoToOutput_options_completionHandler phLivePhotoEditingContext output options handler =
  sendMessage phLivePhotoEditingContext saveLivePhotoToOutput_options_completionHandlerSelector (toPHContentEditingOutput output) (toNSDictionary options) handler

-- | Cancel the current asynchronous operation This is implicitly called whenever prepare or save is called A canceled operation will call its completion handler with an appropriate error code
--
-- ObjC selector: @- cancel@
cancel :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO ()
cancel phLivePhotoEditingContext =
  sendMessage phLivePhotoEditingContext cancelSelector

-- | A block that can be set to process each frame of the live photo Note that the context uses a copy of the processor block during processing
--
-- ObjC selector: @- frameProcessor@
frameProcessor :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO (Ptr ())
frameProcessor phLivePhotoEditingContext =
  sendMessage phLivePhotoEditingContext frameProcessorSelector

-- | A block that can be set to process each frame of the live photo Note that the context uses a copy of the processor block during processing
--
-- ObjC selector: @- setFrameProcessor:@
setFrameProcessor :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> Ptr () -> IO ()
setFrameProcessor phLivePhotoEditingContext value =
  sendMessage phLivePhotoEditingContext setFrameProcessorSelector value

-- | Specify the audio volume of the edited live photo Must be between 0.0 and 1.0 Default to 1.0
--
-- ObjC selector: @- audioVolume@
audioVolume :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO CFloat
audioVolume phLivePhotoEditingContext =
  sendMessage phLivePhotoEditingContext audioVolumeSelector

-- | Specify the audio volume of the edited live photo Must be between 0.0 and 1.0 Default to 1.0
--
-- ObjC selector: @- setAudioVolume:@
setAudioVolume :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> CFloat -> IO ()
setAudioVolume phLivePhotoEditingContext value =
  sendMessage phLivePhotoEditingContext setAudioVolumeSelector value

-- | @- orientation@
orientation :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO CInt
orientation phLivePhotoEditingContext =
  sendMessage phLivePhotoEditingContext orientationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLivePhotoEditingInput:@
initWithLivePhotoEditingInputSelector :: Selector '[Id PHContentEditingInput] (Id PHLivePhotoEditingContext)
initWithLivePhotoEditingInputSelector = mkSelector "initWithLivePhotoEditingInput:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHLivePhotoEditingContext)
initSelector = mkSelector "init"

-- | @Selector@ for @saveLivePhotoToOutput:options:completionHandler:@
saveLivePhotoToOutput_options_completionHandlerSelector :: Selector '[Id PHContentEditingOutput, Id NSDictionary, Ptr ()] ()
saveLivePhotoToOutput_options_completionHandlerSelector = mkSelector "saveLivePhotoToOutput:options:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @frameProcessor@
frameProcessorSelector :: Selector '[] (Ptr ())
frameProcessorSelector = mkSelector "frameProcessor"

-- | @Selector@ for @setFrameProcessor:@
setFrameProcessorSelector :: Selector '[Ptr ()] ()
setFrameProcessorSelector = mkSelector "setFrameProcessor:"

-- | @Selector@ for @audioVolume@
audioVolumeSelector :: Selector '[] CFloat
audioVolumeSelector = mkSelector "audioVolume"

-- | @Selector@ for @setAudioVolume:@
setAudioVolumeSelector :: Selector '[CFloat] ()
setAudioVolumeSelector = mkSelector "setAudioVolume:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] CInt
orientationSelector = mkSelector "orientation"

