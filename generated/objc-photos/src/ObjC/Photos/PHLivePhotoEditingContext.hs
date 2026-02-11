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
  , initWithLivePhotoEditingInputSelector
  , initSelector
  , saveLivePhotoToOutput_options_completionHandlerSelector
  , cancelSelector
  , frameProcessorSelector
  , setFrameProcessorSelector
  , audioVolumeSelector
  , setAudioVolumeSelector
  , orientationSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializer from the specified live photo input Return nil if the specified input is not for a live photo
--
-- ObjC selector: @- initWithLivePhotoEditingInput:@
initWithLivePhotoEditingInput :: (IsPHLivePhotoEditingContext phLivePhotoEditingContext, IsPHContentEditingInput livePhotoInput) => phLivePhotoEditingContext -> livePhotoInput -> IO (Id PHLivePhotoEditingContext)
initWithLivePhotoEditingInput phLivePhotoEditingContext  livePhotoInput =
withObjCPtr livePhotoInput $ \raw_livePhotoInput ->
    sendMsg phLivePhotoEditingContext (mkSelector "initWithLivePhotoEditingInput:") (retPtr retVoid) [argPtr (castPtr raw_livePhotoInput :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO (Id PHLivePhotoEditingContext)
init_ phLivePhotoEditingContext  =
  sendMsg phLivePhotoEditingContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Asynchronously process and save the edited live photo to the specified content editing output Options dictionary should be nil, reserved for future expansion
--
-- ObjC selector: @- saveLivePhotoToOutput:options:completionHandler:@
saveLivePhotoToOutput_options_completionHandler :: (IsPHLivePhotoEditingContext phLivePhotoEditingContext, IsPHContentEditingOutput output, IsNSDictionary options) => phLivePhotoEditingContext -> output -> options -> Ptr () -> IO ()
saveLivePhotoToOutput_options_completionHandler phLivePhotoEditingContext  output options handler =
withObjCPtr output $ \raw_output ->
  withObjCPtr options $ \raw_options ->
      sendMsg phLivePhotoEditingContext (mkSelector "saveLivePhotoToOutput:options:completionHandler:") retVoid [argPtr (castPtr raw_output :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Cancel the current asynchronous operation This is implicitly called whenever prepare or save is called A canceled operation will call its completion handler with an appropriate error code
--
-- ObjC selector: @- cancel@
cancel :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO ()
cancel phLivePhotoEditingContext  =
  sendMsg phLivePhotoEditingContext (mkSelector "cancel") retVoid []

-- | A block that can be set to process each frame of the live photo Note that the context uses a copy of the processor block during processing
--
-- ObjC selector: @- frameProcessor@
frameProcessor :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO (Ptr ())
frameProcessor phLivePhotoEditingContext  =
  fmap castPtr $ sendMsg phLivePhotoEditingContext (mkSelector "frameProcessor") (retPtr retVoid) []

-- | A block that can be set to process each frame of the live photo Note that the context uses a copy of the processor block during processing
--
-- ObjC selector: @- setFrameProcessor:@
setFrameProcessor :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> Ptr () -> IO ()
setFrameProcessor phLivePhotoEditingContext  value =
  sendMsg phLivePhotoEditingContext (mkSelector "setFrameProcessor:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Specify the audio volume of the edited live photo Must be between 0.0 and 1.0 Default to 1.0
--
-- ObjC selector: @- audioVolume@
audioVolume :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO CFloat
audioVolume phLivePhotoEditingContext  =
  sendMsg phLivePhotoEditingContext (mkSelector "audioVolume") retCFloat []

-- | Specify the audio volume of the edited live photo Must be between 0.0 and 1.0 Default to 1.0
--
-- ObjC selector: @- setAudioVolume:@
setAudioVolume :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> CFloat -> IO ()
setAudioVolume phLivePhotoEditingContext  value =
  sendMsg phLivePhotoEditingContext (mkSelector "setAudioVolume:") retVoid [argCFloat (fromIntegral value)]

-- | @- orientation@
orientation :: IsPHLivePhotoEditingContext phLivePhotoEditingContext => phLivePhotoEditingContext -> IO CInt
orientation phLivePhotoEditingContext  =
  sendMsg phLivePhotoEditingContext (mkSelector "orientation") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLivePhotoEditingInput:@
initWithLivePhotoEditingInputSelector :: Selector
initWithLivePhotoEditingInputSelector = mkSelector "initWithLivePhotoEditingInput:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @saveLivePhotoToOutput:options:completionHandler:@
saveLivePhotoToOutput_options_completionHandlerSelector :: Selector
saveLivePhotoToOutput_options_completionHandlerSelector = mkSelector "saveLivePhotoToOutput:options:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @frameProcessor@
frameProcessorSelector :: Selector
frameProcessorSelector = mkSelector "frameProcessor"

-- | @Selector@ for @setFrameProcessor:@
setFrameProcessorSelector :: Selector
setFrameProcessorSelector = mkSelector "setFrameProcessor:"

-- | @Selector@ for @audioVolume@
audioVolumeSelector :: Selector
audioVolumeSelector = mkSelector "audioVolume"

-- | @Selector@ for @setAudioVolume:@
setAudioVolumeSelector :: Selector
setAudioVolumeSelector = mkSelector "setAudioVolume:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

