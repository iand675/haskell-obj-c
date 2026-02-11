{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class containing methods that relate to an application bundle's audio (i.e. a collection of one or more AVAudioSession instances)
--
-- Generated bindings for @AVAudioApplication@.
module ObjC.AVFAudio.AVAudioApplication
  ( AVAudioApplication
  , IsAVAudioApplication(..)
  , init_
  , setInputMuted_error
  , setInputMuteStateChangeHandler_error
  , requestRecordPermissionWithCompletionHandler
  , requestMicrophoneInjectionPermissionWithCompletionHandler
  , sharedInstance
  , inputMuted
  , recordPermission
  , microphoneInjectionPermission
  , initSelector
  , setInputMuted_errorSelector
  , setInputMuteStateChangeHandler_errorSelector
  , requestRecordPermissionWithCompletionHandlerSelector
  , requestMicrophoneInjectionPermissionWithCompletionHandlerSelector
  , sharedInstanceSelector
  , inputMutedSelector
  , recordPermissionSelector
  , microphoneInjectionPermissionSelector

  -- * Enum types
  , AVAudioApplicationMicrophoneInjectionPermission(AVAudioApplicationMicrophoneInjectionPermission)
  , pattern AVAudioApplicationMicrophoneInjectionPermissionServiceDisabled
  , pattern AVAudioApplicationMicrophoneInjectionPermissionUndetermined
  , pattern AVAudioApplicationMicrophoneInjectionPermissionDenied
  , pattern AVAudioApplicationMicrophoneInjectionPermissionGranted
  , AVAudioApplicationRecordPermission(AVAudioApplicationRecordPermission)
  , pattern AVAudioApplicationRecordPermissionUndetermined
  , pattern AVAudioApplicationRecordPermissionDenied
  , pattern AVAudioApplicationRecordPermissionGranted

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
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | See: @sharedInstance@
--
-- ObjC selector: @- init@
init_ :: IsAVAudioApplication avAudioApplication => avAudioApplication -> IO (Id AVAudioApplication)
init_ avAudioApplication  =
    sendMsg avAudioApplication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Set the muted/unmuted state of the application's audio input. When set true, inputs (microphone etc.) of all audio clients relating to this application will have their samples zeroed out.
--
-- Note: - this is per-application input muting and doesn't affect the hardware mute state.
--
-- ObjC selector: @- setInputMuted:error:@
setInputMuted_error :: (IsAVAudioApplication avAudioApplication, IsNSError outError) => avAudioApplication -> Bool -> outError -> IO Bool
setInputMuted_error avAudioApplication  muted outError =
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioApplication (mkSelector "setInputMuted:error:") retCULong [argCULong (if muted then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | Provide a block that implements your app's input (microphone) muting logic (macOS only). The block will be called			whenever the input mute state changes, either due to changing the @AVAudioApplication.inputMute@ property on			this API, or due to a Bluetooth audio accessory gesture (certain AirPods / Beats headphones) changing the mute state.
--
-- @inputMuteHandler@ — block that will be called upon every input mute state change. If the boolean @inputShouldBeMuted@			is true, your block should mute all input/microphone samples until the next time the handler is called. Your block should return			a value of YES if successful, or in exceptional cases return a NO value if the mute action was unsuccesful.			Since the input mute handling logic should happen a single place, subsequent calls to this method will overwrite any previously			registered block with the one provided. A nil value may be provided to cancel the block being called, e.g. at end of call lifecycle.
--
-- Note: This is available on macOS only - for all other platforms input muting will be handled internally. It is recommended only to			perform your input muting logic within this block, and to perform your UI updates for input mute state changes within the handler			for AVAudioApplicationInputMuteStateChangeNotification. This handler should be set by the process doing the call's audio I/O.
--
-- ObjC selector: @- setInputMuteStateChangeHandler:error:@
setInputMuteStateChangeHandler_error :: (IsAVAudioApplication avAudioApplication, IsNSError outError) => avAudioApplication -> Ptr () -> outError -> IO Bool
setInputMuteStateChangeHandler_error avAudioApplication  inputMuteHandler outError =
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioApplication (mkSelector "setInputMuteStateChangeHandler:error:") retCULong [argPtr (castPtr inputMuteHandler :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Checks to see if calling process has permission to record audio.
--
-- The 'response' block will be called immediately if permission has already been granted or	denied.  Otherwise, it presents a dialog to notify the user and allow them to choose, and calls	the block once the UI has been dismissed.  'granted' indicates whether permission has been	granted. Note that the block may be called in a different thread context.
--
-- ObjC selector: @+ requestRecordPermissionWithCompletionHandler:@
requestRecordPermissionWithCompletionHandler :: Ptr () -> IO ()
requestRecordPermissionWithCompletionHandler response =
  do
    cls' <- getRequiredClass "AVAudioApplication"
    sendClassMsg cls' (mkSelector "requestRecordPermissionWithCompletionHandler:") retVoid [argPtr (castPtr response :: Ptr ())]

-- | Checks to see if calling process has permission to inject audio to input stream.
--
-- The 'response' block will be called immediately if permission has already been granted or    denied or if the service is disabled by the user.  Otherwise, it presents a dialog to notify the    user and allow them to choose, and calls the block once the UI has been dismissed.    'granted' indicates whether permission has been granted. Note that the block may be    called in a different thread context.
--
-- ObjC selector: @+ requestMicrophoneInjectionPermissionWithCompletionHandler:@
requestMicrophoneInjectionPermissionWithCompletionHandler :: Ptr () -> IO ()
requestMicrophoneInjectionPermissionWithCompletionHandler response =
  do
    cls' <- getRequiredClass "AVAudioApplication"
    sendClassMsg cls' (mkSelector "requestMicrophoneInjectionPermissionWithCompletionHandler:") retVoid [argPtr (castPtr response :: Ptr ())]

-- | Returns the singleton instance
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id AVAudioApplication)
sharedInstance  =
  do
    cls' <- getRequiredClass "AVAudioApplication"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the input muted state - return value is boolean 0 for unmuted or value 1 for muted (input samples zeroed out)
--
-- ObjC selector: @- inputMuted@
inputMuted :: IsAVAudioApplication avAudioApplication => avAudioApplication -> IO Bool
inputMuted avAudioApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioApplication (mkSelector "inputMuted") retCULong []

-- | Returns an enum indicating whether the user has granted or denied permission to record, or has not been asked
--
-- ObjC selector: @- recordPermission@
recordPermission :: IsAVAudioApplication avAudioApplication => avAudioApplication -> IO AVAudioApplicationRecordPermission
recordPermission avAudioApplication  =
    fmap (coerce :: CLong -> AVAudioApplicationRecordPermission) $ sendMsg avAudioApplication (mkSelector "recordPermission") retCLong []

-- | Returns an enum indicating whether the user has granted or denied permission to inject audio into input, or has not been asked
--
-- ObjC selector: @- microphoneInjectionPermission@
microphoneInjectionPermission :: IsAVAudioApplication avAudioApplication => avAudioApplication -> IO AVAudioApplicationMicrophoneInjectionPermission
microphoneInjectionPermission avAudioApplication  =
    fmap (coerce :: CLong -> AVAudioApplicationMicrophoneInjectionPermission) $ sendMsg avAudioApplication (mkSelector "microphoneInjectionPermission") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setInputMuted:error:@
setInputMuted_errorSelector :: Selector
setInputMuted_errorSelector = mkSelector "setInputMuted:error:"

-- | @Selector@ for @setInputMuteStateChangeHandler:error:@
setInputMuteStateChangeHandler_errorSelector :: Selector
setInputMuteStateChangeHandler_errorSelector = mkSelector "setInputMuteStateChangeHandler:error:"

-- | @Selector@ for @requestRecordPermissionWithCompletionHandler:@
requestRecordPermissionWithCompletionHandlerSelector :: Selector
requestRecordPermissionWithCompletionHandlerSelector = mkSelector "requestRecordPermissionWithCompletionHandler:"

-- | @Selector@ for @requestMicrophoneInjectionPermissionWithCompletionHandler:@
requestMicrophoneInjectionPermissionWithCompletionHandlerSelector :: Selector
requestMicrophoneInjectionPermissionWithCompletionHandlerSelector = mkSelector "requestMicrophoneInjectionPermissionWithCompletionHandler:"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @inputMuted@
inputMutedSelector :: Selector
inputMutedSelector = mkSelector "inputMuted"

-- | @Selector@ for @recordPermission@
recordPermissionSelector :: Selector
recordPermissionSelector = mkSelector "recordPermission"

-- | @Selector@ for @microphoneInjectionPermission@
microphoneInjectionPermissionSelector :: Selector
microphoneInjectionPermissionSelector = mkSelector "microphoneInjectionPermission"

