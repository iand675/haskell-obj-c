{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticEngine
--
-- Represents the connection with the haptic server.
--
-- Generated bindings for @CHHapticEngine@.
module ObjC.CoreHaptics.CHHapticEngine
  ( CHHapticEngine
  , IsCHHapticEngine(..)
  , capabilitiesForHardware
  , init_
  , initAndReturnError
  , initWithAudioSession_error
  , startWithCompletionHandler
  , startAndReturnError
  , stopWithCompletionHandler
  , notifyWhenPlayersFinished
  , createPlayerWithPattern_error
  , createAdvancedPlayerWithPattern_error
  , registerAudioResource_options_error
  , unregisterAudioResource_error
  , playPatternFromURL_error
  , playPatternFromData_error
  , currentTime
  , stoppedHandler
  , setStoppedHandler
  , resetHandler
  , setResetHandler
  , playsHapticsOnly
  , setPlaysHapticsOnly
  , playsAudioOnly
  , setPlaysAudioOnly
  , isMutedForAudio
  , setIsMutedForAudio
  , isMutedForHaptics
  , setIsMutedForHaptics
  , autoShutdownEnabled
  , setAutoShutdownEnabled
  , intendedSpatialExperience
  , setIntendedSpatialExperience
  , capabilitiesForHardwareSelector
  , initSelector
  , initAndReturnErrorSelector
  , initWithAudioSession_errorSelector
  , startWithCompletionHandlerSelector
  , startAndReturnErrorSelector
  , stopWithCompletionHandlerSelector
  , notifyWhenPlayersFinishedSelector
  , createPlayerWithPattern_errorSelector
  , createAdvancedPlayerWithPattern_errorSelector
  , registerAudioResource_options_errorSelector
  , unregisterAudioResource_errorSelector
  , playPatternFromURL_errorSelector
  , playPatternFromData_errorSelector
  , currentTimeSelector
  , stoppedHandlerSelector
  , setStoppedHandlerSelector
  , resetHandlerSelector
  , setResetHandlerSelector
  , playsHapticsOnlySelector
  , setPlaysHapticsOnlySelector
  , playsAudioOnlySelector
  , setPlaysAudioOnlySelector
  , isMutedForAudioSelector
  , setIsMutedForAudioSelector
  , isMutedForHapticsSelector
  , setIsMutedForHapticsSelector
  , autoShutdownEnabledSelector
  , setAutoShutdownEnabledSelector
  , intendedSpatialExperienceSelector
  , setIntendedSpatialExperienceSelector


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

import ObjC.CoreHaptics.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | capabilitiesForHardware
--
-- Get the protocol that describes haptic and audio capabilities on this device.
--
-- Detailed description on the capability protocol is in CHHapticDeviceCapability.h.
--
-- ObjC selector: @+ capabilitiesForHardware@
capabilitiesForHardware :: IO RawId
capabilitiesForHardware  =
  do
    cls' <- getRequiredClass "CHHapticEngine"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "capabilitiesForHardware") (retPtr retVoid) []

-- | @- init@
init_ :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO (Id CHHapticEngine)
init_ chHapticEngine  =
    sendMsg chHapticEngine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initAndReturnError:
--
-- Create an instance of the CHHapticEngine.
--
-- More than one instance may exist within a process.  Each will function independently of the others. 		CHHapticEngines created using this method will be associated with the device's internal haptics hardware system, 		if one exists.  For systems without internal haptics, this method will fail with the error @CHHapticErrorCodeNotSupported@. 		To access engine instances associated with external game controllers, see the GameController framework documentation 		for the @hapticEngines@ property on the GCController class.
--
-- ObjC selector: @- initAndReturnError:@
initAndReturnError :: (IsCHHapticEngine chHapticEngine, IsNSError error_) => chHapticEngine -> error_ -> IO (Id CHHapticEngine)
initAndReturnError chHapticEngine  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg chHapticEngine (mkSelector "initAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | initWithAudioSession:error
--
-- Create an instance of an CHHapticEngine and associate it with an audio session.  If 'audioSession' is nil, 		the engine will create its own.
--
-- More than one instance may exist within a process.  Each will function independently of the others, but all 		CHHapticEngines which share an audio session will have identical audio behavior with regard to interruptions, etc. 		CHHapticEngines created using this method will be associated with the device's internal haptics hardware system, 		if one exists.  For systems without internal haptics, this method will fail with the error @CHHapticErrorCodeNotSupported@. 		To access engine instances associated with external game controllers, see the GameController framework documentation 		for the @hapticEngines@ property on the GCController class.
--
-- ObjC selector: @- initWithAudioSession:error:@
initWithAudioSession_error :: (IsCHHapticEngine chHapticEngine, IsAVAudioSession audioSession, IsNSError error_) => chHapticEngine -> audioSession -> error_ -> IO (Id CHHapticEngine)
initWithAudioSession_error chHapticEngine  audioSession error_ =
  withObjCPtr audioSession $ \raw_audioSession ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg chHapticEngine (mkSelector "initWithAudioSession:error:") (retPtr retVoid) [argPtr (castPtr raw_audioSession :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | startWithCompletionHandler:
--
-- Asynchronously start the engine. The handler will be called when the operation completes.
--
-- The handler is guaranteed to be called on either success or failure.
--
-- ObjC selector: @- startWithCompletionHandler:@
startWithCompletionHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
startWithCompletionHandler chHapticEngine  completionHandler =
    sendMsg chHapticEngine (mkSelector "startWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | startAndReturnError:
--
-- Start the engine and block until the engine has started.
--
-- This method will return NO upon failure, and outError will be set to a valid NSError describing the error.
--
-- ObjC selector: @- startAndReturnError:@
startAndReturnError :: (IsCHHapticEngine chHapticEngine, IsNSError outError) => chHapticEngine -> outError -> IO Bool
startAndReturnError chHapticEngine  outError =
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "startAndReturnError:") retCULong [argPtr (castPtr raw_outError :: Ptr ())]

-- | stopWithCompletionHandler:
--
-- Asynchronously stop the engine.  The handler will be called when the operation completes.
--
-- The handler is guaranteed to be called on either success or failure.
--
-- ObjC selector: @- stopWithCompletionHandler:@
stopWithCompletionHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
stopWithCompletionHandler chHapticEngine  completionHandler =
    sendMsg chHapticEngine (mkSelector "stopWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | notifyWhenPlayersFinished:
--
-- Tell the engine to asynchronously call the passed-in handler when all active pattern players associated 		with this engine have stopped.
--
-- @finishedHandler@ — The block that will be called asynchronously.  The return value of this block determines the action the 		engine will take when the block finishes (see @CHHapticEngineFinishedHandler@).
--
-- If additional players are started after this call is made, they will delay the callback. 		If no players are active or the engine is stopped, the callback will happen immediately.
--
-- ObjC selector: @- notifyWhenPlayersFinished:@
notifyWhenPlayersFinished :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
notifyWhenPlayersFinished chHapticEngine  finishedHandler =
    sendMsg chHapticEngine (mkSelector "notifyWhenPlayersFinished:") retVoid [argPtr (castPtr finishedHandler :: Ptr ())]

-- | createPlayerWithPattern:error
--
-- Factory method for creating a CHHapticPatternPlayer from a CHHapticPattern.
--
-- @pattern@ — The pattern to be played.
--
-- ObjC selector: @- createPlayerWithPattern:error:@
createPlayerWithPattern_error :: (IsCHHapticEngine chHapticEngine, IsCHHapticPattern pattern_, IsNSError outError) => chHapticEngine -> pattern_ -> outError -> IO RawId
createPlayerWithPattern_error chHapticEngine  pattern_ outError =
  withObjCPtr pattern_ $ \raw_pattern_ ->
    withObjCPtr outError $ \raw_outError ->
        fmap (RawId . castPtr) $ sendMsg chHapticEngine (mkSelector "createPlayerWithPattern:error:") (retPtr retVoid) [argPtr (castPtr raw_pattern_ :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | createAdvancedPlayerWithPattern:error
--
-- Factory method for creating a CHHapticAdvancedPatternPlayer from a CHHapticPattern.
--
-- @pattern@ — The pattern to be played.
--
-- ObjC selector: @- createAdvancedPlayerWithPattern:error:@
createAdvancedPlayerWithPattern_error :: (IsCHHapticEngine chHapticEngine, IsCHHapticPattern pattern_, IsNSError outError) => chHapticEngine -> pattern_ -> outError -> IO RawId
createAdvancedPlayerWithPattern_error chHapticEngine  pattern_ outError =
  withObjCPtr pattern_ $ \raw_pattern_ ->
    withObjCPtr outError $ \raw_outError ->
        fmap (RawId . castPtr) $ sendMsg chHapticEngine (mkSelector "createAdvancedPlayerWithPattern:error:") (retPtr retVoid) [argPtr (castPtr raw_pattern_ :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | registerAudioResource:options:error
--
-- Register an external audio file for use as a custom waveform.
--
-- @resourceURL@ — A URL referencing the location of the audio file to be registered.
--
-- @options@ — A dictionary containing CHHapticAudioResourceKey/value pairs describing how this resource should be played.
--
-- @outError@ — If register operation fails, this will be set to a valid NSError describing the error.
--
-- ObjC selector: @- registerAudioResource:options:error:@
registerAudioResource_options_error :: (IsCHHapticEngine chHapticEngine, IsNSURL resourceURL, IsNSDictionary options, IsNSError outError) => chHapticEngine -> resourceURL -> options -> outError -> IO CULong
registerAudioResource_options_error chHapticEngine  resourceURL options outError =
  withObjCPtr resourceURL $ \raw_resourceURL ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg chHapticEngine (mkSelector "registerAudioResource:options:error:") retCULong [argPtr (castPtr raw_resourceURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | unregisterAudioResource:error
--
-- Unregister and remove a previously-registered audio resource.
--
-- @resourceID@ — The resource ID that was returned when the resource was registered.
--
-- @outError@ — If the unregister operation fails, this will be set to a valid NSError describing the error.
--
-- ObjC selector: @- unregisterAudioResource:error:@
unregisterAudioResource_error :: (IsCHHapticEngine chHapticEngine, IsNSError outError) => chHapticEngine -> CULong -> outError -> IO Bool
unregisterAudioResource_error chHapticEngine  resourceID outError =
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "unregisterAudioResource:error:") retCULong [argCULong resourceID, argPtr (castPtr raw_outError :: Ptr ())]

-- | playPatternFromURL:error
--
-- Simple one-shot call to play a pattern specified by a URL.
--
-- @fileURL@ — The URL of the file containing a haptic/audio pattern dictionary.
--
-- @outError@ — If the operation fails, this will be set to a valid NSError describing the error.
--
-- The engine should be started prior to calling this method if low latency is desired. If this is not done, 		this method will start it, which can cause a significant delay.
--
-- ObjC selector: @- playPatternFromURL:error:@
playPatternFromURL_error :: (IsCHHapticEngine chHapticEngine, IsNSURL fileURL, IsNSError outError) => chHapticEngine -> fileURL -> outError -> IO Bool
playPatternFromURL_error chHapticEngine  fileURL outError =
  withObjCPtr fileURL $ \raw_fileURL ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "playPatternFromURL:error:") retCULong [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | playPatternFromData:error
--
-- Simple one-shot call to play a pattern specified by NSData.
--
-- @data@ — The NSData containing a haptic/audio pattern dictionary.
--
-- @outError@ — If the operation fails, this will be set to a valid NSError describing the error.
--
-- The engine should be started prior to calling this method if low latency is desired. If this is not done, 		this method will start it, which can cause a significant delay.
--
-- ObjC selector: @- playPatternFromData:error:@
playPatternFromData_error :: (IsCHHapticEngine chHapticEngine, IsNSData data_, IsNSError outError) => chHapticEngine -> data_ -> outError -> IO Bool
playPatternFromData_error chHapticEngine  data_ outError =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "playPatternFromData:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | currentTime
--
-- The absolute time from which all current and future event times may be calculated.		The units are seconds.
--
-- ObjC selector: @- currentTime@
currentTime :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO CDouble
currentTime chHapticEngine  =
    sendMsg chHapticEngine (mkSelector "currentTime") retCDouble []

-- | stoppedHandler
--
-- The engine will call this block when it has stopped due to external causes (such as		an audio session interruption or the app going into the background).  It will NOT be called 		if the client calls stopWithCompletionHandler:.
--
-- In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- stoppedHandler@
stoppedHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO (Ptr ())
stoppedHandler chHapticEngine  =
    fmap castPtr $ sendMsg chHapticEngine (mkSelector "stoppedHandler") (retPtr retVoid) []

-- | stoppedHandler
--
-- The engine will call this block when it has stopped due to external causes (such as		an audio session interruption or the app going into the background).  It will NOT be called 		if the client calls stopWithCompletionHandler:.
--
-- In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- setStoppedHandler:@
setStoppedHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
setStoppedHandler chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setStoppedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | resetHandler
--
-- This block will called asynchronously if the haptic engine has to reset itself after a server failure.
--
-- In response to this handler being called, the client must release all haptic pattern players 		and recreate them.  All CHHapticPattern objects and CHHapticEngine properties will have been preserved. 		In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- resetHandler@
resetHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO (Ptr ())
resetHandler chHapticEngine  =
    fmap castPtr $ sendMsg chHapticEngine (mkSelector "resetHandler") (retPtr retVoid) []

-- | resetHandler
--
-- This block will called asynchronously if the haptic engine has to reset itself after a server failure.
--
-- In response to this handler being called, the client must release all haptic pattern players 		and recreate them.  All CHHapticPattern objects and CHHapticEngine properties will have been preserved. 		In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- setResetHandler:@
setResetHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
setResetHandler chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setResetHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | playsHapticsOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeAudio and play only haptic events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- playsHapticsOnly@
playsHapticsOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
playsHapticsOnly chHapticEngine  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "playsHapticsOnly") retCULong []

-- | playsHapticsOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeAudio and play only haptic events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- setPlaysHapticsOnly:@
setPlaysHapticsOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setPlaysHapticsOnly chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setPlaysHapticsOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | playsAudioOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeHaptic and play only audio events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- playsAudioOnly@
playsAudioOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
playsAudioOnly chHapticEngine  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "playsAudioOnly") retCULong []

-- | playsAudioOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeHaptic and play only audio events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- setPlaysAudioOnly:@
setPlaysAudioOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setPlaysAudioOnly chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setPlaysAudioOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | isMutedForAudio
--
-- When set to YES, the CHHapticEngine mutes audio playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- isMutedForAudio@
isMutedForAudio :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
isMutedForAudio chHapticEngine  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "isMutedForAudio") retCULong []

-- | isMutedForAudio
--
-- When set to YES, the CHHapticEngine mutes audio playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- setIsMutedForAudio:@
setIsMutedForAudio :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setIsMutedForAudio chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setIsMutedForAudio:") retVoid [argCULong (if value then 1 else 0)]

-- | isMutedForHaptics
--
-- When set to YES, the CHHapticEngine mutes haptic playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- isMutedForHaptics@
isMutedForHaptics :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
isMutedForHaptics chHapticEngine  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "isMutedForHaptics") retCULong []

-- | isMutedForHaptics
--
-- When set to YES, the CHHapticEngine mutes haptic playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- setIsMutedForHaptics:@
setIsMutedForHaptics :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setIsMutedForHaptics chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setIsMutedForHaptics:") retVoid [argCULong (if value then 1 else 0)]

-- | autoShutdownEnabled
--
-- When auto shutdown is enabled, the haptic engine can start and stop the hardware dynamically,		to conserve power.
--
-- To conserve power, it is advised that the client stop the haptic engine when not in use.		But when auto shutdown is enabled, the haptic engine will stop the hardware if it was running		idle for a certain duration, and restart it later when required.		Note that, because this operation is dynamic, it may affect the start times of the pattern players		(e.g. @CHHapticPatternplayer@), if the engine has to resume from its shutdown state.
--
-- This feature is disabled by default, but the client can enable it if needed.
--
-- ObjC selector: @- autoShutdownEnabled@
autoShutdownEnabled :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
autoShutdownEnabled chHapticEngine  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg chHapticEngine (mkSelector "autoShutdownEnabled") retCULong []

-- | autoShutdownEnabled
--
-- When auto shutdown is enabled, the haptic engine can start and stop the hardware dynamically,		to conserve power.
--
-- To conserve power, it is advised that the client stop the haptic engine when not in use.		But when auto shutdown is enabled, the haptic engine will stop the hardware if it was running		idle for a certain duration, and restart it later when required.		Note that, because this operation is dynamic, it may affect the start times of the pattern players		(e.g. @CHHapticPatternplayer@), if the engine has to resume from its shutdown state.
--
-- This feature is disabled by default, but the client can enable it if needed.
--
-- ObjC selector: @- setAutoShutdownEnabled:@
setAutoShutdownEnabled :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setAutoShutdownEnabled chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setAutoShutdownEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | intendedSpatialExperience
--
-- The CHHapticEngine's intended @CASpatialAudioExperience@.
--
-- Only useful for engines that have audio output. If unspecified, the        property value defaults to @CAAutomaticSpatialAudio@.
--
-- ObjC selector: @- intendedSpatialExperience@
intendedSpatialExperience :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO RawId
intendedSpatialExperience chHapticEngine  =
    fmap (RawId . castPtr) $ sendMsg chHapticEngine (mkSelector "intendedSpatialExperience") (retPtr retVoid) []

-- | intendedSpatialExperience
--
-- The CHHapticEngine's intended @CASpatialAudioExperience@.
--
-- Only useful for engines that have audio output. If unspecified, the        property value defaults to @CAAutomaticSpatialAudio@.
--
-- ObjC selector: @- setIntendedSpatialExperience:@
setIntendedSpatialExperience :: IsCHHapticEngine chHapticEngine => chHapticEngine -> RawId -> IO ()
setIntendedSpatialExperience chHapticEngine  value =
    sendMsg chHapticEngine (mkSelector "setIntendedSpatialExperience:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capabilitiesForHardware@
capabilitiesForHardwareSelector :: Selector
capabilitiesForHardwareSelector = mkSelector "capabilitiesForHardware"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initAndReturnError:@
initAndReturnErrorSelector :: Selector
initAndReturnErrorSelector = mkSelector "initAndReturnError:"

-- | @Selector@ for @initWithAudioSession:error:@
initWithAudioSession_errorSelector :: Selector
initWithAudioSession_errorSelector = mkSelector "initWithAudioSession:error:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @startAndReturnError:@
startAndReturnErrorSelector :: Selector
startAndReturnErrorSelector = mkSelector "startAndReturnError:"

-- | @Selector@ for @stopWithCompletionHandler:@
stopWithCompletionHandlerSelector :: Selector
stopWithCompletionHandlerSelector = mkSelector "stopWithCompletionHandler:"

-- | @Selector@ for @notifyWhenPlayersFinished:@
notifyWhenPlayersFinishedSelector :: Selector
notifyWhenPlayersFinishedSelector = mkSelector "notifyWhenPlayersFinished:"

-- | @Selector@ for @createPlayerWithPattern:error:@
createPlayerWithPattern_errorSelector :: Selector
createPlayerWithPattern_errorSelector = mkSelector "createPlayerWithPattern:error:"

-- | @Selector@ for @createAdvancedPlayerWithPattern:error:@
createAdvancedPlayerWithPattern_errorSelector :: Selector
createAdvancedPlayerWithPattern_errorSelector = mkSelector "createAdvancedPlayerWithPattern:error:"

-- | @Selector@ for @registerAudioResource:options:error:@
registerAudioResource_options_errorSelector :: Selector
registerAudioResource_options_errorSelector = mkSelector "registerAudioResource:options:error:"

-- | @Selector@ for @unregisterAudioResource:error:@
unregisterAudioResource_errorSelector :: Selector
unregisterAudioResource_errorSelector = mkSelector "unregisterAudioResource:error:"

-- | @Selector@ for @playPatternFromURL:error:@
playPatternFromURL_errorSelector :: Selector
playPatternFromURL_errorSelector = mkSelector "playPatternFromURL:error:"

-- | @Selector@ for @playPatternFromData:error:@
playPatternFromData_errorSelector :: Selector
playPatternFromData_errorSelector = mkSelector "playPatternFromData:error:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @stoppedHandler@
stoppedHandlerSelector :: Selector
stoppedHandlerSelector = mkSelector "stoppedHandler"

-- | @Selector@ for @setStoppedHandler:@
setStoppedHandlerSelector :: Selector
setStoppedHandlerSelector = mkSelector "setStoppedHandler:"

-- | @Selector@ for @resetHandler@
resetHandlerSelector :: Selector
resetHandlerSelector = mkSelector "resetHandler"

-- | @Selector@ for @setResetHandler:@
setResetHandlerSelector :: Selector
setResetHandlerSelector = mkSelector "setResetHandler:"

-- | @Selector@ for @playsHapticsOnly@
playsHapticsOnlySelector :: Selector
playsHapticsOnlySelector = mkSelector "playsHapticsOnly"

-- | @Selector@ for @setPlaysHapticsOnly:@
setPlaysHapticsOnlySelector :: Selector
setPlaysHapticsOnlySelector = mkSelector "setPlaysHapticsOnly:"

-- | @Selector@ for @playsAudioOnly@
playsAudioOnlySelector :: Selector
playsAudioOnlySelector = mkSelector "playsAudioOnly"

-- | @Selector@ for @setPlaysAudioOnly:@
setPlaysAudioOnlySelector :: Selector
setPlaysAudioOnlySelector = mkSelector "setPlaysAudioOnly:"

-- | @Selector@ for @isMutedForAudio@
isMutedForAudioSelector :: Selector
isMutedForAudioSelector = mkSelector "isMutedForAudio"

-- | @Selector@ for @setIsMutedForAudio:@
setIsMutedForAudioSelector :: Selector
setIsMutedForAudioSelector = mkSelector "setIsMutedForAudio:"

-- | @Selector@ for @isMutedForHaptics@
isMutedForHapticsSelector :: Selector
isMutedForHapticsSelector = mkSelector "isMutedForHaptics"

-- | @Selector@ for @setIsMutedForHaptics:@
setIsMutedForHapticsSelector :: Selector
setIsMutedForHapticsSelector = mkSelector "setIsMutedForHaptics:"

-- | @Selector@ for @autoShutdownEnabled@
autoShutdownEnabledSelector :: Selector
autoShutdownEnabledSelector = mkSelector "autoShutdownEnabled"

-- | @Selector@ for @setAutoShutdownEnabled:@
setAutoShutdownEnabledSelector :: Selector
setAutoShutdownEnabledSelector = mkSelector "setAutoShutdownEnabled:"

-- | @Selector@ for @intendedSpatialExperience@
intendedSpatialExperienceSelector :: Selector
intendedSpatialExperienceSelector = mkSelector "intendedSpatialExperience"

-- | @Selector@ for @setIntendedSpatialExperience:@
setIntendedSpatialExperienceSelector :: Selector
setIntendedSpatialExperienceSelector = mkSelector "setIntendedSpatialExperience:"

