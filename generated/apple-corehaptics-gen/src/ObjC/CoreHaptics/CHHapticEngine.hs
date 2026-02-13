{-# LANGUAGE DataKinds #-}
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
  , autoShutdownEnabledSelector
  , capabilitiesForHardwareSelector
  , createAdvancedPlayerWithPattern_errorSelector
  , createPlayerWithPattern_errorSelector
  , currentTimeSelector
  , initAndReturnErrorSelector
  , initSelector
  , initWithAudioSession_errorSelector
  , intendedSpatialExperienceSelector
  , isMutedForAudioSelector
  , isMutedForHapticsSelector
  , notifyWhenPlayersFinishedSelector
  , playPatternFromData_errorSelector
  , playPatternFromURL_errorSelector
  , playsAudioOnlySelector
  , playsHapticsOnlySelector
  , registerAudioResource_options_errorSelector
  , resetHandlerSelector
  , setAutoShutdownEnabledSelector
  , setIntendedSpatialExperienceSelector
  , setIsMutedForAudioSelector
  , setIsMutedForHapticsSelector
  , setPlaysAudioOnlySelector
  , setPlaysHapticsOnlySelector
  , setResetHandlerSelector
  , setStoppedHandlerSelector
  , startAndReturnErrorSelector
  , startWithCompletionHandlerSelector
  , stopWithCompletionHandlerSelector
  , stoppedHandlerSelector
  , unregisterAudioResource_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' capabilitiesForHardwareSelector

-- | @- init@
init_ :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO (Id CHHapticEngine)
init_ chHapticEngine =
  sendOwnedMessage chHapticEngine initSelector

-- | initAndReturnError:
--
-- Create an instance of the CHHapticEngine.
--
-- More than one instance may exist within a process.  Each will function independently of the others. 		CHHapticEngines created using this method will be associated with the device's internal haptics hardware system, 		if one exists.  For systems without internal haptics, this method will fail with the error @CHHapticErrorCodeNotSupported@. 		To access engine instances associated with external game controllers, see the GameController framework documentation 		for the @hapticEngines@ property on the GCController class.
--
-- ObjC selector: @- initAndReturnError:@
initAndReturnError :: (IsCHHapticEngine chHapticEngine, IsNSError error_) => chHapticEngine -> error_ -> IO (Id CHHapticEngine)
initAndReturnError chHapticEngine error_ =
  sendOwnedMessage chHapticEngine initAndReturnErrorSelector (toNSError error_)

-- | initWithAudioSession:error
--
-- Create an instance of an CHHapticEngine and associate it with an audio session.  If 'audioSession' is nil, 		the engine will create its own.
--
-- More than one instance may exist within a process.  Each will function independently of the others, but all 		CHHapticEngines which share an audio session will have identical audio behavior with regard to interruptions, etc. 		CHHapticEngines created using this method will be associated with the device's internal haptics hardware system, 		if one exists.  For systems without internal haptics, this method will fail with the error @CHHapticErrorCodeNotSupported@. 		To access engine instances associated with external game controllers, see the GameController framework documentation 		for the @hapticEngines@ property on the GCController class.
--
-- ObjC selector: @- initWithAudioSession:error:@
initWithAudioSession_error :: (IsCHHapticEngine chHapticEngine, IsAVAudioSession audioSession, IsNSError error_) => chHapticEngine -> audioSession -> error_ -> IO (Id CHHapticEngine)
initWithAudioSession_error chHapticEngine audioSession error_ =
  sendOwnedMessage chHapticEngine initWithAudioSession_errorSelector (toAVAudioSession audioSession) (toNSError error_)

-- | startWithCompletionHandler:
--
-- Asynchronously start the engine. The handler will be called when the operation completes.
--
-- The handler is guaranteed to be called on either success or failure.
--
-- ObjC selector: @- startWithCompletionHandler:@
startWithCompletionHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
startWithCompletionHandler chHapticEngine completionHandler =
  sendMessage chHapticEngine startWithCompletionHandlerSelector completionHandler

-- | startAndReturnError:
--
-- Start the engine and block until the engine has started.
--
-- This method will return NO upon failure, and outError will be set to a valid NSError describing the error.
--
-- ObjC selector: @- startAndReturnError:@
startAndReturnError :: (IsCHHapticEngine chHapticEngine, IsNSError outError) => chHapticEngine -> outError -> IO Bool
startAndReturnError chHapticEngine outError =
  sendMessage chHapticEngine startAndReturnErrorSelector (toNSError outError)

-- | stopWithCompletionHandler:
--
-- Asynchronously stop the engine.  The handler will be called when the operation completes.
--
-- The handler is guaranteed to be called on either success or failure.
--
-- ObjC selector: @- stopWithCompletionHandler:@
stopWithCompletionHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
stopWithCompletionHandler chHapticEngine completionHandler =
  sendMessage chHapticEngine stopWithCompletionHandlerSelector completionHandler

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
notifyWhenPlayersFinished chHapticEngine finishedHandler =
  sendMessage chHapticEngine notifyWhenPlayersFinishedSelector finishedHandler

-- | createPlayerWithPattern:error
--
-- Factory method for creating a CHHapticPatternPlayer from a CHHapticPattern.
--
-- @pattern@ — The pattern to be played.
--
-- ObjC selector: @- createPlayerWithPattern:error:@
createPlayerWithPattern_error :: (IsCHHapticEngine chHapticEngine, IsCHHapticPattern pattern_, IsNSError outError) => chHapticEngine -> pattern_ -> outError -> IO RawId
createPlayerWithPattern_error chHapticEngine pattern_ outError =
  sendMessage chHapticEngine createPlayerWithPattern_errorSelector (toCHHapticPattern pattern_) (toNSError outError)

-- | createAdvancedPlayerWithPattern:error
--
-- Factory method for creating a CHHapticAdvancedPatternPlayer from a CHHapticPattern.
--
-- @pattern@ — The pattern to be played.
--
-- ObjC selector: @- createAdvancedPlayerWithPattern:error:@
createAdvancedPlayerWithPattern_error :: (IsCHHapticEngine chHapticEngine, IsCHHapticPattern pattern_, IsNSError outError) => chHapticEngine -> pattern_ -> outError -> IO RawId
createAdvancedPlayerWithPattern_error chHapticEngine pattern_ outError =
  sendMessage chHapticEngine createAdvancedPlayerWithPattern_errorSelector (toCHHapticPattern pattern_) (toNSError outError)

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
registerAudioResource_options_error chHapticEngine resourceURL options outError =
  sendMessage chHapticEngine registerAudioResource_options_errorSelector (toNSURL resourceURL) (toNSDictionary options) (toNSError outError)

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
unregisterAudioResource_error chHapticEngine resourceID outError =
  sendMessage chHapticEngine unregisterAudioResource_errorSelector resourceID (toNSError outError)

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
playPatternFromURL_error chHapticEngine fileURL outError =
  sendMessage chHapticEngine playPatternFromURL_errorSelector (toNSURL fileURL) (toNSError outError)

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
playPatternFromData_error chHapticEngine data_ outError =
  sendMessage chHapticEngine playPatternFromData_errorSelector (toNSData data_) (toNSError outError)

-- | currentTime
--
-- The absolute time from which all current and future event times may be calculated.		The units are seconds.
--
-- ObjC selector: @- currentTime@
currentTime :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO CDouble
currentTime chHapticEngine =
  sendMessage chHapticEngine currentTimeSelector

-- | stoppedHandler
--
-- The engine will call this block when it has stopped due to external causes (such as		an audio session interruption or the app going into the background).  It will NOT be called 		if the client calls stopWithCompletionHandler:.
--
-- In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- stoppedHandler@
stoppedHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO (Ptr ())
stoppedHandler chHapticEngine =
  sendMessage chHapticEngine stoppedHandlerSelector

-- | stoppedHandler
--
-- The engine will call this block when it has stopped due to external causes (such as		an audio session interruption or the app going into the background).  It will NOT be called 		if the client calls stopWithCompletionHandler:.
--
-- In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- setStoppedHandler:@
setStoppedHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
setStoppedHandler chHapticEngine value =
  sendMessage chHapticEngine setStoppedHandlerSelector value

-- | resetHandler
--
-- This block will called asynchronously if the haptic engine has to reset itself after a server failure.
--
-- In response to this handler being called, the client must release all haptic pattern players 		and recreate them.  All CHHapticPattern objects and CHHapticEngine properties will have been preserved. 		In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- resetHandler@
resetHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO (Ptr ())
resetHandler chHapticEngine =
  sendMessage chHapticEngine resetHandlerSelector

-- | resetHandler
--
-- This block will called asynchronously if the haptic engine has to reset itself after a server failure.
--
-- In response to this handler being called, the client must release all haptic pattern players 		and recreate them.  All CHHapticPattern objects and CHHapticEngine properties will have been preserved. 		In general, callbacks arrive on a non-main thread and it is the client's responsibility to handle		it in a thread-safe manner.
--
-- ObjC selector: @- setResetHandler:@
setResetHandler :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Ptr () -> IO ()
setResetHandler chHapticEngine value =
  sendMessage chHapticEngine setResetHandlerSelector value

-- | playsHapticsOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeAudio and play only haptic events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- playsHapticsOnly@
playsHapticsOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
playsHapticsOnly chHapticEngine =
  sendMessage chHapticEngine playsHapticsOnlySelector

-- | playsHapticsOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeAudio and play only haptic events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- setPlaysHapticsOnly:@
setPlaysHapticsOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setPlaysHapticsOnly chHapticEngine value =
  sendMessage chHapticEngine setPlaysHapticsOnlySelector value

-- | playsAudioOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeHaptic and play only audio events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- playsAudioOnly@
playsAudioOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
playsAudioOnly chHapticEngine =
  sendMessage chHapticEngine playsAudioOnlySelector

-- | playsAudioOnly
--
-- If set to YES, the CHHapticEngine will ignore all events of type CHHapticEventTypeHaptic and play only audio events.
--
-- This behavior change will only take effect after the engine is stopped and restarted.		The default is NO.
--
-- ObjC selector: @- setPlaysAudioOnly:@
setPlaysAudioOnly :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setPlaysAudioOnly chHapticEngine value =
  sendMessage chHapticEngine setPlaysAudioOnlySelector value

-- | isMutedForAudio
--
-- When set to YES, the CHHapticEngine mutes audio playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- isMutedForAudio@
isMutedForAudio :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
isMutedForAudio chHapticEngine =
  sendMessage chHapticEngine isMutedForAudioSelector

-- | isMutedForAudio
--
-- When set to YES, the CHHapticEngine mutes audio playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- setIsMutedForAudio:@
setIsMutedForAudio :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setIsMutedForAudio chHapticEngine value =
  sendMessage chHapticEngine setIsMutedForAudioSelector value

-- | isMutedForHaptics
--
-- When set to YES, the CHHapticEngine mutes haptic playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- isMutedForHaptics@
isMutedForHaptics :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO Bool
isMutedForHaptics chHapticEngine =
  sendMessage chHapticEngine isMutedForHapticsSelector

-- | isMutedForHaptics
--
-- When set to YES, the CHHapticEngine mutes haptic playback from its players.
--
-- Default is NO.
--
-- ObjC selector: @- setIsMutedForHaptics:@
setIsMutedForHaptics :: IsCHHapticEngine chHapticEngine => chHapticEngine -> Bool -> IO ()
setIsMutedForHaptics chHapticEngine value =
  sendMessage chHapticEngine setIsMutedForHapticsSelector value

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
autoShutdownEnabled chHapticEngine =
  sendMessage chHapticEngine autoShutdownEnabledSelector

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
setAutoShutdownEnabled chHapticEngine value =
  sendMessage chHapticEngine setAutoShutdownEnabledSelector value

-- | intendedSpatialExperience
--
-- The CHHapticEngine's intended @CASpatialAudioExperience@.
--
-- Only useful for engines that have audio output. If unspecified, the        property value defaults to @CAAutomaticSpatialAudio@.
--
-- ObjC selector: @- intendedSpatialExperience@
intendedSpatialExperience :: IsCHHapticEngine chHapticEngine => chHapticEngine -> IO RawId
intendedSpatialExperience chHapticEngine =
  sendMessage chHapticEngine intendedSpatialExperienceSelector

-- | intendedSpatialExperience
--
-- The CHHapticEngine's intended @CASpatialAudioExperience@.
--
-- Only useful for engines that have audio output. If unspecified, the        property value defaults to @CAAutomaticSpatialAudio@.
--
-- ObjC selector: @- setIntendedSpatialExperience:@
setIntendedSpatialExperience :: IsCHHapticEngine chHapticEngine => chHapticEngine -> RawId -> IO ()
setIntendedSpatialExperience chHapticEngine value =
  sendMessage chHapticEngine setIntendedSpatialExperienceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capabilitiesForHardware@
capabilitiesForHardwareSelector :: Selector '[] RawId
capabilitiesForHardwareSelector = mkSelector "capabilitiesForHardware"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CHHapticEngine)
initSelector = mkSelector "init"

-- | @Selector@ for @initAndReturnError:@
initAndReturnErrorSelector :: Selector '[Id NSError] (Id CHHapticEngine)
initAndReturnErrorSelector = mkSelector "initAndReturnError:"

-- | @Selector@ for @initWithAudioSession:error:@
initWithAudioSession_errorSelector :: Selector '[Id AVAudioSession, Id NSError] (Id CHHapticEngine)
initWithAudioSession_errorSelector = mkSelector "initWithAudioSession:error:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @startAndReturnError:@
startAndReturnErrorSelector :: Selector '[Id NSError] Bool
startAndReturnErrorSelector = mkSelector "startAndReturnError:"

-- | @Selector@ for @stopWithCompletionHandler:@
stopWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
stopWithCompletionHandlerSelector = mkSelector "stopWithCompletionHandler:"

-- | @Selector@ for @notifyWhenPlayersFinished:@
notifyWhenPlayersFinishedSelector :: Selector '[Ptr ()] ()
notifyWhenPlayersFinishedSelector = mkSelector "notifyWhenPlayersFinished:"

-- | @Selector@ for @createPlayerWithPattern:error:@
createPlayerWithPattern_errorSelector :: Selector '[Id CHHapticPattern, Id NSError] RawId
createPlayerWithPattern_errorSelector = mkSelector "createPlayerWithPattern:error:"

-- | @Selector@ for @createAdvancedPlayerWithPattern:error:@
createAdvancedPlayerWithPattern_errorSelector :: Selector '[Id CHHapticPattern, Id NSError] RawId
createAdvancedPlayerWithPattern_errorSelector = mkSelector "createAdvancedPlayerWithPattern:error:"

-- | @Selector@ for @registerAudioResource:options:error:@
registerAudioResource_options_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] CULong
registerAudioResource_options_errorSelector = mkSelector "registerAudioResource:options:error:"

-- | @Selector@ for @unregisterAudioResource:error:@
unregisterAudioResource_errorSelector :: Selector '[CULong, Id NSError] Bool
unregisterAudioResource_errorSelector = mkSelector "unregisterAudioResource:error:"

-- | @Selector@ for @playPatternFromURL:error:@
playPatternFromURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
playPatternFromURL_errorSelector = mkSelector "playPatternFromURL:error:"

-- | @Selector@ for @playPatternFromData:error:@
playPatternFromData_errorSelector :: Selector '[Id NSData, Id NSError] Bool
playPatternFromData_errorSelector = mkSelector "playPatternFromData:error:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector '[] CDouble
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @stoppedHandler@
stoppedHandlerSelector :: Selector '[] (Ptr ())
stoppedHandlerSelector = mkSelector "stoppedHandler"

-- | @Selector@ for @setStoppedHandler:@
setStoppedHandlerSelector :: Selector '[Ptr ()] ()
setStoppedHandlerSelector = mkSelector "setStoppedHandler:"

-- | @Selector@ for @resetHandler@
resetHandlerSelector :: Selector '[] (Ptr ())
resetHandlerSelector = mkSelector "resetHandler"

-- | @Selector@ for @setResetHandler:@
setResetHandlerSelector :: Selector '[Ptr ()] ()
setResetHandlerSelector = mkSelector "setResetHandler:"

-- | @Selector@ for @playsHapticsOnly@
playsHapticsOnlySelector :: Selector '[] Bool
playsHapticsOnlySelector = mkSelector "playsHapticsOnly"

-- | @Selector@ for @setPlaysHapticsOnly:@
setPlaysHapticsOnlySelector :: Selector '[Bool] ()
setPlaysHapticsOnlySelector = mkSelector "setPlaysHapticsOnly:"

-- | @Selector@ for @playsAudioOnly@
playsAudioOnlySelector :: Selector '[] Bool
playsAudioOnlySelector = mkSelector "playsAudioOnly"

-- | @Selector@ for @setPlaysAudioOnly:@
setPlaysAudioOnlySelector :: Selector '[Bool] ()
setPlaysAudioOnlySelector = mkSelector "setPlaysAudioOnly:"

-- | @Selector@ for @isMutedForAudio@
isMutedForAudioSelector :: Selector '[] Bool
isMutedForAudioSelector = mkSelector "isMutedForAudio"

-- | @Selector@ for @setIsMutedForAudio:@
setIsMutedForAudioSelector :: Selector '[Bool] ()
setIsMutedForAudioSelector = mkSelector "setIsMutedForAudio:"

-- | @Selector@ for @isMutedForHaptics@
isMutedForHapticsSelector :: Selector '[] Bool
isMutedForHapticsSelector = mkSelector "isMutedForHaptics"

-- | @Selector@ for @setIsMutedForHaptics:@
setIsMutedForHapticsSelector :: Selector '[Bool] ()
setIsMutedForHapticsSelector = mkSelector "setIsMutedForHaptics:"

-- | @Selector@ for @autoShutdownEnabled@
autoShutdownEnabledSelector :: Selector '[] Bool
autoShutdownEnabledSelector = mkSelector "autoShutdownEnabled"

-- | @Selector@ for @setAutoShutdownEnabled:@
setAutoShutdownEnabledSelector :: Selector '[Bool] ()
setAutoShutdownEnabledSelector = mkSelector "setAutoShutdownEnabled:"

-- | @Selector@ for @intendedSpatialExperience@
intendedSpatialExperienceSelector :: Selector '[] RawId
intendedSpatialExperienceSelector = mkSelector "intendedSpatialExperience"

-- | @Selector@ for @setIntendedSpatialExperience:@
setIntendedSpatialExperienceSelector :: Selector '[RawId] ()
setIntendedSpatialExperienceSelector = mkSelector "setIntendedSpatialExperience:"

