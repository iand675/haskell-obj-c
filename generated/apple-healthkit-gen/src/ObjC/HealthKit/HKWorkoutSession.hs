{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutSession
--
-- An HKWorkoutSession is an object describing the properties of a workout activity session.
--
-- Generated bindings for @HKWorkoutSession@.
module ObjC.HealthKit.HKWorkoutSession
  ( HKWorkoutSession
  , IsHKWorkoutSession(..)
  , initWithActivityType_locationType
  , initWithConfiguration_error
  , initWithHealthStore_configuration_error
  , init_
  , prepare
  , startActivityWithDate
  , stopActivityWithDate
  , end
  , pause
  , resume
  , associatedWorkoutBuilder
  , beginNewActivityWithConfiguration_date_metadata
  , endCurrentActivityOnDate
  , startMirroringToCompanionDeviceWithCompletion
  , stopMirroringToCompanionDeviceWithCompletion
  , sendDataToRemoteWorkoutSession_completion
  , activityType
  , locationType
  , workoutConfiguration
  , delegate
  , setDelegate
  , state
  , type_
  , startDate
  , endDate
  , currentActivity
  , initWithActivityType_locationTypeSelector
  , initWithConfiguration_errorSelector
  , initWithHealthStore_configuration_errorSelector
  , initSelector
  , prepareSelector
  , startActivityWithDateSelector
  , stopActivityWithDateSelector
  , endSelector
  , pauseSelector
  , resumeSelector
  , associatedWorkoutBuilderSelector
  , beginNewActivityWithConfiguration_date_metadataSelector
  , endCurrentActivityOnDateSelector
  , startMirroringToCompanionDeviceWithCompletionSelector
  , stopMirroringToCompanionDeviceWithCompletionSelector
  , sendDataToRemoteWorkoutSession_completionSelector
  , activityTypeSelector
  , locationTypeSelector
  , workoutConfigurationSelector
  , delegateSelector
  , setDelegateSelector
  , stateSelector
  , typeSelector
  , startDateSelector
  , endDateSelector
  , currentActivitySelector

  -- * Enum types
  , HKWorkoutActivityType(HKWorkoutActivityType)
  , pattern HKWorkoutActivityTypeAmericanFootball
  , pattern HKWorkoutActivityTypeArchery
  , pattern HKWorkoutActivityTypeAustralianFootball
  , pattern HKWorkoutActivityTypeBadminton
  , pattern HKWorkoutActivityTypeBaseball
  , pattern HKWorkoutActivityTypeBasketball
  , pattern HKWorkoutActivityTypeBowling
  , pattern HKWorkoutActivityTypeBoxing
  , pattern HKWorkoutActivityTypeClimbing
  , pattern HKWorkoutActivityTypeCricket
  , pattern HKWorkoutActivityTypeCrossTraining
  , pattern HKWorkoutActivityTypeCurling
  , pattern HKWorkoutActivityTypeCycling
  , pattern HKWorkoutActivityTypeDance
  , pattern HKWorkoutActivityTypeDanceInspiredTraining
  , pattern HKWorkoutActivityTypeElliptical
  , pattern HKWorkoutActivityTypeEquestrianSports
  , pattern HKWorkoutActivityTypeFencing
  , pattern HKWorkoutActivityTypeFishing
  , pattern HKWorkoutActivityTypeFunctionalStrengthTraining
  , pattern HKWorkoutActivityTypeGolf
  , pattern HKWorkoutActivityTypeGymnastics
  , pattern HKWorkoutActivityTypeHandball
  , pattern HKWorkoutActivityTypeHiking
  , pattern HKWorkoutActivityTypeHockey
  , pattern HKWorkoutActivityTypeHunting
  , pattern HKWorkoutActivityTypeLacrosse
  , pattern HKWorkoutActivityTypeMartialArts
  , pattern HKWorkoutActivityTypeMindAndBody
  , pattern HKWorkoutActivityTypeMixedMetabolicCardioTraining
  , pattern HKWorkoutActivityTypePaddleSports
  , pattern HKWorkoutActivityTypePlay
  , pattern HKWorkoutActivityTypePreparationAndRecovery
  , pattern HKWorkoutActivityTypeRacquetball
  , pattern HKWorkoutActivityTypeRowing
  , pattern HKWorkoutActivityTypeRugby
  , pattern HKWorkoutActivityTypeRunning
  , pattern HKWorkoutActivityTypeSailing
  , pattern HKWorkoutActivityTypeSkatingSports
  , pattern HKWorkoutActivityTypeSnowSports
  , pattern HKWorkoutActivityTypeSoccer
  , pattern HKWorkoutActivityTypeSoftball
  , pattern HKWorkoutActivityTypeSquash
  , pattern HKWorkoutActivityTypeStairClimbing
  , pattern HKWorkoutActivityTypeSurfingSports
  , pattern HKWorkoutActivityTypeSwimming
  , pattern HKWorkoutActivityTypeTableTennis
  , pattern HKWorkoutActivityTypeTennis
  , pattern HKWorkoutActivityTypeTrackAndField
  , pattern HKWorkoutActivityTypeTraditionalStrengthTraining
  , pattern HKWorkoutActivityTypeVolleyball
  , pattern HKWorkoutActivityTypeWalking
  , pattern HKWorkoutActivityTypeWaterFitness
  , pattern HKWorkoutActivityTypeWaterPolo
  , pattern HKWorkoutActivityTypeWaterSports
  , pattern HKWorkoutActivityTypeWrestling
  , pattern HKWorkoutActivityTypeYoga
  , pattern HKWorkoutActivityTypeBarre
  , pattern HKWorkoutActivityTypeCoreTraining
  , pattern HKWorkoutActivityTypeCrossCountrySkiing
  , pattern HKWorkoutActivityTypeDownhillSkiing
  , pattern HKWorkoutActivityTypeFlexibility
  , pattern HKWorkoutActivityTypeHighIntensityIntervalTraining
  , pattern HKWorkoutActivityTypeJumpRope
  , pattern HKWorkoutActivityTypeKickboxing
  , pattern HKWorkoutActivityTypePilates
  , pattern HKWorkoutActivityTypeSnowboarding
  , pattern HKWorkoutActivityTypeStairs
  , pattern HKWorkoutActivityTypeStepTraining
  , pattern HKWorkoutActivityTypeWheelchairWalkPace
  , pattern HKWorkoutActivityTypeWheelchairRunPace
  , pattern HKWorkoutActivityTypeTaiChi
  , pattern HKWorkoutActivityTypeMixedCardio
  , pattern HKWorkoutActivityTypeHandCycling
  , pattern HKWorkoutActivityTypeDiscSports
  , pattern HKWorkoutActivityTypeFitnessGaming
  , pattern HKWorkoutActivityTypeCardioDance
  , pattern HKWorkoutActivityTypeSocialDance
  , pattern HKWorkoutActivityTypePickleball
  , pattern HKWorkoutActivityTypeCooldown
  , pattern HKWorkoutActivityTypeSwimBikeRun
  , pattern HKWorkoutActivityTypeTransition
  , pattern HKWorkoutActivityTypeUnderwaterDiving
  , pattern HKWorkoutActivityTypeOther
  , HKWorkoutSessionLocationType(HKWorkoutSessionLocationType)
  , pattern HKWorkoutSessionLocationTypeUnknown
  , pattern HKWorkoutSessionLocationTypeIndoor
  , pattern HKWorkoutSessionLocationTypeOutdoor
  , HKWorkoutSessionState(HKWorkoutSessionState)
  , pattern HKWorkoutSessionStateNotStarted
  , pattern HKWorkoutSessionStateRunning
  , pattern HKWorkoutSessionStateEnded
  , pattern HKWorkoutSessionStatePaused
  , pattern HKWorkoutSessionStatePrepared
  , pattern HKWorkoutSessionStateStopped
  , HKWorkoutSessionType(HKWorkoutSessionType)
  , pattern HKWorkoutSessionTypePrimary
  , pattern HKWorkoutSessionTypeMirrored

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

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithActivityType:locationType:
--
-- @activityType@ — The activity type of the workout session.
--
-- @locationType@ — The type of location where the workout will be performed.
--
-- ObjC selector: @- initWithActivityType:locationType:@
initWithActivityType_locationType :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> HKWorkoutActivityType -> HKWorkoutSessionLocationType -> IO (Id HKWorkoutSession)
initWithActivityType_locationType hkWorkoutSession  activityType locationType =
    sendMsg hkWorkoutSession (mkSelector "initWithActivityType:locationType:") (retPtr retVoid) [argCULong (coerce activityType), argCLong (coerce locationType)] >>= ownedObject . castPtr

-- | initWithConfiguration:error:
--
-- @workoutConfiguration@ — Configuration object describing the various properties of a workout.
--
-- @error@ — If the configuration does not specify valid configuration properties, an                                     an NSError describing the error is set and nil is returned.
--
-- ObjC selector: @- initWithConfiguration:error:@
initWithConfiguration_error :: (IsHKWorkoutSession hkWorkoutSession, IsHKWorkoutConfiguration workoutConfiguration, IsNSError error_) => hkWorkoutSession -> workoutConfiguration -> error_ -> IO (Id HKWorkoutSession)
initWithConfiguration_error hkWorkoutSession  workoutConfiguration error_ =
  withObjCPtr workoutConfiguration $ \raw_workoutConfiguration ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg hkWorkoutSession (mkSelector "initWithConfiguration:error:") (retPtr retVoid) [argPtr (castPtr raw_workoutConfiguration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | initWithHealthStore:configuration:error:
--
-- @healthStore@ — Specifies the HKHealthStore object to use.
--
-- @workoutConfiguration@ — Configuration object describing the various properties of a workout.
--
-- @error@ — If the configuration does not specify valid configuration properties, an                                     an NSError describing the error is set and nil is returned.
--
-- ObjC selector: @- initWithHealthStore:configuration:error:@
initWithHealthStore_configuration_error :: (IsHKWorkoutSession hkWorkoutSession, IsHKHealthStore healthStore, IsHKWorkoutConfiguration workoutConfiguration, IsNSError error_) => hkWorkoutSession -> healthStore -> workoutConfiguration -> error_ -> IO (Id HKWorkoutSession)
initWithHealthStore_configuration_error hkWorkoutSession  healthStore workoutConfiguration error_ =
  withObjCPtr healthStore $ \raw_healthStore ->
    withObjCPtr workoutConfiguration $ \raw_workoutConfiguration ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg hkWorkoutSession (mkSelector "initWithHealthStore:configuration:error:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ()), argPtr (castPtr raw_workoutConfiguration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO (Id HKWorkoutSession)
init_ hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | prepare
--
-- Prepares the workout session for starting.
--
-- This method will asynchronously prepare the workout session for starting. The state of the workout session                will transition to HKWorkoutSessionStatePrepared. A prepared session will put the system in session mode,                but will not start the session activity.                You might call this methods, for example, prior to displaying a countdown on your application while waiting                for the activity to start.
--
-- ObjC selector: @- prepare@
prepare :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO ()
prepare hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "prepare") retVoid []

-- | startActivityWithDate:
--
-- @date@ — Start date for the workout session activity
--
-- Starts the workout session activity.
--
-- This method will asynchronously begin the workout session activity. The state of the workout session will                transition to HKWorkoutSessionStateRunning. Once a session activity is started the system will be in session                mode and sensor algorithms will be applied to generate data for the workout activity.
--
-- ObjC selector: @- startActivityWithDate:@
startActivityWithDate :: (IsHKWorkoutSession hkWorkoutSession, IsNSDate date) => hkWorkoutSession -> date -> IO ()
startActivityWithDate hkWorkoutSession  date =
  withObjCPtr date $ \raw_date ->
      sendMsg hkWorkoutSession (mkSelector "startActivityWithDate:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | stopActivityWithDate:
--
-- @date@ — Stop date for the workout session activity
--
-- Stops the workout session activity.
--
-- This method will asynchronously stop the session activity if it is currently running. The state of the workout                session will transition to HKWorkoutSessionStateStopped. Once a workout session is stopped, it cannot be reused to                start a new workout session. Sensor algorithms will be stopped and no new data will be generated for this session.                However, the system will remain in session mode.
--
-- ObjC selector: @- stopActivityWithDate:@
stopActivityWithDate :: (IsHKWorkoutSession hkWorkoutSession, IsNSDate date) => hkWorkoutSession -> date -> IO ()
stopActivityWithDate hkWorkoutSession  date =
  withObjCPtr date $ \raw_date ->
      sendMsg hkWorkoutSession (mkSelector "stopActivityWithDate:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | end
--
-- Ends the workout session.
--
-- This method will end the session if it is currently running or stopped. The state of the workout session will                transition to HKWorkoutSessionStateEnded. Once a workout session is ended, it cannot be reused to start a new                workout session. Sensor algorithms will be stopped, no new data will be generated for this session, and the                system will exit session mode.
--
-- ObjC selector: @- end@
end :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO ()
end hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "end") retVoid []

-- | pause
--
-- Pauses the workout session.
--
-- This method will pause the session if it is currently running. The state of the workout session                will transition to HKWorkoutSessionStatePaused. An HKWorkoutEventTypePause will be generated and                delivered to the workout session's delegate.
--
-- ObjC selector: @- pause@
pause :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO ()
pause hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "pause") retVoid []

-- | resume
--
-- Resumes the workout session.
--
-- This method will resume the session if it is currently paused. The state of the workout session                will transition to HKWorkoutSessionStateRunning. An HKWorkoutEventTypeResume will be generated and                delivered to the workout session's delegate.
--
-- ObjC selector: @- resume@
resume :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO ()
resume hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "resume") retVoid []

-- | associatedWorkoutBuilder
--
-- Retrieves (and creates if necessary) an HKLiveWorkoutBuilder associated with this session.
--
-- A session may have associated with it an HKLiveWorkoutBuilder that will be used to record the workout                for this session. This method will return the session's associated builder, creating it if needed.                Calling this method more than once will return the previously-created builder. If this session was not                initialized with initWithHealthStore:configuration:error:, an exception will be thrown.
--
-- ObjC selector: @- associatedWorkoutBuilder@
associatedWorkoutBuilder :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO (Id HKLiveWorkoutBuilder)
associatedWorkoutBuilder hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "associatedWorkoutBuilder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | beginNewActivityWithConfiguration:date:metadata:
--
-- Begins a new workout activity for this session.
--
-- This method will asynchronously begin the workout activity. The delegate for this session would be                informed once the activity effectively begins.  Sensor algorithms to generate data would be updated                to match the new activity.
--
-- ObjC selector: @- beginNewActivityWithConfiguration:date:metadata:@
beginNewActivityWithConfiguration_date_metadata :: (IsHKWorkoutSession hkWorkoutSession, IsHKWorkoutConfiguration workoutConfiguration, IsNSDate date, IsNSDictionary metadata) => hkWorkoutSession -> workoutConfiguration -> date -> metadata -> IO ()
beginNewActivityWithConfiguration_date_metadata hkWorkoutSession  workoutConfiguration date metadata =
  withObjCPtr workoutConfiguration $ \raw_workoutConfiguration ->
    withObjCPtr date $ \raw_date ->
      withObjCPtr metadata $ \raw_metadata ->
          sendMsg hkWorkoutSession (mkSelector "beginNewActivityWithConfiguration:date:metadata:") retVoid [argPtr (castPtr raw_workoutConfiguration :: Ptr ()), argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())]

-- | endCurrentActivityOnDate:
--
-- Ends the current workout activity.
--
-- This method will end the current activity, reverting to the main session activity. The delegate for this session                would be informed once the activity effectively ends. Sensor algorithms to generate data would be updated to                match the main session activity.
--
-- ObjC selector: @- endCurrentActivityOnDate:@
endCurrentActivityOnDate :: (IsHKWorkoutSession hkWorkoutSession, IsNSDate date) => hkWorkoutSession -> date -> IO ()
endCurrentActivityOnDate hkWorkoutSession  date =
  withObjCPtr date $ \raw_date ->
      sendMsg hkWorkoutSession (mkSelector "endCurrentActivityOnDate:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | startMirroringToCompanionDeviceWithCompletion:
--
-- Starts mirroring the session to the companion device.
--
-- Calling this method will result in your app on the companion device being launched in the background.                When your app is launched set the @HKHealthStore@ @workoutSessionMirroringStartHandler@ property to retrieve                the mirrored session.                This method will fail if called for a session that is ended.                The completion handler will be executed on an arbitrary background queue.
--
-- ObjC selector: @- startMirroringToCompanionDeviceWithCompletion:@
startMirroringToCompanionDeviceWithCompletion :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> Ptr () -> IO ()
startMirroringToCompanionDeviceWithCompletion hkWorkoutSession  completion =
    sendMsg hkWorkoutSession (mkSelector "startMirroringToCompanionDeviceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | stopMirroringToCompanionDeviceWithCompletion:
--
-- Stops mirroring the session to the companion device.
--
-- Calling this method will stop sending data to the companion device. The mirrored session's delegate method                @didDisconnectFromRemoteDeviceWithError:@ will be called to indicate that.                When a workout session is ended, mirroring is automatically stopped.                The completion handler will be executed on an arbitrary background queue.
--
-- ObjC selector: @- stopMirroringToCompanionDeviceWithCompletion:@
stopMirroringToCompanionDeviceWithCompletion :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> Ptr () -> IO ()
stopMirroringToCompanionDeviceWithCompletion hkWorkoutSession  completion =
    sendMsg hkWorkoutSession (mkSelector "stopMirroringToCompanionDeviceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | sendDataToRemoteWorkoutSession:completion:
--
-- Sends an NSData object to the connected remote workout session.
--
-- This method can be called to send data from a primary session to its mirrored counterpart and vice-versa.                It's only intended to be used for data that describes the current state of the workout, such as accumulated metrics, and any                data needed to keep your app on both devices in sync.                The maximum amount of data that can be sent is 100 KB in any given 10-second time window.                If this limit is exceeded, an error will be returned in the completion handler.                An error will also be returned if the session is not mirroring.                The completion handler will be executed on an arbitrary background queue.
--
-- ObjC selector: @- sendDataToRemoteWorkoutSession:completion:@
sendDataToRemoteWorkoutSession_completion :: (IsHKWorkoutSession hkWorkoutSession, IsNSData data_) => hkWorkoutSession -> data_ -> Ptr () -> IO ()
sendDataToRemoteWorkoutSession_completion hkWorkoutSession  data_ completion =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg hkWorkoutSession (mkSelector "sendDataToRemoteWorkoutSession:completion:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | activityType
--
-- Indicates the type of workout that will be performed during the session.
--
-- ObjC selector: @- activityType@
activityType :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO HKWorkoutActivityType
activityType hkWorkoutSession  =
    fmap (coerce :: CULong -> HKWorkoutActivityType) $ sendMsg hkWorkoutSession (mkSelector "activityType") retCULong []

-- | locationType
--
-- Indicates the type of location (indoors vs. outdoors) where the workout will take place.
--
-- Knowing the location type allows for more accurate measurements and better performance.
--
-- ObjC selector: @- locationType@
locationType :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO HKWorkoutSessionLocationType
locationType hkWorkoutSession  =
    fmap (coerce :: CLong -> HKWorkoutSessionLocationType) $ sendMsg hkWorkoutSession (mkSelector "locationType") retCLong []

-- | workoutConfiguration
--
-- The configuration object describing the workout.
--
-- This returns a copy of the configuration passed when creating the HKWorkoutSession. Changes made to                the returned object have no impact on the HKWorkoutSession.
--
-- ObjC selector: @- workoutConfiguration@
workoutConfiguration :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO (Id HKWorkoutConfiguration)
workoutConfiguration hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "workoutConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | delegate
--
-- The session delegate, which receives
--
-- The session delegate object is the one implementing the methods that get called when the session                state changes or a failure occurs in the session.
--
-- ObjC selector: @- delegate@
delegate :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO RawId
delegate hkWorkoutSession  =
    fmap (RawId . castPtr) $ sendMsg hkWorkoutSession (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The session delegate, which receives
--
-- The session delegate object is the one implementing the methods that get called when the session                state changes or a failure occurs in the session.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> RawId -> IO ()
setDelegate hkWorkoutSession  value =
    sendMsg hkWorkoutSession (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | state
--
-- Indicates the current state of the workout session.
--
-- Each time this value is updated, the delegate method workoutSession:didChangeToState:fromState:date:                will be called.
--
-- ObjC selector: @- state@
state :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO HKWorkoutSessionState
state hkWorkoutSession  =
    fmap (coerce :: CLong -> HKWorkoutSessionState) $ sendMsg hkWorkoutSession (mkSelector "state") retCLong []

-- | type
--
-- Indicates the type of the workout session.
--
-- A workout session created using an initializer will be primary, while a session retrieved with                the @HKHealthStore@ @workoutSessionMirroringStartHandler@ property will be mirrored.
--
-- ObjC selector: @- type@
type_ :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO HKWorkoutSessionType
type_ hkWorkoutSession  =
    fmap (coerce :: CLong -> HKWorkoutSessionType) $ sendMsg hkWorkoutSession (mkSelector "type") retCLong []

-- | startDate
--
-- Indicates the date when the workout session started running.
--
-- This value is nil when a workout session is initialized. It is set when the workout session state                changes to HKWorkoutSessionStateRunning.
--
-- ObjC selector: @- startDate@
startDate :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO (Id NSDate)
startDate hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | endDate
--
-- Indicates the date when the workout session stopped.
--
-- This value is nil when a workout session is initialized. It is set when the workout session state                changes to HKWorkoutSessionStateStopped.
--
-- ObjC selector: @- endDate@
endDate :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO (Id NSDate)
endDate hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | currentActivity
--
-- The current workout activity.
--
-- This returns a copy of the session's current workout activity. It will return                a copy of the main workout activity if no new activity has begun. Changes made                to the returned object have no impact on the HKWorkoutSession.
--
-- ObjC selector: @- currentActivity@
currentActivity :: IsHKWorkoutSession hkWorkoutSession => hkWorkoutSession -> IO (Id HKWorkoutActivity)
currentActivity hkWorkoutSession  =
    sendMsg hkWorkoutSession (mkSelector "currentActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithActivityType:locationType:@
initWithActivityType_locationTypeSelector :: Selector
initWithActivityType_locationTypeSelector = mkSelector "initWithActivityType:locationType:"

-- | @Selector@ for @initWithConfiguration:error:@
initWithConfiguration_errorSelector :: Selector
initWithConfiguration_errorSelector = mkSelector "initWithConfiguration:error:"

-- | @Selector@ for @initWithHealthStore:configuration:error:@
initWithHealthStore_configuration_errorSelector :: Selector
initWithHealthStore_configuration_errorSelector = mkSelector "initWithHealthStore:configuration:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @prepare@
prepareSelector :: Selector
prepareSelector = mkSelector "prepare"

-- | @Selector@ for @startActivityWithDate:@
startActivityWithDateSelector :: Selector
startActivityWithDateSelector = mkSelector "startActivityWithDate:"

-- | @Selector@ for @stopActivityWithDate:@
stopActivityWithDateSelector :: Selector
stopActivityWithDateSelector = mkSelector "stopActivityWithDate:"

-- | @Selector@ for @end@
endSelector :: Selector
endSelector = mkSelector "end"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @associatedWorkoutBuilder@
associatedWorkoutBuilderSelector :: Selector
associatedWorkoutBuilderSelector = mkSelector "associatedWorkoutBuilder"

-- | @Selector@ for @beginNewActivityWithConfiguration:date:metadata:@
beginNewActivityWithConfiguration_date_metadataSelector :: Selector
beginNewActivityWithConfiguration_date_metadataSelector = mkSelector "beginNewActivityWithConfiguration:date:metadata:"

-- | @Selector@ for @endCurrentActivityOnDate:@
endCurrentActivityOnDateSelector :: Selector
endCurrentActivityOnDateSelector = mkSelector "endCurrentActivityOnDate:"

-- | @Selector@ for @startMirroringToCompanionDeviceWithCompletion:@
startMirroringToCompanionDeviceWithCompletionSelector :: Selector
startMirroringToCompanionDeviceWithCompletionSelector = mkSelector "startMirroringToCompanionDeviceWithCompletion:"

-- | @Selector@ for @stopMirroringToCompanionDeviceWithCompletion:@
stopMirroringToCompanionDeviceWithCompletionSelector :: Selector
stopMirroringToCompanionDeviceWithCompletionSelector = mkSelector "stopMirroringToCompanionDeviceWithCompletion:"

-- | @Selector@ for @sendDataToRemoteWorkoutSession:completion:@
sendDataToRemoteWorkoutSession_completionSelector :: Selector
sendDataToRemoteWorkoutSession_completionSelector = mkSelector "sendDataToRemoteWorkoutSession:completion:"

-- | @Selector@ for @activityType@
activityTypeSelector :: Selector
activityTypeSelector = mkSelector "activityType"

-- | @Selector@ for @locationType@
locationTypeSelector :: Selector
locationTypeSelector = mkSelector "locationType"

-- | @Selector@ for @workoutConfiguration@
workoutConfigurationSelector :: Selector
workoutConfigurationSelector = mkSelector "workoutConfiguration"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @currentActivity@
currentActivitySelector :: Selector
currentActivitySelector = mkSelector "currentActivity"

