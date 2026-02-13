{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKHealthStore
--
-- The HKHealthStore class provides an interface for accessing and storing the user's health data.
--
-- Generated bindings for @HKHealthStore@.
module ObjC.HealthKit.HKHealthStore
  ( HKHealthStore
  , IsHKHealthStore(..)
  , isHealthDataAvailable
  , supportsHealthRecords
  , authorizationStatusForType
  , requestAuthorizationToShareTypes_readTypes_completion
  , requestPerObjectReadAuthorizationForType_predicate_completion
  , getRequestStatusForAuthorizationToShareTypes_readTypes_completion
  , handleAuthorizationForExtensionWithCompletion
  , earliestPermittedSampleDate
  , saveObject_withCompletion
  , saveObjects_withCompletion
  , deleteObject_withCompletion
  , deleteObjects_withCompletion
  , deleteObjectsOfType_predicate_withCompletion
  , executeQuery
  , stopQuery
  , splitTotalEnergy_startDate_endDate_resultsHandler
  , dateOfBirthWithError
  , dateOfBirthComponentsWithError
  , biologicalSexWithError
  , bloodTypeWithError
  , fitzpatrickSkinTypeWithError
  , wheelchairUseWithError
  , activityMoveModeWithError
  , relateWorkoutEffortSample_withWorkout_activity_completion
  , unrelateWorkoutEffortSample_fromWorkout_activity_completion
  , recalibrateEstimatesForSampleType_atDate_completion
  , enableBackgroundDeliveryForType_frequency_withCompletion
  , disableBackgroundDeliveryForType_withCompletion
  , disableAllBackgroundDeliveryWithCompletion
  , addSamples_toWorkout_completion
  , startWorkoutSession
  , endWorkoutSession
  , pauseWorkoutSession
  , resumeWorkoutSession
  , startWatchAppWithWorkoutConfiguration_completion
  , recoverActiveWorkoutSessionWithCompletion
  , workoutSessionMirroringStartHandler
  , setWorkoutSessionMirroringStartHandler
  , activityMoveModeWithErrorSelector
  , addSamples_toWorkout_completionSelector
  , authorizationStatusForTypeSelector
  , biologicalSexWithErrorSelector
  , bloodTypeWithErrorSelector
  , dateOfBirthComponentsWithErrorSelector
  , dateOfBirthWithErrorSelector
  , deleteObject_withCompletionSelector
  , deleteObjectsOfType_predicate_withCompletionSelector
  , deleteObjects_withCompletionSelector
  , disableAllBackgroundDeliveryWithCompletionSelector
  , disableBackgroundDeliveryForType_withCompletionSelector
  , earliestPermittedSampleDateSelector
  , enableBackgroundDeliveryForType_frequency_withCompletionSelector
  , endWorkoutSessionSelector
  , executeQuerySelector
  , fitzpatrickSkinTypeWithErrorSelector
  , getRequestStatusForAuthorizationToShareTypes_readTypes_completionSelector
  , handleAuthorizationForExtensionWithCompletionSelector
  , isHealthDataAvailableSelector
  , pauseWorkoutSessionSelector
  , recalibrateEstimatesForSampleType_atDate_completionSelector
  , recoverActiveWorkoutSessionWithCompletionSelector
  , relateWorkoutEffortSample_withWorkout_activity_completionSelector
  , requestAuthorizationToShareTypes_readTypes_completionSelector
  , requestPerObjectReadAuthorizationForType_predicate_completionSelector
  , resumeWorkoutSessionSelector
  , saveObject_withCompletionSelector
  , saveObjects_withCompletionSelector
  , setWorkoutSessionMirroringStartHandlerSelector
  , splitTotalEnergy_startDate_endDate_resultsHandlerSelector
  , startWatchAppWithWorkoutConfiguration_completionSelector
  , startWorkoutSessionSelector
  , stopQuerySelector
  , supportsHealthRecordsSelector
  , unrelateWorkoutEffortSample_fromWorkout_activity_completionSelector
  , wheelchairUseWithErrorSelector
  , workoutSessionMirroringStartHandlerSelector

  -- * Enum types
  , HKAuthorizationStatus(HKAuthorizationStatus)
  , pattern HKAuthorizationStatusNotDetermined
  , pattern HKAuthorizationStatusSharingDenied
  , pattern HKAuthorizationStatusSharingAuthorized
  , HKUpdateFrequency(HKUpdateFrequency)
  , pattern HKUpdateFrequencyImmediate
  , pattern HKUpdateFrequencyHourly
  , pattern HKUpdateFrequencyDaily
  , pattern HKUpdateFrequencyWeekly

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | isHealthDataAvailable
--
-- Returns YES if HealthKit is supported on the device.
--
-- HealthKit is not supported on all iOS devices.  Using HKHealthStore APIs on devices which are not                supported will result in errors with the HKErrorHealthDataUnavailable code.  Call isHealthDataAvailable                before attempting to use other parts of the framework.
--
-- ObjC selector: @+ isHealthDataAvailable@
isHealthDataAvailable :: IO Bool
isHealthDataAvailable  =
  do
    cls' <- getRequiredClass "HKHealthStore"
    sendClassMessage cls' isHealthDataAvailableSelector

-- | supportsHealthRecords
--
-- Returns YES if the Health Records feature is available.
--
-- The Health Records feature is not available in all regions but may be present in unsupported regions                if accounts have already been configured. This can change as accounts are modified during device                restore or synchronization.                Call supportsHealthRecords before attempting to request authorization for any clinical types.
--
-- ObjC selector: @- supportsHealthRecords@
supportsHealthRecords :: IsHKHealthStore hkHealthStore => hkHealthStore -> IO Bool
supportsHealthRecords hkHealthStore =
  sendMessage hkHealthStore supportsHealthRecordsSelector

-- | authorizationStatusForType:
--
-- Returns the application's authorization status for the given object type.
--
-- ObjC selector: @- authorizationStatusForType:@
authorizationStatusForType :: (IsHKHealthStore hkHealthStore, IsHKObjectType type_) => hkHealthStore -> type_ -> IO HKAuthorizationStatus
authorizationStatusForType hkHealthStore type_ =
  sendMessage hkHealthStore authorizationStatusForTypeSelector (toHKObjectType type_)

-- | requestAuthorizationToShareTypes:readTypes:completion:
--
-- Prompts the user to authorize the application for reading and saving objects of the given types.
--
-- Before attempting to execute queries or save objects, the application should first request authorization                from the user to read and share every type of object for which the application may require access.
--
-- The request is performed asynchronously and its completion will be executed on an arbitrary background                queue after the user has responded.  If the user has already chosen whether to grant the application                access to all of the types provided, then the completion will be called without prompting the user.                The success parameter of the completion indicates whether prompting the user, if necessary, completed                successfully and was not cancelled by the user.  It does NOT indicate whether the application was                granted authorization.
--
-- To customize the messages displayed on the authorization sheet, set the following keys in your app's                Info.plist file. Set the NSHealthShareUsageDescription key to customize the message for reading data.                Set the NSHealthUpdateUsageDescription key to customize the message for writing data.
--
-- ObjC selector: @- requestAuthorizationToShareTypes:readTypes:completion:@
requestAuthorizationToShareTypes_readTypes_completion :: (IsHKHealthStore hkHealthStore, IsNSSet typesToShare, IsNSSet typesToRead) => hkHealthStore -> typesToShare -> typesToRead -> Ptr () -> IO ()
requestAuthorizationToShareTypes_readTypes_completion hkHealthStore typesToShare typesToRead completion =
  sendMessage hkHealthStore requestAuthorizationToShareTypes_readTypes_completionSelector (toNSSet typesToShare) (toNSSet typesToRead) completion

-- | requestPerObjectReadAuthorizationForType:predicate:completion:
--
-- For types that support per object authorization (like vision prescriptions), prompts the user to select                the objects for which they want to grant your app access.
--
-- Before attempting to execute queries, the application should first request authorization from the user                to read objects for which the application may require access.
--
-- The request is performed asynchronously, and its completion will be executed on an arbitrary background                queue after the user has responded. The user will always be prompted to provide access to objects                regardless of whether access had been previously provided. The user can choose to toggle each object's                access with each prompt. The success parameter of the completion indicates whether prompting the user                completed successfully and was not cancelled. It does NOT indicate whether the application was granted                authorization.
--
-- ObjC selector: @- requestPerObjectReadAuthorizationForType:predicate:completion:@
requestPerObjectReadAuthorizationForType_predicate_completion :: (IsHKHealthStore hkHealthStore, IsHKObjectType objectType, IsNSPredicate predicate) => hkHealthStore -> objectType -> predicate -> Ptr () -> IO ()
requestPerObjectReadAuthorizationForType_predicate_completion hkHealthStore objectType predicate completion =
  sendMessage hkHealthStore requestPerObjectReadAuthorizationForType_predicate_completionSelector (toHKObjectType objectType) (toNSPredicate predicate) completion

-- | getRequestStatusForAuthorizationToShareTypes:readTypes:completion:
--
-- Determines whether requesting authorization for the given types is necessary.
--
-- Applications may call this method to determine whether the user would be prompted for authorization if                the same collections of types are passed to requestAuthorizationToShareTypes:readTypes:completion:.                This determination is performed asynchronously and its completion will be executed on an arbitrary                background queue.
--
-- ObjC selector: @- getRequestStatusForAuthorizationToShareTypes:readTypes:completion:@
getRequestStatusForAuthorizationToShareTypes_readTypes_completion :: (IsHKHealthStore hkHealthStore, IsNSSet typesToShare, IsNSSet typesToRead) => hkHealthStore -> typesToShare -> typesToRead -> Ptr () -> IO ()
getRequestStatusForAuthorizationToShareTypes_readTypes_completion hkHealthStore typesToShare typesToRead completion =
  sendMessage hkHealthStore getRequestStatusForAuthorizationToShareTypes_readTypes_completionSelector (toNSSet typesToShare) (toNSSet typesToRead) completion

-- | handleAuthorizationForExtensionWithCompletion:
--
-- Prompts the user to authorize the application for reading and saving objects.
--
-- When an app extension calls requestAuthorizationToShareTypes:readTypes:completion:, the parent application                is responsible for calling this method to prompt the user to authorize the app and its extensions for the                types that the extension requested access to.
--
-- The request is performed asynchronously and its completion will be executed on an arbitrary background                queue after the user has responded.  The success parameter of the completion indicates whether prompting                the user, if necessary, completed successfully and was not cancelled by the user.  It does NOT indicate                whether the application was granted authorization.
--
-- ObjC selector: @- handleAuthorizationForExtensionWithCompletion:@
handleAuthorizationForExtensionWithCompletion :: IsHKHealthStore hkHealthStore => hkHealthStore -> Ptr () -> IO ()
handleAuthorizationForExtensionWithCompletion hkHealthStore completion =
  sendMessage hkHealthStore handleAuthorizationForExtensionWithCompletionSelector completion

-- | earliestPermittedSampleDate
--
-- Samples prior to the earliestPermittedSampleDate cannot be saved or queried.
--
-- On some platforms, only samples with end dates newer than the value returned by earliestPermittedSampleDate                may be saved or retrieved.
--
-- ObjC selector: @- earliestPermittedSampleDate@
earliestPermittedSampleDate :: IsHKHealthStore hkHealthStore => hkHealthStore -> IO (Id NSDate)
earliestPermittedSampleDate hkHealthStore =
  sendMessage hkHealthStore earliestPermittedSampleDateSelector

-- | saveObject:withCompletion:
--
-- Saves an HKObject.
--
-- After an object is saved, on subsequent retrievals the sourceRevision property of the object will be set                to the HKSourceRevision representing the version of the application that saved it.
--
-- If the object has an HKObjectType property, then in order to save an object successfully the application                must first request authorization to share objects with that type.  Saving an object with the same unique                identifier as another object that has already been saved will fail.  When the application attempts to                save multiple objects, if any single object cannot be saved then none of the objects will be saved.                The operation will fail if the objects array contains samples with endDates that are older than the date                returned by earliestPermittedSampleDate.
--
-- This operation is performed asynchronously and the completion will be executed on an arbitrary                background queue.
--
-- ObjC selector: @- saveObject:withCompletion:@
saveObject_withCompletion :: (IsHKHealthStore hkHealthStore, IsHKObject object) => hkHealthStore -> object -> Ptr () -> IO ()
saveObject_withCompletion hkHealthStore object completion =
  sendMessage hkHealthStore saveObject_withCompletionSelector (toHKObject object) completion

-- | saveObjects:withCompletion:
--
-- Saves an array of HKObjects.
--
-- See discussion of saveObject:withCompletion:.
--
-- ObjC selector: @- saveObjects:withCompletion:@
saveObjects_withCompletion :: (IsHKHealthStore hkHealthStore, IsNSArray objects) => hkHealthStore -> objects -> Ptr () -> IO ()
saveObjects_withCompletion hkHealthStore objects completion =
  sendMessage hkHealthStore saveObjects_withCompletionSelector (toNSArray objects) completion

-- | deleteObject:withCompletion:
--
-- Deletes a single HKObject from the HealthKit database.
--
-- See deleteObjects:withCompletion:.
--
-- ObjC selector: @- deleteObject:withCompletion:@
deleteObject_withCompletion :: (IsHKHealthStore hkHealthStore, IsHKObject object) => hkHealthStore -> object -> Ptr () -> IO ()
deleteObject_withCompletion hkHealthStore object completion =
  sendMessage hkHealthStore deleteObject_withCompletionSelector (toHKObject object) completion

-- | deleteObjects:withCompletion:
--
-- Deletes multiple HKObjects from the HealthKit database.
--
-- An application may only delete objects that it previously saved.  This operation is performed                asynchronously and the completion will be executed on an arbitrary background queue.
--
-- ObjC selector: @- deleteObjects:withCompletion:@
deleteObjects_withCompletion :: (IsHKHealthStore hkHealthStore, IsNSArray objects) => hkHealthStore -> objects -> Ptr () -> IO ()
deleteObjects_withCompletion hkHealthStore objects completion =
  sendMessage hkHealthStore deleteObjects_withCompletionSelector (toNSArray objects) completion

-- | deleteObjectsOfType:predicate:withCompletion:
--
-- Deletes all objects matching the given predicate from the HealthKit database.
--
-- An application may only delete objects that it previously saved.  This operation is performed                asynchronously and the completion will be executed on an arbitrary background queue.
--
-- ObjC selector: @- deleteObjectsOfType:predicate:withCompletion:@
deleteObjectsOfType_predicate_withCompletion :: (IsHKHealthStore hkHealthStore, IsHKObjectType objectType, IsNSPredicate predicate) => hkHealthStore -> objectType -> predicate -> Ptr () -> IO ()
deleteObjectsOfType_predicate_withCompletion hkHealthStore objectType predicate completion =
  sendMessage hkHealthStore deleteObjectsOfType_predicate_withCompletionSelector (toHKObjectType objectType) (toNSPredicate predicate) completion

-- | executeQuery:
--
-- Begins executing the given query.
--
-- After executing a query, the completion, update, and/or results handlers of that query will be invoked                asynchronously on an arbitrary background queue as results become available.  Errors that prevent a                query from executing will be delivered to one of the query's handlers.  Which handler the error will be                delivered to is defined by the HKQuery subclass.
--
-- Each HKQuery instance may only be executed once and calling this method with a currently executing query                or one that was previously executed will result in an exception.
--
-- If a query would retrieve objects with an HKObjectType property, then the application must request                authorization to access objects of that type before executing the query.
--
-- ObjC selector: @- executeQuery:@
executeQuery :: (IsHKHealthStore hkHealthStore, IsHKQuery query) => hkHealthStore -> query -> IO ()
executeQuery hkHealthStore query =
  sendMessage hkHealthStore executeQuerySelector (toHKQuery query)

-- | stopQuery:
--
-- Stops a query that is executing from continuing to run.
--
-- Calling this method will prevent the handlers of the query from being invoked in the future.  If the                query is already stopped, this method does nothing.
--
-- ObjC selector: @- stopQuery:@
stopQuery :: (IsHKHealthStore hkHealthStore, IsHKQuery query) => hkHealthStore -> query -> IO ()
stopQuery hkHealthStore query =
  sendMessage hkHealthStore stopQuerySelector (toHKQuery query)

-- | splitTotalEnergy:startDate:endDate:resultsHandler:
--
-- For the time period specified, this method calculates the resting and active energy parts of the total                energy provided.
--
-- This method uses the user's metrics like age, biological sex, body mass and height to determine                their basal metabolic rate. If the application does not have authorization to access these characteristics                or if the user has not entered their data then this method uses builtin default values.
--
-- ObjC selector: @- splitTotalEnergy:startDate:endDate:resultsHandler:@
splitTotalEnergy_startDate_endDate_resultsHandler :: (IsHKHealthStore hkHealthStore, IsHKQuantity totalEnergy, IsNSDate startDate, IsNSDate endDate) => hkHealthStore -> totalEnergy -> startDate -> endDate -> Ptr () -> IO ()
splitTotalEnergy_startDate_endDate_resultsHandler hkHealthStore totalEnergy startDate endDate resultsHandler =
  sendMessage hkHealthStore splitTotalEnergy_startDate_endDate_resultsHandlerSelector (toHKQuantity totalEnergy) (toNSDate startDate) (toNSDate endDate) resultsHandler

-- | @- dateOfBirthWithError:@
dateOfBirthWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id NSDate)
dateOfBirthWithError hkHealthStore error_ =
  sendMessage hkHealthStore dateOfBirthWithErrorSelector (toNSError error_)

-- | dateOfBirthComponentsWithError:
--
-- Returns the user's date of birth in the Gregorian calendar.
--
-- Before calling this method, the application should request authorization to access objects with the                HKCharacteristicType identified by HKCharacteristicTypeIdentifierDateOfBirth.
--
-- ObjC selector: @- dateOfBirthComponentsWithError:@
dateOfBirthComponentsWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id NSDateComponents)
dateOfBirthComponentsWithError hkHealthStore error_ =
  sendMessage hkHealthStore dateOfBirthComponentsWithErrorSelector (toNSError error_)

-- | biologicalSexWithError:
--
-- Returns an object encapsulating the user's biological sex.
--
-- Before calling this method, the application should request authorization to access objects with the                HKCharacteristicType identified by HKCharacteristicTypeIdentifierBiologicalSex.
--
-- ObjC selector: @- biologicalSexWithError:@
biologicalSexWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id HKBiologicalSexObject)
biologicalSexWithError hkHealthStore error_ =
  sendMessage hkHealthStore biologicalSexWithErrorSelector (toNSError error_)

-- | bloodTypeWithError:
--
-- Returns an object encapsulating the user's blood type.
--
-- Before calling this method, the application should request authorization to access objects with the                HKCharacteristicType identified by HKCharacteristicTypeIdentifierBloodType.
--
-- ObjC selector: @- bloodTypeWithError:@
bloodTypeWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id HKBloodTypeObject)
bloodTypeWithError hkHealthStore error_ =
  sendMessage hkHealthStore bloodTypeWithErrorSelector (toNSError error_)

-- | fitzpatrickSkinTypeWithError:
--
-- Returns an object encapsulating the user's Fitzpatrick skin type.
--
-- Before calling this method, the application should request authorization to access objects with the                HKCharacteristicType identified by HKCharacteristicTypeIdentifierFitzpatrickSkinType.
--
-- ObjC selector: @- fitzpatrickSkinTypeWithError:@
fitzpatrickSkinTypeWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id HKFitzpatrickSkinTypeObject)
fitzpatrickSkinTypeWithError hkHealthStore error_ =
  sendMessage hkHealthStore fitzpatrickSkinTypeWithErrorSelector (toNSError error_)

-- | wheelchairUseWithError:
--
-- Returns an object encapsulating the user's wheelchair use.
--
-- Before calling this method, the application should request authorization to access objects with the                HKCharacteristicType identified by HKCharacteristicTypeIdentifierWheelchairUse.
--
-- ObjC selector: @- wheelchairUseWithError:@
wheelchairUseWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id HKWheelchairUseObject)
wheelchairUseWithError hkHealthStore error_ =
  sendMessage hkHealthStore wheelchairUseWithErrorSelector (toNSError error_)

-- | activityMoveModeWithError:
--
-- Returns an object encapsulating the user's activity move mode
--
-- Before calling this method, the application should request authorization to access objects with the                HKCharacteristicType identified by HKCharacteristicTypeIdentifierActivityMoveMode.
--
-- ObjC selector: @- activityMoveModeWithError:@
activityMoveModeWithError :: (IsHKHealthStore hkHealthStore, IsNSError error_) => hkHealthStore -> error_ -> IO (Id HKActivityMoveModeObject)
activityMoveModeWithError hkHealthStore error_ =
  sendMessage hkHealthStore activityMoveModeWithErrorSelector (toNSError error_)

-- | relateWorkoutEffortSample:withWorkout:activity:completion
--
-- Relates a workout effort sample with a workout
--
-- @sample@ — The workout effort sample
--
-- @workout@ — The HKWorkout to relate the sample to
--
-- @activity@ — The HKWorkoutActivity on the HKWorkout
--
-- @completion@ — The block to be called when the sample has been related
--
-- ObjC selector: @- relateWorkoutEffortSample:withWorkout:activity:completion:@
relateWorkoutEffortSample_withWorkout_activity_completion :: (IsHKHealthStore hkHealthStore, IsHKSample sample, IsHKWorkout workout, IsHKWorkoutActivity activity) => hkHealthStore -> sample -> workout -> activity -> Ptr () -> IO ()
relateWorkoutEffortSample_withWorkout_activity_completion hkHealthStore sample workout activity completion =
  sendMessage hkHealthStore relateWorkoutEffortSample_withWorkout_activity_completionSelector (toHKSample sample) (toHKWorkout workout) (toHKWorkoutActivity activity) completion

-- | unrelateWorkoutEffortSample:fromWorkout:activity:completion
--
-- Unrelates a workout effort sample from a workout
--
-- @sample@ — The workout effort sample
--
-- @workout@ — The HKWorkout to unrelate the sample from
--
-- @activity@ — The HKWorkoutActivity on the HKWorkout
--
-- @completion@ — The block to be called when the sample has been unrelated
--
-- ObjC selector: @- unrelateWorkoutEffortSample:fromWorkout:activity:completion:@
unrelateWorkoutEffortSample_fromWorkout_activity_completion :: (IsHKHealthStore hkHealthStore, IsHKSample sample, IsHKWorkout workout, IsHKWorkoutActivity activity) => hkHealthStore -> sample -> workout -> activity -> Ptr () -> IO ()
unrelateWorkoutEffortSample_fromWorkout_activity_completion hkHealthStore sample workout activity completion =
  sendMessage hkHealthStore unrelateWorkoutEffortSample_fromWorkout_activity_completionSelector (toHKSample sample) (toHKWorkout workout) (toHKWorkoutActivity activity) completion

-- | recalibrateEstimatesForSampleType:atDate:completion:
--
-- Recalibrates the prediction algorithm used for this sample type.
--
-- Check -[HKSampleType allowsRecalibrationForEstimates] to see if a given sample type is supported. Calling this method results in first-party estimation algorithms to recalibrate what data is used when generating values for HKSamples of this sampleType.
--
-- ObjC selector: @- recalibrateEstimatesForSampleType:atDate:completion:@
recalibrateEstimatesForSampleType_atDate_completion :: (IsHKHealthStore hkHealthStore, IsHKSampleType sampleType, IsNSDate date) => hkHealthStore -> sampleType -> date -> Ptr () -> IO ()
recalibrateEstimatesForSampleType_atDate_completion hkHealthStore sampleType date completion =
  sendMessage hkHealthStore recalibrateEstimatesForSampleType_atDate_completionSelector (toHKSampleType sampleType) (toNSDate date) completion

-- | enableBackgroundDeliveryForType:frequency:withCompletion:
--
-- This method enables activation of your app when data of the type is recorded at the cadence specified.
--
-- When an app has subscribed to a certain data type it will get activated at the cadence that is specified                with the frequency parameter. The app is still responsible for creating an HKObserverQuery to know which                data types have been updated and the corresponding fetch queries. Note that certain data types (such as                HKQuantityTypeIdentifierStepCount) have a minimum frequency of HKUpdateFrequencyHourly. This is enforced                transparently to the caller.
--
-- ObjC selector: @- enableBackgroundDeliveryForType:frequency:withCompletion:@
enableBackgroundDeliveryForType_frequency_withCompletion :: (IsHKHealthStore hkHealthStore, IsHKObjectType type_) => hkHealthStore -> type_ -> HKUpdateFrequency -> Ptr () -> IO ()
enableBackgroundDeliveryForType_frequency_withCompletion hkHealthStore type_ frequency completion =
  sendMessage hkHealthStore enableBackgroundDeliveryForType_frequency_withCompletionSelector (toHKObjectType type_) frequency completion

-- | @- disableBackgroundDeliveryForType:withCompletion:@
disableBackgroundDeliveryForType_withCompletion :: (IsHKHealthStore hkHealthStore, IsHKObjectType type_) => hkHealthStore -> type_ -> Ptr () -> IO ()
disableBackgroundDeliveryForType_withCompletion hkHealthStore type_ completion =
  sendMessage hkHealthStore disableBackgroundDeliveryForType_withCompletionSelector (toHKObjectType type_) completion

-- | @- disableAllBackgroundDeliveryWithCompletion:@
disableAllBackgroundDeliveryWithCompletion :: IsHKHealthStore hkHealthStore => hkHealthStore -> Ptr () -> IO ()
disableAllBackgroundDeliveryWithCompletion hkHealthStore completion =
  sendMessage hkHealthStore disableAllBackgroundDeliveryWithCompletionSelector completion

-- | addSamples:toWorkout:completion:
--
-- Associates samples with a given workout.
--
-- This will associate the given samples with the given workout. These samples will then be returned by a                query that contains this workout as a predicate. If a sample is added that is not saved yet, then it will                be saved for you. Note that the sample will be saved without an HKDevice.
--
-- The workout provided must be one that has already been saved to HealthKit.
--
-- ObjC selector: @- addSamples:toWorkout:completion:@
addSamples_toWorkout_completion :: (IsHKHealthStore hkHealthStore, IsNSArray samples, IsHKWorkout workout) => hkHealthStore -> samples -> workout -> Ptr () -> IO ()
addSamples_toWorkout_completion hkHealthStore samples workout completion =
  sendMessage hkHealthStore addSamples_toWorkout_completionSelector (toNSArray samples) (toHKWorkout workout) completion

-- | startWorkoutSession:
--
-- Starts the given workout session.
--
-- This method will asynchronously begin a workout session. The methods on the session's delegate will be                 called when the session has successfully started or fails to start.
--
-- ObjC selector: @- startWorkoutSession:@
startWorkoutSession :: (IsHKHealthStore hkHealthStore, IsHKWorkoutSession workoutSession) => hkHealthStore -> workoutSession -> IO ()
startWorkoutSession hkHealthStore workoutSession =
  sendMessage hkHealthStore startWorkoutSessionSelector (toHKWorkoutSession workoutSession)

-- | endWorkoutSession:
--
-- Ends the given workout session.
--
-- This method will end the given session if it is currently running. The state of the workout session will                transition to HKWorkoutSessionStateEnded. Once a workout session is ended, it cannot be reused to start                a new workout session.
--
-- ObjC selector: @- endWorkoutSession:@
endWorkoutSession :: (IsHKHealthStore hkHealthStore, IsHKWorkoutSession workoutSession) => hkHealthStore -> workoutSession -> IO ()
endWorkoutSession hkHealthStore workoutSession =
  sendMessage hkHealthStore endWorkoutSessionSelector (toHKWorkoutSession workoutSession)

-- | pauseWorkoutSession:
--
-- Pauses the given workout session.
--
-- This method will pause the given session if it is currently running. The state of the workout session                will transition to HKWorkoutSessionStatePaused. An HKWorkoutEventTypePause will be generated and                delivered to the workout session's delegate.
--
-- ObjC selector: @- pauseWorkoutSession:@
pauseWorkoutSession :: (IsHKHealthStore hkHealthStore, IsHKWorkoutSession workoutSession) => hkHealthStore -> workoutSession -> IO ()
pauseWorkoutSession hkHealthStore workoutSession =
  sendMessage hkHealthStore pauseWorkoutSessionSelector (toHKWorkoutSession workoutSession)

-- | resumeWorkoutSession:
--
-- Resumes the given workout session.
--
-- This method will resume the given session if it is currently paused. The state of the workout session                will transition to HKWorkoutSessionStateRunning. An HKWorkoutEventTypeResume will be generated and                delivered to the workout session's delegate.
--
-- ObjC selector: @- resumeWorkoutSession:@
resumeWorkoutSession :: (IsHKHealthStore hkHealthStore, IsHKWorkoutSession workoutSession) => hkHealthStore -> workoutSession -> IO ()
resumeWorkoutSession hkHealthStore workoutSession =
  sendMessage hkHealthStore resumeWorkoutSessionSelector (toHKWorkoutSession workoutSession)

-- | startWatchAppWithWorkoutConfiguration:completion:
--
-- Launches or wakes up the WatchKit app on the watch
--
-- This method will launch the WatchKit app corresponding to the calling iOS application on the currently                active Apple Watch. After launching, the handleWorkoutConfiguration: method on the WKExtensionDelegate                protocol will be called with the HKWorkoutConfiguration as a parameter. The receiving Watch app can use                this configuration object to create an HKWorkoutSession and start it with -startWorkoutSession:.
--
-- ObjC selector: @- startWatchAppWithWorkoutConfiguration:completion:@
startWatchAppWithWorkoutConfiguration_completion :: (IsHKHealthStore hkHealthStore, IsHKWorkoutConfiguration workoutConfiguration) => hkHealthStore -> workoutConfiguration -> Ptr () -> IO ()
startWatchAppWithWorkoutConfiguration_completion hkHealthStore workoutConfiguration completion =
  sendMessage hkHealthStore startWatchAppWithWorkoutConfiguration_completionSelector (toHKWorkoutConfiguration workoutConfiguration) completion

-- | recoverActiveWorkoutSessionWithCompletion:
--
-- Recovers an active workout session after a client crash. If no session is available to be re-attached,                nil will be returned. If an error occurs, session will be nil and error will be set appropriately.
--
-- ObjC selector: @- recoverActiveWorkoutSessionWithCompletion:@
recoverActiveWorkoutSessionWithCompletion :: IsHKHealthStore hkHealthStore => hkHealthStore -> Ptr () -> IO ()
recoverActiveWorkoutSessionWithCompletion hkHealthStore completion =
  sendMessage hkHealthStore recoverActiveWorkoutSessionWithCompletionSelector completion

-- | workoutSessionMirroringStartHandler
--
-- Called when a session has started mirroring.
--
-- This property should always be assigned a value promptly after your app is launched,                to ensure it is always observing for incoming mirrored workout sessions.                If your app is not active when a mirrored session starts, it will be launched in the background and given a one-time                permission to start a Live Activity from the background.                The assigned block will be executed on an arbitrary background queue.
--
-- ObjC selector: @- workoutSessionMirroringStartHandler@
workoutSessionMirroringStartHandler :: IsHKHealthStore hkHealthStore => hkHealthStore -> IO (Ptr ())
workoutSessionMirroringStartHandler hkHealthStore =
  sendMessage hkHealthStore workoutSessionMirroringStartHandlerSelector

-- | workoutSessionMirroringStartHandler
--
-- Called when a session has started mirroring.
--
-- This property should always be assigned a value promptly after your app is launched,                to ensure it is always observing for incoming mirrored workout sessions.                If your app is not active when a mirrored session starts, it will be launched in the background and given a one-time                permission to start a Live Activity from the background.                The assigned block will be executed on an arbitrary background queue.
--
-- ObjC selector: @- setWorkoutSessionMirroringStartHandler:@
setWorkoutSessionMirroringStartHandler :: IsHKHealthStore hkHealthStore => hkHealthStore -> Ptr () -> IO ()
setWorkoutSessionMirroringStartHandler hkHealthStore value =
  sendMessage hkHealthStore setWorkoutSessionMirroringStartHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isHealthDataAvailable@
isHealthDataAvailableSelector :: Selector '[] Bool
isHealthDataAvailableSelector = mkSelector "isHealthDataAvailable"

-- | @Selector@ for @supportsHealthRecords@
supportsHealthRecordsSelector :: Selector '[] Bool
supportsHealthRecordsSelector = mkSelector "supportsHealthRecords"

-- | @Selector@ for @authorizationStatusForType:@
authorizationStatusForTypeSelector :: Selector '[Id HKObjectType] HKAuthorizationStatus
authorizationStatusForTypeSelector = mkSelector "authorizationStatusForType:"

-- | @Selector@ for @requestAuthorizationToShareTypes:readTypes:completion:@
requestAuthorizationToShareTypes_readTypes_completionSelector :: Selector '[Id NSSet, Id NSSet, Ptr ()] ()
requestAuthorizationToShareTypes_readTypes_completionSelector = mkSelector "requestAuthorizationToShareTypes:readTypes:completion:"

-- | @Selector@ for @requestPerObjectReadAuthorizationForType:predicate:completion:@
requestPerObjectReadAuthorizationForType_predicate_completionSelector :: Selector '[Id HKObjectType, Id NSPredicate, Ptr ()] ()
requestPerObjectReadAuthorizationForType_predicate_completionSelector = mkSelector "requestPerObjectReadAuthorizationForType:predicate:completion:"

-- | @Selector@ for @getRequestStatusForAuthorizationToShareTypes:readTypes:completion:@
getRequestStatusForAuthorizationToShareTypes_readTypes_completionSelector :: Selector '[Id NSSet, Id NSSet, Ptr ()] ()
getRequestStatusForAuthorizationToShareTypes_readTypes_completionSelector = mkSelector "getRequestStatusForAuthorizationToShareTypes:readTypes:completion:"

-- | @Selector@ for @handleAuthorizationForExtensionWithCompletion:@
handleAuthorizationForExtensionWithCompletionSelector :: Selector '[Ptr ()] ()
handleAuthorizationForExtensionWithCompletionSelector = mkSelector "handleAuthorizationForExtensionWithCompletion:"

-- | @Selector@ for @earliestPermittedSampleDate@
earliestPermittedSampleDateSelector :: Selector '[] (Id NSDate)
earliestPermittedSampleDateSelector = mkSelector "earliestPermittedSampleDate"

-- | @Selector@ for @saveObject:withCompletion:@
saveObject_withCompletionSelector :: Selector '[Id HKObject, Ptr ()] ()
saveObject_withCompletionSelector = mkSelector "saveObject:withCompletion:"

-- | @Selector@ for @saveObjects:withCompletion:@
saveObjects_withCompletionSelector :: Selector '[Id NSArray, Ptr ()] ()
saveObjects_withCompletionSelector = mkSelector "saveObjects:withCompletion:"

-- | @Selector@ for @deleteObject:withCompletion:@
deleteObject_withCompletionSelector :: Selector '[Id HKObject, Ptr ()] ()
deleteObject_withCompletionSelector = mkSelector "deleteObject:withCompletion:"

-- | @Selector@ for @deleteObjects:withCompletion:@
deleteObjects_withCompletionSelector :: Selector '[Id NSArray, Ptr ()] ()
deleteObjects_withCompletionSelector = mkSelector "deleteObjects:withCompletion:"

-- | @Selector@ for @deleteObjectsOfType:predicate:withCompletion:@
deleteObjectsOfType_predicate_withCompletionSelector :: Selector '[Id HKObjectType, Id NSPredicate, Ptr ()] ()
deleteObjectsOfType_predicate_withCompletionSelector = mkSelector "deleteObjectsOfType:predicate:withCompletion:"

-- | @Selector@ for @executeQuery:@
executeQuerySelector :: Selector '[Id HKQuery] ()
executeQuerySelector = mkSelector "executeQuery:"

-- | @Selector@ for @stopQuery:@
stopQuerySelector :: Selector '[Id HKQuery] ()
stopQuerySelector = mkSelector "stopQuery:"

-- | @Selector@ for @splitTotalEnergy:startDate:endDate:resultsHandler:@
splitTotalEnergy_startDate_endDate_resultsHandlerSelector :: Selector '[Id HKQuantity, Id NSDate, Id NSDate, Ptr ()] ()
splitTotalEnergy_startDate_endDate_resultsHandlerSelector = mkSelector "splitTotalEnergy:startDate:endDate:resultsHandler:"

-- | @Selector@ for @dateOfBirthWithError:@
dateOfBirthWithErrorSelector :: Selector '[Id NSError] (Id NSDate)
dateOfBirthWithErrorSelector = mkSelector "dateOfBirthWithError:"

-- | @Selector@ for @dateOfBirthComponentsWithError:@
dateOfBirthComponentsWithErrorSelector :: Selector '[Id NSError] (Id NSDateComponents)
dateOfBirthComponentsWithErrorSelector = mkSelector "dateOfBirthComponentsWithError:"

-- | @Selector@ for @biologicalSexWithError:@
biologicalSexWithErrorSelector :: Selector '[Id NSError] (Id HKBiologicalSexObject)
biologicalSexWithErrorSelector = mkSelector "biologicalSexWithError:"

-- | @Selector@ for @bloodTypeWithError:@
bloodTypeWithErrorSelector :: Selector '[Id NSError] (Id HKBloodTypeObject)
bloodTypeWithErrorSelector = mkSelector "bloodTypeWithError:"

-- | @Selector@ for @fitzpatrickSkinTypeWithError:@
fitzpatrickSkinTypeWithErrorSelector :: Selector '[Id NSError] (Id HKFitzpatrickSkinTypeObject)
fitzpatrickSkinTypeWithErrorSelector = mkSelector "fitzpatrickSkinTypeWithError:"

-- | @Selector@ for @wheelchairUseWithError:@
wheelchairUseWithErrorSelector :: Selector '[Id NSError] (Id HKWheelchairUseObject)
wheelchairUseWithErrorSelector = mkSelector "wheelchairUseWithError:"

-- | @Selector@ for @activityMoveModeWithError:@
activityMoveModeWithErrorSelector :: Selector '[Id NSError] (Id HKActivityMoveModeObject)
activityMoveModeWithErrorSelector = mkSelector "activityMoveModeWithError:"

-- | @Selector@ for @relateWorkoutEffortSample:withWorkout:activity:completion:@
relateWorkoutEffortSample_withWorkout_activity_completionSelector :: Selector '[Id HKSample, Id HKWorkout, Id HKWorkoutActivity, Ptr ()] ()
relateWorkoutEffortSample_withWorkout_activity_completionSelector = mkSelector "relateWorkoutEffortSample:withWorkout:activity:completion:"

-- | @Selector@ for @unrelateWorkoutEffortSample:fromWorkout:activity:completion:@
unrelateWorkoutEffortSample_fromWorkout_activity_completionSelector :: Selector '[Id HKSample, Id HKWorkout, Id HKWorkoutActivity, Ptr ()] ()
unrelateWorkoutEffortSample_fromWorkout_activity_completionSelector = mkSelector "unrelateWorkoutEffortSample:fromWorkout:activity:completion:"

-- | @Selector@ for @recalibrateEstimatesForSampleType:atDate:completion:@
recalibrateEstimatesForSampleType_atDate_completionSelector :: Selector '[Id HKSampleType, Id NSDate, Ptr ()] ()
recalibrateEstimatesForSampleType_atDate_completionSelector = mkSelector "recalibrateEstimatesForSampleType:atDate:completion:"

-- | @Selector@ for @enableBackgroundDeliveryForType:frequency:withCompletion:@
enableBackgroundDeliveryForType_frequency_withCompletionSelector :: Selector '[Id HKObjectType, HKUpdateFrequency, Ptr ()] ()
enableBackgroundDeliveryForType_frequency_withCompletionSelector = mkSelector "enableBackgroundDeliveryForType:frequency:withCompletion:"

-- | @Selector@ for @disableBackgroundDeliveryForType:withCompletion:@
disableBackgroundDeliveryForType_withCompletionSelector :: Selector '[Id HKObjectType, Ptr ()] ()
disableBackgroundDeliveryForType_withCompletionSelector = mkSelector "disableBackgroundDeliveryForType:withCompletion:"

-- | @Selector@ for @disableAllBackgroundDeliveryWithCompletion:@
disableAllBackgroundDeliveryWithCompletionSelector :: Selector '[Ptr ()] ()
disableAllBackgroundDeliveryWithCompletionSelector = mkSelector "disableAllBackgroundDeliveryWithCompletion:"

-- | @Selector@ for @addSamples:toWorkout:completion:@
addSamples_toWorkout_completionSelector :: Selector '[Id NSArray, Id HKWorkout, Ptr ()] ()
addSamples_toWorkout_completionSelector = mkSelector "addSamples:toWorkout:completion:"

-- | @Selector@ for @startWorkoutSession:@
startWorkoutSessionSelector :: Selector '[Id HKWorkoutSession] ()
startWorkoutSessionSelector = mkSelector "startWorkoutSession:"

-- | @Selector@ for @endWorkoutSession:@
endWorkoutSessionSelector :: Selector '[Id HKWorkoutSession] ()
endWorkoutSessionSelector = mkSelector "endWorkoutSession:"

-- | @Selector@ for @pauseWorkoutSession:@
pauseWorkoutSessionSelector :: Selector '[Id HKWorkoutSession] ()
pauseWorkoutSessionSelector = mkSelector "pauseWorkoutSession:"

-- | @Selector@ for @resumeWorkoutSession:@
resumeWorkoutSessionSelector :: Selector '[Id HKWorkoutSession] ()
resumeWorkoutSessionSelector = mkSelector "resumeWorkoutSession:"

-- | @Selector@ for @startWatchAppWithWorkoutConfiguration:completion:@
startWatchAppWithWorkoutConfiguration_completionSelector :: Selector '[Id HKWorkoutConfiguration, Ptr ()] ()
startWatchAppWithWorkoutConfiguration_completionSelector = mkSelector "startWatchAppWithWorkoutConfiguration:completion:"

-- | @Selector@ for @recoverActiveWorkoutSessionWithCompletion:@
recoverActiveWorkoutSessionWithCompletionSelector :: Selector '[Ptr ()] ()
recoverActiveWorkoutSessionWithCompletionSelector = mkSelector "recoverActiveWorkoutSessionWithCompletion:"

-- | @Selector@ for @workoutSessionMirroringStartHandler@
workoutSessionMirroringStartHandlerSelector :: Selector '[] (Ptr ())
workoutSessionMirroringStartHandlerSelector = mkSelector "workoutSessionMirroringStartHandler"

-- | @Selector@ for @setWorkoutSessionMirroringStartHandler:@
setWorkoutSessionMirroringStartHandlerSelector :: Selector '[Ptr ()] ()
setWorkoutSessionMirroringStartHandlerSelector = mkSelector "setWorkoutSessionMirroringStartHandler:"

