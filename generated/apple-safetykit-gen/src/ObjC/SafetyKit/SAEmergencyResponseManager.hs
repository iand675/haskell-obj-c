{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SAEmergencyResponseManager
--
-- Use SAEmergencyResponseManager to request actions in response to an emergency event. Set the delegate to monitor the progress of requested emergency response actions. SAEmergencyResponseManager requires user authorization for at least one of the emergency event detections e.g. SACrashDetectionEvent
--
-- SAEmergencyResponseManager requires an entitlement from Apple to at least one of the emergency event detections. To apply for the entitlement, see respective detection mechanisms
--
-- Generated bindings for @SAEmergencyResponseManager@.
module ObjC.SafetyKit.SAEmergencyResponseManager
  ( SAEmergencyResponseManager
  , IsSAEmergencyResponseManager(..)
  , dialVoiceCallToPhoneNumber_completionHandler
  , delegate
  , setDelegate
  , delegateSelector
  , dialVoiceCallToPhoneNumber_completionHandlerSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafetyKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Requests the system to dial a voice call on behalf of the user. Apps running in the background / foreground can request to dial a voice call without user confirmation. Emergency numbers are not allowed. Requests are accepted only if user has authorized the app to receive and handle emergency detection events and only for a limited time after an emergency event is detected.
--
-- SAEmergencyResponseDelegate
--
-- @phoneNumber@ — Apps can request the system to dial a voice call by providing a phone number. Emergency numbers are not allowed.
--
-- @handler@ — Completion handler invoked with the status of the voice call request. If requested is accepted, the handler is invoked with a nil error. Interpret the error returned using SAErrorDomain. Requests will fail with SAErrorNotAuthorized if user has not authorized the app to receive and handle any emergency events. Requests will fail with SAErrorNotAvailable if invoked outside of the limited time window after an emergency event is detected. Use the SAEmergencyResponseDelegate to monitor the progress of the voice call.
--
-- ObjC selector: @- dialVoiceCallToPhoneNumber:completionHandler:@
dialVoiceCallToPhoneNumber_completionHandler :: (IsSAEmergencyResponseManager saEmergencyResponseManager, IsNSString phoneNumber) => saEmergencyResponseManager -> phoneNumber -> Ptr () -> IO ()
dialVoiceCallToPhoneNumber_completionHandler saEmergencyResponseManager phoneNumber handler =
  sendMessage saEmergencyResponseManager dialVoiceCallToPhoneNumber_completionHandlerSelector (toNSString phoneNumber) handler

-- | delegate
--
-- The delegate object to receive updates about requested emergency response action.
--
-- ObjC selector: @- delegate@
delegate :: IsSAEmergencyResponseManager saEmergencyResponseManager => saEmergencyResponseManager -> IO RawId
delegate saEmergencyResponseManager =
  sendMessage saEmergencyResponseManager delegateSelector

-- | delegate
--
-- The delegate object to receive updates about requested emergency response action.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSAEmergencyResponseManager saEmergencyResponseManager => saEmergencyResponseManager -> RawId -> IO ()
setDelegate saEmergencyResponseManager value =
  sendMessage saEmergencyResponseManager setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dialVoiceCallToPhoneNumber:completionHandler:@
dialVoiceCallToPhoneNumber_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
dialVoiceCallToPhoneNumber_completionHandlerSelector = mkSelector "dialVoiceCallToPhoneNumber:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

