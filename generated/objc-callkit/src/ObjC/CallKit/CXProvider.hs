{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXProvider@.
module ObjC.CallKit.CXProvider
  ( CXProvider
  , IsCXProvider(..)
  , initWithConfiguration
  , new
  , init_
  , setDelegate_queue
  , reportNewIncomingCallWithUUID_update_completion
  , reportCallWithUUID_updated
  , reportCallWithUUID_endedAtDate_reason
  , reportOutgoingCallWithUUID_startedConnectingAtDate
  , reportOutgoingCallWithUUID_connectedAtDate
  , reportNewIncomingVoIPPushPayload_completion
  , invalidate
  , pendingCallActionsOfClass_withCallUUID
  , configuration
  , setConfiguration
  , pendingTransactions
  , initWithConfigurationSelector
  , newSelector
  , initSelector
  , setDelegate_queueSelector
  , reportNewIncomingCallWithUUID_update_completionSelector
  , reportCallWithUUID_updatedSelector
  , reportCallWithUUID_endedAtDate_reasonSelector
  , reportOutgoingCallWithUUID_startedConnectingAtDateSelector
  , reportOutgoingCallWithUUID_connectedAtDateSelector
  , reportNewIncomingVoIPPushPayload_completionSelector
  , invalidateSelector
  , pendingCallActionsOfClass_withCallUUIDSelector
  , configurationSelector
  , setConfigurationSelector
  , pendingTransactionsSelector

  -- * Enum types
  , CXCallEndedReason(CXCallEndedReason)
  , pattern CXCallEndedReasonFailed
  , pattern CXCallEndedReasonRemoteEnded
  , pattern CXCallEndedReasonUnanswered
  , pattern CXCallEndedReasonAnsweredElsewhere
  , pattern CXCallEndedReasonDeclinedElsewhere

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

import ObjC.CallKit.Internal.Classes
import ObjC.CallKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize a new provider instance with the supplied configuration
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsCXProvider cxProvider, IsCXProviderConfiguration configuration) => cxProvider -> configuration -> IO (Id CXProvider)
initWithConfiguration cxProvider  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg cxProvider (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CXProvider)
new  =
  do
    cls' <- getRequiredClass "CXProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCXProvider cxProvider => cxProvider -> IO (Id CXProvider)
init_ cxProvider  =
  sendMsg cxProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Set delegate and optional queue for delegate callbacks to be performed on. A nil queue implies that delegate callbacks should happen on the main queue. The delegate is stored weakly
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsCXProvider cxProvider, IsNSObject queue) => cxProvider -> RawId -> queue -> IO ()
setDelegate_queue cxProvider  delegate queue =
withObjCPtr queue $ \raw_queue ->
    sendMsg cxProvider (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Report a new incoming call to the system.
--
-- If completion is invoked with a non-nil @error@, the incoming call has been disallowed by the system and will not be displayed, so the provider should not proceed with the call.
--
-- Completion block will be called on delegate queue, if specified, otherwise on a private serial queue.
--
-- ObjC selector: @- reportNewIncomingCallWithUUID:update:completion:@
reportNewIncomingCallWithUUID_update_completion :: (IsCXProvider cxProvider, IsNSUUID uuid, IsCXCallUpdate update) => cxProvider -> uuid -> update -> Ptr () -> IO ()
reportNewIncomingCallWithUUID_update_completion cxProvider  uuid update completion =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr update $ \raw_update ->
      sendMsg cxProvider (mkSelector "reportNewIncomingCallWithUUID:update:completion:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_update :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Report an update to call information.
--
-- ObjC selector: @- reportCallWithUUID:updated:@
reportCallWithUUID_updated :: (IsCXProvider cxProvider, IsNSUUID uuid, IsCXCallUpdate update) => cxProvider -> uuid -> update -> IO ()
reportCallWithUUID_updated cxProvider  uuid update =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr update $ \raw_update ->
      sendMsg cxProvider (mkSelector "reportCallWithUUID:updated:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_update :: Ptr ())]

-- | Report that a call ended. A nil value for @dateEnded@ results in the ended date being set to now.
--
-- ObjC selector: @- reportCallWithUUID:endedAtDate:reason:@
reportCallWithUUID_endedAtDate_reason :: (IsCXProvider cxProvider, IsNSUUID uuid, IsNSDate dateEnded) => cxProvider -> uuid -> dateEnded -> CXCallEndedReason -> IO ()
reportCallWithUUID_endedAtDate_reason cxProvider  uuid dateEnded endedReason =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr dateEnded $ \raw_dateEnded ->
      sendMsg cxProvider (mkSelector "reportCallWithUUID:endedAtDate:reason:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_dateEnded :: Ptr ()), argCLong (coerce endedReason)]

-- | Report that an outgoing call started connecting. A nil value for @dateStartedConnecting@ results in the started connecting date being set to now.
--
-- ObjC selector: @- reportOutgoingCallWithUUID:startedConnectingAtDate:@
reportOutgoingCallWithUUID_startedConnectingAtDate :: (IsCXProvider cxProvider, IsNSUUID uuid, IsNSDate dateStartedConnecting) => cxProvider -> uuid -> dateStartedConnecting -> IO ()
reportOutgoingCallWithUUID_startedConnectingAtDate cxProvider  uuid dateStartedConnecting =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr dateStartedConnecting $ \raw_dateStartedConnecting ->
      sendMsg cxProvider (mkSelector "reportOutgoingCallWithUUID:startedConnectingAtDate:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_dateStartedConnecting :: Ptr ())]

-- | Report that an outgoing call connected. A nil value for @dateConnected@ results in the connected date being set to now.
--
-- ObjC selector: @- reportOutgoingCallWithUUID:connectedAtDate:@
reportOutgoingCallWithUUID_connectedAtDate :: (IsCXProvider cxProvider, IsNSUUID uuid, IsNSDate dateConnected) => cxProvider -> uuid -> dateConnected -> IO ()
reportOutgoingCallWithUUID_connectedAtDate cxProvider  uuid dateConnected =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr dateConnected $ \raw_dateConnected ->
      sendMsg cxProvider (mkSelector "reportOutgoingCallWithUUID:connectedAtDate:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_dateConnected :: Ptr ())]

-- | From within a Notification Service Extension, request the containing application be launched to handle an incoming VoIP call. The application's PKPushRegistryDelegate must handle the push upon launch.
--
-- ObjC selector: @+ reportNewIncomingVoIPPushPayload:completion:@
reportNewIncomingVoIPPushPayload_completion :: IsNSDictionary dictionaryPayload => dictionaryPayload -> Ptr () -> IO ()
reportNewIncomingVoIPPushPayload_completion dictionaryPayload completion =
  do
    cls' <- getRequiredClass "CXProvider"
    withObjCPtr dictionaryPayload $ \raw_dictionaryPayload ->
      sendClassMsg cls' (mkSelector "reportNewIncomingVoIPPushPayload:completion:") retVoid [argPtr (castPtr raw_dictionaryPayload :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Invalidate the receiver. All existing calls will be marked as ended in failure. The provider must be invalidated before it is deallocated.
--
-- ObjC selector: @- invalidate@
invalidate :: IsCXProvider cxProvider => cxProvider -> IO ()
invalidate cxProvider  =
  sendMsg cxProvider (mkSelector "invalidate") retVoid []

-- | Returns subset of call actions contained in any transaction in -pendingTransactions of the specified class and with the specified call UUID.
--
-- ObjC selector: @- pendingCallActionsOfClass:withCallUUID:@
pendingCallActionsOfClass_withCallUUID :: (IsCXProvider cxProvider, IsNSUUID callUUID) => cxProvider -> Class -> callUUID -> IO (Id NSArray)
pendingCallActionsOfClass_withCallUUID cxProvider  callActionClass callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxProvider (mkSelector "pendingCallActionsOfClass:withCallUUID:") (retPtr retVoid) [argPtr (unClass callActionClass), argPtr (castPtr raw_callUUID :: Ptr ())] >>= retainedObject . castPtr

-- | The receiver's current configuration.
--
-- ObjC selector: @- configuration@
configuration :: IsCXProvider cxProvider => cxProvider -> IO (Id CXProviderConfiguration)
configuration cxProvider  =
  sendMsg cxProvider (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The receiver's current configuration.
--
-- ObjC selector: @- setConfiguration:@
setConfiguration :: (IsCXProvider cxProvider, IsCXProviderConfiguration value) => cxProvider -> value -> IO ()
setConfiguration cxProvider  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxProvider (mkSelector "setConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | List of all transactions that are incomplete.
--
-- ObjC selector: @- pendingTransactions@
pendingTransactions :: IsCXProvider cxProvider => cxProvider -> IO (Id NSArray)
pendingTransactions cxProvider  =
  sendMsg cxProvider (mkSelector "pendingTransactions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @reportNewIncomingCallWithUUID:update:completion:@
reportNewIncomingCallWithUUID_update_completionSelector :: Selector
reportNewIncomingCallWithUUID_update_completionSelector = mkSelector "reportNewIncomingCallWithUUID:update:completion:"

-- | @Selector@ for @reportCallWithUUID:updated:@
reportCallWithUUID_updatedSelector :: Selector
reportCallWithUUID_updatedSelector = mkSelector "reportCallWithUUID:updated:"

-- | @Selector@ for @reportCallWithUUID:endedAtDate:reason:@
reportCallWithUUID_endedAtDate_reasonSelector :: Selector
reportCallWithUUID_endedAtDate_reasonSelector = mkSelector "reportCallWithUUID:endedAtDate:reason:"

-- | @Selector@ for @reportOutgoingCallWithUUID:startedConnectingAtDate:@
reportOutgoingCallWithUUID_startedConnectingAtDateSelector :: Selector
reportOutgoingCallWithUUID_startedConnectingAtDateSelector = mkSelector "reportOutgoingCallWithUUID:startedConnectingAtDate:"

-- | @Selector@ for @reportOutgoingCallWithUUID:connectedAtDate:@
reportOutgoingCallWithUUID_connectedAtDateSelector :: Selector
reportOutgoingCallWithUUID_connectedAtDateSelector = mkSelector "reportOutgoingCallWithUUID:connectedAtDate:"

-- | @Selector@ for @reportNewIncomingVoIPPushPayload:completion:@
reportNewIncomingVoIPPushPayload_completionSelector :: Selector
reportNewIncomingVoIPPushPayload_completionSelector = mkSelector "reportNewIncomingVoIPPushPayload:completion:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @pendingCallActionsOfClass:withCallUUID:@
pendingCallActionsOfClass_withCallUUIDSelector :: Selector
pendingCallActionsOfClass_withCallUUIDSelector = mkSelector "pendingCallActionsOfClass:withCallUUID:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector
setConfigurationSelector = mkSelector "setConfiguration:"

-- | @Selector@ for @pendingTransactions@
pendingTransactionsSelector :: Selector
pendingTransactionsSelector = mkSelector "pendingTransactions"

