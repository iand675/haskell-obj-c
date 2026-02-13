{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , configurationSelector
  , initSelector
  , initWithConfigurationSelector
  , invalidateSelector
  , newSelector
  , pendingCallActionsOfClass_withCallUUIDSelector
  , pendingTransactionsSelector
  , reportCallWithUUID_endedAtDate_reasonSelector
  , reportCallWithUUID_updatedSelector
  , reportNewIncomingCallWithUUID_update_completionSelector
  , reportNewIncomingVoIPPushPayload_completionSelector
  , reportOutgoingCallWithUUID_connectedAtDateSelector
  , reportOutgoingCallWithUUID_startedConnectingAtDateSelector
  , setConfigurationSelector
  , setDelegate_queueSelector

  -- * Enum types
  , CXCallEndedReason(CXCallEndedReason)
  , pattern CXCallEndedReasonFailed
  , pattern CXCallEndedReasonRemoteEnded
  , pattern CXCallEndedReasonUnanswered
  , pattern CXCallEndedReasonAnsweredElsewhere
  , pattern CXCallEndedReasonDeclinedElsewhere

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.CallKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize a new provider instance with the supplied configuration
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsCXProvider cxProvider, IsCXProviderConfiguration configuration) => cxProvider -> configuration -> IO (Id CXProvider)
initWithConfiguration cxProvider configuration =
  sendOwnedMessage cxProvider initWithConfigurationSelector (toCXProviderConfiguration configuration)

-- | @+ new@
new :: IO (Id CXProvider)
new  =
  do
    cls' <- getRequiredClass "CXProvider"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsCXProvider cxProvider => cxProvider -> IO (Id CXProvider)
init_ cxProvider =
  sendOwnedMessage cxProvider initSelector

-- | Set delegate and optional queue for delegate callbacks to be performed on. A nil queue implies that delegate callbacks should happen on the main queue. The delegate is stored weakly
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsCXProvider cxProvider, IsNSObject queue) => cxProvider -> RawId -> queue -> IO ()
setDelegate_queue cxProvider delegate queue =
  sendMessage cxProvider setDelegate_queueSelector delegate (toNSObject queue)

-- | Report a new incoming call to the system.
--
-- If completion is invoked with a non-nil @error@, the incoming call has been disallowed by the system and will not be displayed, so the provider should not proceed with the call.
--
-- Completion block will be called on delegate queue, if specified, otherwise on a private serial queue.
--
-- ObjC selector: @- reportNewIncomingCallWithUUID:update:completion:@
reportNewIncomingCallWithUUID_update_completion :: (IsCXProvider cxProvider, IsNSUUID uuid, IsCXCallUpdate update) => cxProvider -> uuid -> update -> Ptr () -> IO ()
reportNewIncomingCallWithUUID_update_completion cxProvider uuid update completion =
  sendMessage cxProvider reportNewIncomingCallWithUUID_update_completionSelector (toNSUUID uuid) (toCXCallUpdate update) completion

-- | Report an update to call information.
--
-- ObjC selector: @- reportCallWithUUID:updated:@
reportCallWithUUID_updated :: (IsCXProvider cxProvider, IsNSUUID uuid, IsCXCallUpdate update) => cxProvider -> uuid -> update -> IO ()
reportCallWithUUID_updated cxProvider uuid update =
  sendMessage cxProvider reportCallWithUUID_updatedSelector (toNSUUID uuid) (toCXCallUpdate update)

-- | Report that a call ended. A nil value for @dateEnded@ results in the ended date being set to now.
--
-- ObjC selector: @- reportCallWithUUID:endedAtDate:reason:@
reportCallWithUUID_endedAtDate_reason :: (IsCXProvider cxProvider, IsNSUUID uuid, IsNSDate dateEnded) => cxProvider -> uuid -> dateEnded -> CXCallEndedReason -> IO ()
reportCallWithUUID_endedAtDate_reason cxProvider uuid dateEnded endedReason =
  sendMessage cxProvider reportCallWithUUID_endedAtDate_reasonSelector (toNSUUID uuid) (toNSDate dateEnded) endedReason

-- | Report that an outgoing call started connecting. A nil value for @dateStartedConnecting@ results in the started connecting date being set to now.
--
-- ObjC selector: @- reportOutgoingCallWithUUID:startedConnectingAtDate:@
reportOutgoingCallWithUUID_startedConnectingAtDate :: (IsCXProvider cxProvider, IsNSUUID uuid, IsNSDate dateStartedConnecting) => cxProvider -> uuid -> dateStartedConnecting -> IO ()
reportOutgoingCallWithUUID_startedConnectingAtDate cxProvider uuid dateStartedConnecting =
  sendMessage cxProvider reportOutgoingCallWithUUID_startedConnectingAtDateSelector (toNSUUID uuid) (toNSDate dateStartedConnecting)

-- | Report that an outgoing call connected. A nil value for @dateConnected@ results in the connected date being set to now.
--
-- ObjC selector: @- reportOutgoingCallWithUUID:connectedAtDate:@
reportOutgoingCallWithUUID_connectedAtDate :: (IsCXProvider cxProvider, IsNSUUID uuid, IsNSDate dateConnected) => cxProvider -> uuid -> dateConnected -> IO ()
reportOutgoingCallWithUUID_connectedAtDate cxProvider uuid dateConnected =
  sendMessage cxProvider reportOutgoingCallWithUUID_connectedAtDateSelector (toNSUUID uuid) (toNSDate dateConnected)

-- | From within a Notification Service Extension, request the containing application be launched to handle an incoming VoIP call. The application's PKPushRegistryDelegate must handle the push upon launch.
--
-- ObjC selector: @+ reportNewIncomingVoIPPushPayload:completion:@
reportNewIncomingVoIPPushPayload_completion :: IsNSDictionary dictionaryPayload => dictionaryPayload -> Ptr () -> IO ()
reportNewIncomingVoIPPushPayload_completion dictionaryPayload completion =
  do
    cls' <- getRequiredClass "CXProvider"
    sendClassMessage cls' reportNewIncomingVoIPPushPayload_completionSelector (toNSDictionary dictionaryPayload) completion

-- | Invalidate the receiver. All existing calls will be marked as ended in failure. The provider must be invalidated before it is deallocated.
--
-- ObjC selector: @- invalidate@
invalidate :: IsCXProvider cxProvider => cxProvider -> IO ()
invalidate cxProvider =
  sendMessage cxProvider invalidateSelector

-- | Returns subset of call actions contained in any transaction in -pendingTransactions of the specified class and with the specified call UUID.
--
-- ObjC selector: @- pendingCallActionsOfClass:withCallUUID:@
pendingCallActionsOfClass_withCallUUID :: (IsCXProvider cxProvider, IsNSUUID callUUID) => cxProvider -> Class -> callUUID -> IO (Id NSArray)
pendingCallActionsOfClass_withCallUUID cxProvider callActionClass callUUID =
  sendMessage cxProvider pendingCallActionsOfClass_withCallUUIDSelector callActionClass (toNSUUID callUUID)

-- | The receiver's current configuration.
--
-- ObjC selector: @- configuration@
configuration :: IsCXProvider cxProvider => cxProvider -> IO (Id CXProviderConfiguration)
configuration cxProvider =
  sendMessage cxProvider configurationSelector

-- | The receiver's current configuration.
--
-- ObjC selector: @- setConfiguration:@
setConfiguration :: (IsCXProvider cxProvider, IsCXProviderConfiguration value) => cxProvider -> value -> IO ()
setConfiguration cxProvider value =
  sendMessage cxProvider setConfigurationSelector (toCXProviderConfiguration value)

-- | List of all transactions that are incomplete.
--
-- ObjC selector: @- pendingTransactions@
pendingTransactions :: IsCXProvider cxProvider => cxProvider -> IO (Id NSArray)
pendingTransactions cxProvider =
  sendMessage cxProvider pendingTransactionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector '[Id CXProviderConfiguration] (Id CXProvider)
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CXProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @reportNewIncomingCallWithUUID:update:completion:@
reportNewIncomingCallWithUUID_update_completionSelector :: Selector '[Id NSUUID, Id CXCallUpdate, Ptr ()] ()
reportNewIncomingCallWithUUID_update_completionSelector = mkSelector "reportNewIncomingCallWithUUID:update:completion:"

-- | @Selector@ for @reportCallWithUUID:updated:@
reportCallWithUUID_updatedSelector :: Selector '[Id NSUUID, Id CXCallUpdate] ()
reportCallWithUUID_updatedSelector = mkSelector "reportCallWithUUID:updated:"

-- | @Selector@ for @reportCallWithUUID:endedAtDate:reason:@
reportCallWithUUID_endedAtDate_reasonSelector :: Selector '[Id NSUUID, Id NSDate, CXCallEndedReason] ()
reportCallWithUUID_endedAtDate_reasonSelector = mkSelector "reportCallWithUUID:endedAtDate:reason:"

-- | @Selector@ for @reportOutgoingCallWithUUID:startedConnectingAtDate:@
reportOutgoingCallWithUUID_startedConnectingAtDateSelector :: Selector '[Id NSUUID, Id NSDate] ()
reportOutgoingCallWithUUID_startedConnectingAtDateSelector = mkSelector "reportOutgoingCallWithUUID:startedConnectingAtDate:"

-- | @Selector@ for @reportOutgoingCallWithUUID:connectedAtDate:@
reportOutgoingCallWithUUID_connectedAtDateSelector :: Selector '[Id NSUUID, Id NSDate] ()
reportOutgoingCallWithUUID_connectedAtDateSelector = mkSelector "reportOutgoingCallWithUUID:connectedAtDate:"

-- | @Selector@ for @reportNewIncomingVoIPPushPayload:completion:@
reportNewIncomingVoIPPushPayload_completionSelector :: Selector '[Id NSDictionary, Ptr ()] ()
reportNewIncomingVoIPPushPayload_completionSelector = mkSelector "reportNewIncomingVoIPPushPayload:completion:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @pendingCallActionsOfClass:withCallUUID:@
pendingCallActionsOfClass_withCallUUIDSelector :: Selector '[Class, Id NSUUID] (Id NSArray)
pendingCallActionsOfClass_withCallUUIDSelector = mkSelector "pendingCallActionsOfClass:withCallUUID:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id CXProviderConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector '[Id CXProviderConfiguration] ()
setConfigurationSelector = mkSelector "setConfiguration:"

-- | @Selector@ for @pendingTransactions@
pendingTransactionsSelector :: Selector '[] (Id NSArray)
pendingTransactionsSelector = mkSelector "pendingTransactions"

