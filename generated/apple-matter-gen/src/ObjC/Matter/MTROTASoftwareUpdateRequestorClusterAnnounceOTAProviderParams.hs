{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams@.
module ObjC.Matter.MTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams
  ( MTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams
  , IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams(..)
  , providerNodeID
  , setProviderNodeID
  , vendorID
  , setVendorID
  , announcementReason
  , setAnnouncementReason
  , metadataForNode
  , setMetadataForNode
  , endpoint
  , setEndpoint
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , providerNodeId
  , setProviderNodeId
  , vendorId
  , setVendorId
  , announcementReasonSelector
  , endpointSelector
  , metadataForNodeSelector
  , providerNodeIDSelector
  , providerNodeIdSelector
  , serverSideProcessingTimeoutSelector
  , setAnnouncementReasonSelector
  , setEndpointSelector
  , setMetadataForNodeSelector
  , setProviderNodeIDSelector
  , setProviderNodeIdSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setVendorIDSelector
  , setVendorIdSelector
  , timedInvokeTimeoutMsSelector
  , vendorIDSelector
  , vendorIdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- providerNodeID@
providerNodeID :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
providerNodeID mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams providerNodeIDSelector

-- | @- setProviderNodeID:@
setProviderNodeID :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setProviderNodeID mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setProviderNodeIDSelector (toNSNumber value)

-- | @- vendorID@
vendorID :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
vendorID mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setVendorID mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setVendorIDSelector (toNSNumber value)

-- | @- announcementReason@
announcementReason :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
announcementReason mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams announcementReasonSelector

-- | @- setAnnouncementReason:@
setAnnouncementReason :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setAnnouncementReason mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setAnnouncementReasonSelector (toNSNumber value)

-- | @- metadataForNode@
metadataForNode :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSData)
metadataForNode mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams metadataForNodeSelector

-- | @- setMetadataForNode:@
setMetadataForNode :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSData value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setMetadataForNode mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setMetadataForNodeSelector (toNSData value)

-- | @- endpoint@
endpoint :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
endpoint mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setEndpoint mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setEndpointSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setServerSideProcessingTimeout mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- | @- providerNodeId@
providerNodeId :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
providerNodeId mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams providerNodeIdSelector

-- | @- setProviderNodeId:@
setProviderNodeId :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setProviderNodeId mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setProviderNodeIdSelector (toNSNumber value)

-- | @- vendorId@
vendorId :: IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> IO (Id NSNumber)
vendorId mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams vendorIdSelector

-- | @- setVendorId:@
setVendorId :: (IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams -> value -> IO ()
setVendorId mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterAnnounceOTAProviderParams setVendorIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerNodeID@
providerNodeIDSelector :: Selector '[] (Id NSNumber)
providerNodeIDSelector = mkSelector "providerNodeID"

-- | @Selector@ for @setProviderNodeID:@
setProviderNodeIDSelector :: Selector '[Id NSNumber] ()
setProviderNodeIDSelector = mkSelector "setProviderNodeID:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @announcementReason@
announcementReasonSelector :: Selector '[] (Id NSNumber)
announcementReasonSelector = mkSelector "announcementReason"

-- | @Selector@ for @setAnnouncementReason:@
setAnnouncementReasonSelector :: Selector '[Id NSNumber] ()
setAnnouncementReasonSelector = mkSelector "setAnnouncementReason:"

-- | @Selector@ for @metadataForNode@
metadataForNodeSelector :: Selector '[] (Id NSData)
metadataForNodeSelector = mkSelector "metadataForNode"

-- | @Selector@ for @setMetadataForNode:@
setMetadataForNodeSelector :: Selector '[Id NSData] ()
setMetadataForNodeSelector = mkSelector "setMetadataForNode:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

-- | @Selector@ for @providerNodeId@
providerNodeIdSelector :: Selector '[] (Id NSNumber)
providerNodeIdSelector = mkSelector "providerNodeId"

-- | @Selector@ for @setProviderNodeId:@
setProviderNodeIdSelector :: Selector '[Id NSNumber] ()
setProviderNodeIdSelector = mkSelector "setProviderNodeId:"

-- | @Selector@ for @vendorId@
vendorIdSelector :: Selector '[] (Id NSNumber)
vendorIdSelector = mkSelector "vendorId"

-- | @Selector@ for @setVendorId:@
setVendorIdSelector :: Selector '[Id NSNumber] ()
setVendorIdSelector = mkSelector "setVendorId:"

