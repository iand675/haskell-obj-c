{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterRegisterClientParams@.
module ObjC.Matter.MTRICDManagementClusterRegisterClientParams
  ( MTRICDManagementClusterRegisterClientParams
  , IsMTRICDManagementClusterRegisterClientParams(..)
  , checkInNodeID
  , setCheckInNodeID
  , monitoredSubject
  , setMonitoredSubject
  , key
  , setKey
  , verificationKey
  , setVerificationKey
  , clientType
  , setClientType
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , checkInNodeIDSelector
  , clientTypeSelector
  , keySelector
  , monitoredSubjectSelector
  , serverSideProcessingTimeoutSelector
  , setCheckInNodeIDSelector
  , setClientTypeSelector
  , setKeySelector
  , setMonitoredSubjectSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setVerificationKeySelector
  , timedInvokeTimeoutMsSelector
  , verificationKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- checkInNodeID@
checkInNodeID :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
checkInNodeID mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams checkInNodeIDSelector

-- | @- setCheckInNodeID:@
setCheckInNodeID :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setCheckInNodeID mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setCheckInNodeIDSelector (toNSNumber value)

-- | @- monitoredSubject@
monitoredSubject :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
monitoredSubject mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams monitoredSubjectSelector

-- | @- setMonitoredSubject:@
setMonitoredSubject :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setMonitoredSubject mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setMonitoredSubjectSelector (toNSNumber value)

-- | @- key@
key :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSData)
key mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams keySelector

-- | @- setKey:@
setKey :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSData value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setKey mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setKeySelector (toNSData value)

-- | @- verificationKey@
verificationKey :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSData)
verificationKey mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams verificationKeySelector

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSData value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setVerificationKey mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setVerificationKeySelector (toNSData value)

-- | @- clientType@
clientType :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
clientType mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams clientTypeSelector

-- | @- setClientType:@
setClientType :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setClientType mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setClientTypeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setTimedInvokeTimeoutMs mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams => mtricdManagementClusterRegisterClientParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtricdManagementClusterRegisterClientParams =
  sendMessage mtricdManagementClusterRegisterClientParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRICDManagementClusterRegisterClientParams mtricdManagementClusterRegisterClientParams, IsNSNumber value) => mtricdManagementClusterRegisterClientParams -> value -> IO ()
setServerSideProcessingTimeout mtricdManagementClusterRegisterClientParams value =
  sendMessage mtricdManagementClusterRegisterClientParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkInNodeID@
checkInNodeIDSelector :: Selector '[] (Id NSNumber)
checkInNodeIDSelector = mkSelector "checkInNodeID"

-- | @Selector@ for @setCheckInNodeID:@
setCheckInNodeIDSelector :: Selector '[Id NSNumber] ()
setCheckInNodeIDSelector = mkSelector "setCheckInNodeID:"

-- | @Selector@ for @monitoredSubject@
monitoredSubjectSelector :: Selector '[] (Id NSNumber)
monitoredSubjectSelector = mkSelector "monitoredSubject"

-- | @Selector@ for @setMonitoredSubject:@
setMonitoredSubjectSelector :: Selector '[Id NSNumber] ()
setMonitoredSubjectSelector = mkSelector "setMonitoredSubject:"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSData)
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[Id NSData] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @verificationKey@
verificationKeySelector :: Selector '[] (Id NSData)
verificationKeySelector = mkSelector "verificationKey"

-- | @Selector@ for @setVerificationKey:@
setVerificationKeySelector :: Selector '[Id NSData] ()
setVerificationKeySelector = mkSelector "setVerificationKey:"

-- | @Selector@ for @clientType@
clientTypeSelector :: Selector '[] (Id NSNumber)
clientTypeSelector = mkSelector "clientType"

-- | @Selector@ for @setClientType:@
setClientTypeSelector :: Selector '[Id NSNumber] ()
setClientTypeSelector = mkSelector "setClientType:"

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

