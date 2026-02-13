{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterUnregisterClientParams@.
module ObjC.Matter.MTRICDManagementClusterUnregisterClientParams
  ( MTRICDManagementClusterUnregisterClientParams
  , IsMTRICDManagementClusterUnregisterClientParams(..)
  , checkInNodeID
  , setCheckInNodeID
  , verificationKey
  , setVerificationKey
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , checkInNodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setCheckInNodeIDSelector
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
checkInNodeID :: IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams => mtricdManagementClusterUnregisterClientParams -> IO (Id NSNumber)
checkInNodeID mtricdManagementClusterUnregisterClientParams =
  sendMessage mtricdManagementClusterUnregisterClientParams checkInNodeIDSelector

-- | @- setCheckInNodeID:@
setCheckInNodeID :: (IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams, IsNSNumber value) => mtricdManagementClusterUnregisterClientParams -> value -> IO ()
setCheckInNodeID mtricdManagementClusterUnregisterClientParams value =
  sendMessage mtricdManagementClusterUnregisterClientParams setCheckInNodeIDSelector (toNSNumber value)

-- | @- verificationKey@
verificationKey :: IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams => mtricdManagementClusterUnregisterClientParams -> IO (Id NSData)
verificationKey mtricdManagementClusterUnregisterClientParams =
  sendMessage mtricdManagementClusterUnregisterClientParams verificationKeySelector

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams, IsNSData value) => mtricdManagementClusterUnregisterClientParams -> value -> IO ()
setVerificationKey mtricdManagementClusterUnregisterClientParams value =
  sendMessage mtricdManagementClusterUnregisterClientParams setVerificationKeySelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams => mtricdManagementClusterUnregisterClientParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtricdManagementClusterUnregisterClientParams =
  sendMessage mtricdManagementClusterUnregisterClientParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams, IsNSNumber value) => mtricdManagementClusterUnregisterClientParams -> value -> IO ()
setTimedInvokeTimeoutMs mtricdManagementClusterUnregisterClientParams value =
  sendMessage mtricdManagementClusterUnregisterClientParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams => mtricdManagementClusterUnregisterClientParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtricdManagementClusterUnregisterClientParams =
  sendMessage mtricdManagementClusterUnregisterClientParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRICDManagementClusterUnregisterClientParams mtricdManagementClusterUnregisterClientParams, IsNSNumber value) => mtricdManagementClusterUnregisterClientParams -> value -> IO ()
setServerSideProcessingTimeout mtricdManagementClusterUnregisterClientParams value =
  sendMessage mtricdManagementClusterUnregisterClientParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkInNodeID@
checkInNodeIDSelector :: Selector '[] (Id NSNumber)
checkInNodeIDSelector = mkSelector "checkInNodeID"

-- | @Selector@ for @setCheckInNodeID:@
setCheckInNodeIDSelector :: Selector '[Id NSNumber] ()
setCheckInNodeIDSelector = mkSelector "setCheckInNodeID:"

-- | @Selector@ for @verificationKey@
verificationKeySelector :: Selector '[] (Id NSData)
verificationKeySelector = mkSelector "verificationKey"

-- | @Selector@ for @setVerificationKey:@
setVerificationKeySelector :: Selector '[Id NSData] ()
setVerificationKeySelector = mkSelector "setVerificationKey:"

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

