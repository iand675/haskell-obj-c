{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterArmFailSafeParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterArmFailSafeParams
  ( MTRGeneralCommissioningClusterArmFailSafeParams
  , IsMTRGeneralCommissioningClusterArmFailSafeParams(..)
  , expiryLengthSeconds
  , setExpiryLengthSeconds
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , breadcrumbSelector
  , expiryLengthSecondsSelector
  , serverSideProcessingTimeoutSelector
  , setBreadcrumbSelector
  , setExpiryLengthSecondsSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- expiryLengthSeconds@
expiryLengthSeconds :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
expiryLengthSeconds mtrGeneralCommissioningClusterArmFailSafeParams =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams expiryLengthSecondsSelector

-- | @- setExpiryLengthSeconds:@
setExpiryLengthSeconds :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setExpiryLengthSeconds mtrGeneralCommissioningClusterArmFailSafeParams value =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams setExpiryLengthSecondsSelector (toNSNumber value)

-- | @- breadcrumb@
breadcrumb :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
breadcrumb mtrGeneralCommissioningClusterArmFailSafeParams =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setBreadcrumb mtrGeneralCommissioningClusterArmFailSafeParams value =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams setBreadcrumbSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralCommissioningClusterArmFailSafeParams =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralCommissioningClusterArmFailSafeParams value =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams => mtrGeneralCommissioningClusterArmFailSafeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralCommissioningClusterArmFailSafeParams =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralCommissioningClusterArmFailSafeParams mtrGeneralCommissioningClusterArmFailSafeParams, IsNSNumber value) => mtrGeneralCommissioningClusterArmFailSafeParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralCommissioningClusterArmFailSafeParams value =
  sendMessage mtrGeneralCommissioningClusterArmFailSafeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expiryLengthSeconds@
expiryLengthSecondsSelector :: Selector '[] (Id NSNumber)
expiryLengthSecondsSelector = mkSelector "expiryLengthSeconds"

-- | @Selector@ for @setExpiryLengthSeconds:@
setExpiryLengthSecondsSelector :: Selector '[Id NSNumber] ()
setExpiryLengthSecondsSelector = mkSelector "setExpiryLengthSeconds:"

-- | @Selector@ for @breadcrumb@
breadcrumbSelector :: Selector '[] (Id NSNumber)
breadcrumbSelector = mkSelector "breadcrumb"

-- | @Selector@ for @setBreadcrumb:@
setBreadcrumbSelector :: Selector '[Id NSNumber] ()
setBreadcrumbSelector = mkSelector "setBreadcrumb:"

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

