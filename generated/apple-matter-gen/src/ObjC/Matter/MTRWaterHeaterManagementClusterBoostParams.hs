{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterManagementClusterBoostParams@.
module ObjC.Matter.MTRWaterHeaterManagementClusterBoostParams
  ( MTRWaterHeaterManagementClusterBoostParams
  , IsMTRWaterHeaterManagementClusterBoostParams(..)
  , boostInfo
  , setBoostInfo
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , boostInfoSelector
  , serverSideProcessingTimeoutSelector
  , setBoostInfoSelector
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

-- | @- boostInfo@
boostInfo :: IsMTRWaterHeaterManagementClusterBoostParams mtrWaterHeaterManagementClusterBoostParams => mtrWaterHeaterManagementClusterBoostParams -> IO (Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct)
boostInfo mtrWaterHeaterManagementClusterBoostParams =
  sendMessage mtrWaterHeaterManagementClusterBoostParams boostInfoSelector

-- | @- setBoostInfo:@
setBoostInfo :: (IsMTRWaterHeaterManagementClusterBoostParams mtrWaterHeaterManagementClusterBoostParams, IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value) => mtrWaterHeaterManagementClusterBoostParams -> value -> IO ()
setBoostInfo mtrWaterHeaterManagementClusterBoostParams value =
  sendMessage mtrWaterHeaterManagementClusterBoostParams setBoostInfoSelector (toMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWaterHeaterManagementClusterBoostParams mtrWaterHeaterManagementClusterBoostParams => mtrWaterHeaterManagementClusterBoostParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWaterHeaterManagementClusterBoostParams =
  sendMessage mtrWaterHeaterManagementClusterBoostParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWaterHeaterManagementClusterBoostParams mtrWaterHeaterManagementClusterBoostParams, IsNSNumber value) => mtrWaterHeaterManagementClusterBoostParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWaterHeaterManagementClusterBoostParams value =
  sendMessage mtrWaterHeaterManagementClusterBoostParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWaterHeaterManagementClusterBoostParams mtrWaterHeaterManagementClusterBoostParams => mtrWaterHeaterManagementClusterBoostParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWaterHeaterManagementClusterBoostParams =
  sendMessage mtrWaterHeaterManagementClusterBoostParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWaterHeaterManagementClusterBoostParams mtrWaterHeaterManagementClusterBoostParams, IsNSNumber value) => mtrWaterHeaterManagementClusterBoostParams -> value -> IO ()
setServerSideProcessingTimeout mtrWaterHeaterManagementClusterBoostParams value =
  sendMessage mtrWaterHeaterManagementClusterBoostParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostInfo@
boostInfoSelector :: Selector '[] (Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct)
boostInfoSelector = mkSelector "boostInfo"

-- | @Selector@ for @setBoostInfo:@
setBoostInfoSelector :: Selector '[Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct] ()
setBoostInfoSelector = mkSelector "setBoostInfo:"

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

