{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterUpdateTwoDCartesianZoneParams@.
module ObjC.Matter.MTRZoneManagementClusterUpdateTwoDCartesianZoneParams
  ( MTRZoneManagementClusterUpdateTwoDCartesianZoneParams
  , IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams(..)
  , zoneID
  , setZoneID
  , zone
  , setZone
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setZoneIDSelector
  , setZoneSelector
  , timedInvokeTimeoutMsSelector
  , zoneIDSelector
  , zoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterUpdateTwoDCartesianZoneParams =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams zoneIDSelector

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsNSNumber value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setZoneID mtrZoneManagementClusterUpdateTwoDCartesianZoneParams value =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams setZoneIDSelector (toNSNumber value)

-- | @- zone@
zone :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id MTRZoneManagementClusterTwoDCartesianZoneStruct)
zone mtrZoneManagementClusterUpdateTwoDCartesianZoneParams =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams zoneSelector

-- | @- setZone:@
setZone :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsMTRZoneManagementClusterTwoDCartesianZoneStruct value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setZone mtrZoneManagementClusterUpdateTwoDCartesianZoneParams value =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams setZoneSelector (toMTRZoneManagementClusterTwoDCartesianZoneStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrZoneManagementClusterUpdateTwoDCartesianZoneParams =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsNSNumber value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrZoneManagementClusterUpdateTwoDCartesianZoneParams value =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrZoneManagementClusterUpdateTwoDCartesianZoneParams =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsNSNumber value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setServerSideProcessingTimeout mtrZoneManagementClusterUpdateTwoDCartesianZoneParams value =
  sendMessage mtrZoneManagementClusterUpdateTwoDCartesianZoneParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id NSNumber)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector '[Id NSNumber] ()
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] (Id MTRZoneManagementClusterTwoDCartesianZoneStruct)
zoneSelector = mkSelector "zone"

-- | @Selector@ for @setZone:@
setZoneSelector :: Selector '[Id MTRZoneManagementClusterTwoDCartesianZoneStruct] ()
setZoneSelector = mkSelector "setZone:"

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

