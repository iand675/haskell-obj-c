{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDirectoryClusterAddNetworkParams@.
module ObjC.Matter.MTRThreadNetworkDirectoryClusterAddNetworkParams
  ( MTRThreadNetworkDirectoryClusterAddNetworkParams
  , IsMTRThreadNetworkDirectoryClusterAddNetworkParams(..)
  , operationalDataset
  , setOperationalDataset
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , operationalDatasetSelector
  , serverSideProcessingTimeoutSelector
  , setOperationalDatasetSelector
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

-- | @- operationalDataset@
operationalDataset :: IsMTRThreadNetworkDirectoryClusterAddNetworkParams mtrThreadNetworkDirectoryClusterAddNetworkParams => mtrThreadNetworkDirectoryClusterAddNetworkParams -> IO (Id NSData)
operationalDataset mtrThreadNetworkDirectoryClusterAddNetworkParams =
  sendMessage mtrThreadNetworkDirectoryClusterAddNetworkParams operationalDatasetSelector

-- | @- setOperationalDataset:@
setOperationalDataset :: (IsMTRThreadNetworkDirectoryClusterAddNetworkParams mtrThreadNetworkDirectoryClusterAddNetworkParams, IsNSData value) => mtrThreadNetworkDirectoryClusterAddNetworkParams -> value -> IO ()
setOperationalDataset mtrThreadNetworkDirectoryClusterAddNetworkParams value =
  sendMessage mtrThreadNetworkDirectoryClusterAddNetworkParams setOperationalDatasetSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThreadNetworkDirectoryClusterAddNetworkParams mtrThreadNetworkDirectoryClusterAddNetworkParams => mtrThreadNetworkDirectoryClusterAddNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThreadNetworkDirectoryClusterAddNetworkParams =
  sendMessage mtrThreadNetworkDirectoryClusterAddNetworkParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThreadNetworkDirectoryClusterAddNetworkParams mtrThreadNetworkDirectoryClusterAddNetworkParams, IsNSNumber value) => mtrThreadNetworkDirectoryClusterAddNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThreadNetworkDirectoryClusterAddNetworkParams value =
  sendMessage mtrThreadNetworkDirectoryClusterAddNetworkParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThreadNetworkDirectoryClusterAddNetworkParams mtrThreadNetworkDirectoryClusterAddNetworkParams => mtrThreadNetworkDirectoryClusterAddNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThreadNetworkDirectoryClusterAddNetworkParams =
  sendMessage mtrThreadNetworkDirectoryClusterAddNetworkParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThreadNetworkDirectoryClusterAddNetworkParams mtrThreadNetworkDirectoryClusterAddNetworkParams, IsNSNumber value) => mtrThreadNetworkDirectoryClusterAddNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrThreadNetworkDirectoryClusterAddNetworkParams value =
  sendMessage mtrThreadNetworkDirectoryClusterAddNetworkParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationalDataset@
operationalDatasetSelector :: Selector '[] (Id NSData)
operationalDatasetSelector = mkSelector "operationalDataset"

-- | @Selector@ for @setOperationalDataset:@
setOperationalDatasetSelector :: Selector '[Id NSData] ()
setOperationalDatasetSelector = mkSelector "setOperationalDataset:"

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

