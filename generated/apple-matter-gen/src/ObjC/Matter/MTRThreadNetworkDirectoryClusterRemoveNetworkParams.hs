{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDirectoryClusterRemoveNetworkParams@.
module ObjC.Matter.MTRThreadNetworkDirectoryClusterRemoveNetworkParams
  ( MTRThreadNetworkDirectoryClusterRemoveNetworkParams
  , IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams(..)
  , extendedPanID
  , setExtendedPanID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , extendedPanIDSelector
  , serverSideProcessingTimeoutSelector
  , setExtendedPanIDSelector
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

-- | @- extendedPanID@
extendedPanID :: IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams mtrThreadNetworkDirectoryClusterRemoveNetworkParams => mtrThreadNetworkDirectoryClusterRemoveNetworkParams -> IO (Id NSData)
extendedPanID mtrThreadNetworkDirectoryClusterRemoveNetworkParams =
  sendMessage mtrThreadNetworkDirectoryClusterRemoveNetworkParams extendedPanIDSelector

-- | @- setExtendedPanID:@
setExtendedPanID :: (IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams mtrThreadNetworkDirectoryClusterRemoveNetworkParams, IsNSData value) => mtrThreadNetworkDirectoryClusterRemoveNetworkParams -> value -> IO ()
setExtendedPanID mtrThreadNetworkDirectoryClusterRemoveNetworkParams value =
  sendMessage mtrThreadNetworkDirectoryClusterRemoveNetworkParams setExtendedPanIDSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams mtrThreadNetworkDirectoryClusterRemoveNetworkParams => mtrThreadNetworkDirectoryClusterRemoveNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThreadNetworkDirectoryClusterRemoveNetworkParams =
  sendMessage mtrThreadNetworkDirectoryClusterRemoveNetworkParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams mtrThreadNetworkDirectoryClusterRemoveNetworkParams, IsNSNumber value) => mtrThreadNetworkDirectoryClusterRemoveNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThreadNetworkDirectoryClusterRemoveNetworkParams value =
  sendMessage mtrThreadNetworkDirectoryClusterRemoveNetworkParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams mtrThreadNetworkDirectoryClusterRemoveNetworkParams => mtrThreadNetworkDirectoryClusterRemoveNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThreadNetworkDirectoryClusterRemoveNetworkParams =
  sendMessage mtrThreadNetworkDirectoryClusterRemoveNetworkParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams mtrThreadNetworkDirectoryClusterRemoveNetworkParams, IsNSNumber value) => mtrThreadNetworkDirectoryClusterRemoveNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrThreadNetworkDirectoryClusterRemoveNetworkParams value =
  sendMessage mtrThreadNetworkDirectoryClusterRemoveNetworkParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extendedPanID@
extendedPanIDSelector :: Selector '[] (Id NSData)
extendedPanIDSelector = mkSelector "extendedPanID"

-- | @Selector@ for @setExtendedPanID:@
setExtendedPanIDSelector :: Selector '[Id NSData] ()
setExtendedPanIDSelector = mkSelector "setExtendedPanID:"

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

