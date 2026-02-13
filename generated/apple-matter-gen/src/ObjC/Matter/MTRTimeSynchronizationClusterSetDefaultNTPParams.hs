{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetDefaultNTPParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetDefaultNTPParams
  ( MTRTimeSynchronizationClusterSetDefaultNTPParams
  , IsMTRTimeSynchronizationClusterSetDefaultNTPParams(..)
  , defaultNTP
  , setDefaultNTP
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , defaultNTPSelector
  , serverSideProcessingTimeoutSelector
  , setDefaultNTPSelector
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

-- | @- defaultNTP@
defaultNTP :: IsMTRTimeSynchronizationClusterSetDefaultNTPParams mtrTimeSynchronizationClusterSetDefaultNTPParams => mtrTimeSynchronizationClusterSetDefaultNTPParams -> IO (Id NSString)
defaultNTP mtrTimeSynchronizationClusterSetDefaultNTPParams =
  sendMessage mtrTimeSynchronizationClusterSetDefaultNTPParams defaultNTPSelector

-- | @- setDefaultNTP:@
setDefaultNTP :: (IsMTRTimeSynchronizationClusterSetDefaultNTPParams mtrTimeSynchronizationClusterSetDefaultNTPParams, IsNSString value) => mtrTimeSynchronizationClusterSetDefaultNTPParams -> value -> IO ()
setDefaultNTP mtrTimeSynchronizationClusterSetDefaultNTPParams value =
  sendMessage mtrTimeSynchronizationClusterSetDefaultNTPParams setDefaultNTPSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTimeSynchronizationClusterSetDefaultNTPParams mtrTimeSynchronizationClusterSetDefaultNTPParams => mtrTimeSynchronizationClusterSetDefaultNTPParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTimeSynchronizationClusterSetDefaultNTPParams =
  sendMessage mtrTimeSynchronizationClusterSetDefaultNTPParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTimeSynchronizationClusterSetDefaultNTPParams mtrTimeSynchronizationClusterSetDefaultNTPParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetDefaultNTPParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTimeSynchronizationClusterSetDefaultNTPParams value =
  sendMessage mtrTimeSynchronizationClusterSetDefaultNTPParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTimeSynchronizationClusterSetDefaultNTPParams mtrTimeSynchronizationClusterSetDefaultNTPParams => mtrTimeSynchronizationClusterSetDefaultNTPParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTimeSynchronizationClusterSetDefaultNTPParams =
  sendMessage mtrTimeSynchronizationClusterSetDefaultNTPParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTimeSynchronizationClusterSetDefaultNTPParams mtrTimeSynchronizationClusterSetDefaultNTPParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetDefaultNTPParams -> value -> IO ()
setServerSideProcessingTimeout mtrTimeSynchronizationClusterSetDefaultNTPParams value =
  sendMessage mtrTimeSynchronizationClusterSetDefaultNTPParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultNTP@
defaultNTPSelector :: Selector '[] (Id NSString)
defaultNTPSelector = mkSelector "defaultNTP"

-- | @Selector@ for @setDefaultNTP:@
setDefaultNTPSelector :: Selector '[Id NSString] ()
setDefaultNTPSelector = mkSelector "setDefaultNTP:"

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

