{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterSignVIDVerificationRequestParams@.
module ObjC.Matter.MTROperationalCredentialsClusterSignVIDVerificationRequestParams
  ( MTROperationalCredentialsClusterSignVIDVerificationRequestParams
  , IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams(..)
  , fabricIndex
  , setFabricIndex
  , clientChallenge
  , setClientChallenge
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , clientChallengeSelector
  , fabricIndexSelector
  , serverSideProcessingTimeoutSelector
  , setClientChallengeSelector
  , setFabricIndexSelector
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

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterSignVIDVerificationRequestParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterSignVIDVerificationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams setFabricIndexSelector (toNSNumber value)

-- | @- clientChallenge@
clientChallenge :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSData)
clientChallenge mtrOperationalCredentialsClusterSignVIDVerificationRequestParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams clientChallengeSelector

-- | @- setClientChallenge:@
setClientChallenge :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSData value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setClientChallenge mtrOperationalCredentialsClusterSignVIDVerificationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams setClientChallengeSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterSignVIDVerificationRequestParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterSignVIDVerificationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterSignVIDVerificationRequestParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterSignVIDVerificationRequestParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @clientChallenge@
clientChallengeSelector :: Selector '[] (Id NSData)
clientChallengeSelector = mkSelector "clientChallenge"

-- | @Selector@ for @setClientChallenge:@
setClientChallengeSelector :: Selector '[Id NSData] ()
setClientChallengeSelector = mkSelector "setClientChallenge:"

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

