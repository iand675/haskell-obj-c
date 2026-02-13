{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterCSRRequestParams@.
module ObjC.Matter.MTROperationalCredentialsClusterCSRRequestParams
  ( MTROperationalCredentialsClusterCSRRequestParams
  , IsMTROperationalCredentialsClusterCSRRequestParams(..)
  , csrNonce
  , setCsrNonce
  , isForUpdateNOC
  , setIsForUpdateNOC
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , csrNonceSelector
  , isForUpdateNOCSelector
  , serverSideProcessingTimeoutSelector
  , setCsrNonceSelector
  , setIsForUpdateNOCSelector
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

-- | @- csrNonce@
csrNonce :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSData)
csrNonce mtrOperationalCredentialsClusterCSRRequestParams =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams csrNonceSelector

-- | @- setCsrNonce:@
setCsrNonce :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSData value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setCsrNonce mtrOperationalCredentialsClusterCSRRequestParams value =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams setCsrNonceSelector (toNSData value)

-- | @- isForUpdateNOC@
isForUpdateNOC :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSNumber)
isForUpdateNOC mtrOperationalCredentialsClusterCSRRequestParams =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams isForUpdateNOCSelector

-- | @- setIsForUpdateNOC:@
setIsForUpdateNOC :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setIsForUpdateNOC mtrOperationalCredentialsClusterCSRRequestParams value =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams setIsForUpdateNOCSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterCSRRequestParams =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterCSRRequestParams value =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams => mtrOperationalCredentialsClusterCSRRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterCSRRequestParams =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterCSRRequestParams mtrOperationalCredentialsClusterCSRRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterCSRRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterCSRRequestParams value =
  sendMessage mtrOperationalCredentialsClusterCSRRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @csrNonce@
csrNonceSelector :: Selector '[] (Id NSData)
csrNonceSelector = mkSelector "csrNonce"

-- | @Selector@ for @setCsrNonce:@
setCsrNonceSelector :: Selector '[Id NSData] ()
setCsrNonceSelector = mkSelector "setCsrNonce:"

-- | @Selector@ for @isForUpdateNOC@
isForUpdateNOCSelector :: Selector '[] (Id NSNumber)
isForUpdateNOCSelector = mkSelector "isForUpdateNOC"

-- | @Selector@ for @setIsForUpdateNOC:@
setIsForUpdateNOCSelector :: Selector '[Id NSNumber] ()
setIsForUpdateNOCSelector = mkSelector "setIsForUpdateNOC:"

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

