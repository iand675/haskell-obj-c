{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterUpdateNOCParams@.
module ObjC.Matter.MTROperationalCredentialsClusterUpdateNOCParams
  ( MTROperationalCredentialsClusterUpdateNOCParams
  , IsMTROperationalCredentialsClusterUpdateNOCParams(..)
  , nocValue
  , setNocValue
  , icacValue
  , setIcacValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , icacValueSelector
  , nocValueSelector
  , serverSideProcessingTimeoutSelector
  , setIcacValueSelector
  , setNocValueSelector
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

-- | @- nocValue@
nocValue :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSData)
nocValue mtrOperationalCredentialsClusterUpdateNOCParams =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams nocValueSelector

-- | @- setNocValue:@
setNocValue :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSData value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setNocValue mtrOperationalCredentialsClusterUpdateNOCParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams setNocValueSelector (toNSData value)

-- | @- icacValue@
icacValue :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSData)
icacValue mtrOperationalCredentialsClusterUpdateNOCParams =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams icacValueSelector

-- | @- setIcacValue:@
setIcacValue :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSData value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setIcacValue mtrOperationalCredentialsClusterUpdateNOCParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams setIcacValueSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterUpdateNOCParams =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterUpdateNOCParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterUpdateNOCParams =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterUpdateNOCParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateNOCParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nocValue@
nocValueSelector :: Selector '[] (Id NSData)
nocValueSelector = mkSelector "nocValue"

-- | @Selector@ for @setNocValue:@
setNocValueSelector :: Selector '[Id NSData] ()
setNocValueSelector = mkSelector "setNocValue:"

-- | @Selector@ for @icacValue@
icacValueSelector :: Selector '[] (Id NSData)
icacValueSelector = mkSelector "icacValue"

-- | @Selector@ for @setIcacValue:@
setIcacValueSelector :: Selector '[Id NSData] ()
setIcacValueSelector = mkSelector "setIcacValue:"

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

