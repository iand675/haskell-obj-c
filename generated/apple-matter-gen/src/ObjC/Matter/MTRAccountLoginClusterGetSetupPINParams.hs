{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccountLoginClusterGetSetupPINParams@.
module ObjC.Matter.MTRAccountLoginClusterGetSetupPINParams
  ( MTRAccountLoginClusterGetSetupPINParams
  , IsMTRAccountLoginClusterGetSetupPINParams(..)
  , tempAccountIdentifier
  , setTempAccountIdentifier
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTempAccountIdentifierSelector
  , setTimedInvokeTimeoutMsSelector
  , tempAccountIdentifierSelector
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

-- | @- tempAccountIdentifier@
tempAccountIdentifier :: IsMTRAccountLoginClusterGetSetupPINParams mtrAccountLoginClusterGetSetupPINParams => mtrAccountLoginClusterGetSetupPINParams -> IO (Id NSString)
tempAccountIdentifier mtrAccountLoginClusterGetSetupPINParams =
  sendMessage mtrAccountLoginClusterGetSetupPINParams tempAccountIdentifierSelector

-- | @- setTempAccountIdentifier:@
setTempAccountIdentifier :: (IsMTRAccountLoginClusterGetSetupPINParams mtrAccountLoginClusterGetSetupPINParams, IsNSString value) => mtrAccountLoginClusterGetSetupPINParams -> value -> IO ()
setTempAccountIdentifier mtrAccountLoginClusterGetSetupPINParams value =
  sendMessage mtrAccountLoginClusterGetSetupPINParams setTempAccountIdentifierSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRAccountLoginClusterGetSetupPINParams mtrAccountLoginClusterGetSetupPINParams => mtrAccountLoginClusterGetSetupPINParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrAccountLoginClusterGetSetupPINParams =
  sendMessage mtrAccountLoginClusterGetSetupPINParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRAccountLoginClusterGetSetupPINParams mtrAccountLoginClusterGetSetupPINParams, IsNSNumber value) => mtrAccountLoginClusterGetSetupPINParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrAccountLoginClusterGetSetupPINParams value =
  sendMessage mtrAccountLoginClusterGetSetupPINParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRAccountLoginClusterGetSetupPINParams mtrAccountLoginClusterGetSetupPINParams => mtrAccountLoginClusterGetSetupPINParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrAccountLoginClusterGetSetupPINParams =
  sendMessage mtrAccountLoginClusterGetSetupPINParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRAccountLoginClusterGetSetupPINParams mtrAccountLoginClusterGetSetupPINParams, IsNSNumber value) => mtrAccountLoginClusterGetSetupPINParams -> value -> IO ()
setServerSideProcessingTimeout mtrAccountLoginClusterGetSetupPINParams value =
  sendMessage mtrAccountLoginClusterGetSetupPINParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tempAccountIdentifier@
tempAccountIdentifierSelector :: Selector '[] (Id NSString)
tempAccountIdentifierSelector = mkSelector "tempAccountIdentifier"

-- | @Selector@ for @setTempAccountIdentifier:@
setTempAccountIdentifierSelector :: Selector '[Id NSString] ()
setTempAccountIdentifierSelector = mkSelector "setTempAccountIdentifier:"

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

