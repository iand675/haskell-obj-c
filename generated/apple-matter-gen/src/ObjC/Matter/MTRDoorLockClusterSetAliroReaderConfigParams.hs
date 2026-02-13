{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetAliroReaderConfigParams@.
module ObjC.Matter.MTRDoorLockClusterSetAliroReaderConfigParams
  ( MTRDoorLockClusterSetAliroReaderConfigParams
  , IsMTRDoorLockClusterSetAliroReaderConfigParams(..)
  , signingKey
  , setSigningKey
  , verificationKey
  , setVerificationKey
  , groupIdentifier
  , setGroupIdentifier
  , groupResolvingKey
  , setGroupResolvingKey
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIdentifierSelector
  , groupResolvingKeySelector
  , serverSideProcessingTimeoutSelector
  , setGroupIdentifierSelector
  , setGroupResolvingKeySelector
  , setServerSideProcessingTimeoutSelector
  , setSigningKeySelector
  , setTimedInvokeTimeoutMsSelector
  , setVerificationKeySelector
  , signingKeySelector
  , timedInvokeTimeoutMsSelector
  , verificationKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- signingKey@
signingKey :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
signingKey mtrDoorLockClusterSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams signingKeySelector

-- | @- setSigningKey:@
setSigningKey :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setSigningKey mtrDoorLockClusterSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams setSigningKeySelector (toNSData value)

-- | @- verificationKey@
verificationKey :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
verificationKey mtrDoorLockClusterSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams verificationKeySelector

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setVerificationKey mtrDoorLockClusterSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams setVerificationKeySelector (toNSData value)

-- | @- groupIdentifier@
groupIdentifier :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
groupIdentifier mtrDoorLockClusterSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams groupIdentifierSelector

-- | @- setGroupIdentifier:@
setGroupIdentifier :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setGroupIdentifier mtrDoorLockClusterSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams setGroupIdentifierSelector (toNSData value)

-- | @- groupResolvingKey@
groupResolvingKey :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSData)
groupResolvingKey mtrDoorLockClusterSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams groupResolvingKeySelector

-- | @- setGroupResolvingKey:@
setGroupResolvingKey :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setGroupResolvingKey mtrDoorLockClusterSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams setGroupResolvingKeySelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams => mtrDoorLockClusterSetAliroReaderConfigParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetAliroReaderConfigParams mtrDoorLockClusterSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterSetAliroReaderConfigParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterSetAliroReaderConfigParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signingKey@
signingKeySelector :: Selector '[] (Id NSData)
signingKeySelector = mkSelector "signingKey"

-- | @Selector@ for @setSigningKey:@
setSigningKeySelector :: Selector '[Id NSData] ()
setSigningKeySelector = mkSelector "setSigningKey:"

-- | @Selector@ for @verificationKey@
verificationKeySelector :: Selector '[] (Id NSData)
verificationKeySelector = mkSelector "verificationKey"

-- | @Selector@ for @setVerificationKey:@
setVerificationKeySelector :: Selector '[Id NSData] ()
setVerificationKeySelector = mkSelector "setVerificationKey:"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector '[] (Id NSData)
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @setGroupIdentifier:@
setGroupIdentifierSelector :: Selector '[Id NSData] ()
setGroupIdentifierSelector = mkSelector "setGroupIdentifier:"

-- | @Selector@ for @groupResolvingKey@
groupResolvingKeySelector :: Selector '[] (Id NSData)
groupResolvingKeySelector = mkSelector "groupResolvingKey"

-- | @Selector@ for @setGroupResolvingKey:@
setGroupResolvingKeySelector :: Selector '[Id NSData] ()
setGroupResolvingKeySelector = mkSelector "setGroupResolvingKey:"

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

