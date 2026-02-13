{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleSetAliroReaderConfigParams@.
module ObjC.Matter.MTRDoorLockClusterAppleSetAliroReaderConfigParams
  ( MTRDoorLockClusterAppleSetAliroReaderConfigParams
  , IsMTRDoorLockClusterAppleSetAliroReaderConfigParams(..)
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
signingKey :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
signingKey mtrDoorLockClusterAppleSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams signingKeySelector

-- | @- setSigningKey:@
setSigningKey :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setSigningKey mtrDoorLockClusterAppleSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams setSigningKeySelector (toNSData value)

-- | @- verificationKey@
verificationKey :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
verificationKey mtrDoorLockClusterAppleSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams verificationKeySelector

-- | @- setVerificationKey:@
setVerificationKey :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setVerificationKey mtrDoorLockClusterAppleSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams setVerificationKeySelector (toNSData value)

-- | @- groupIdentifier@
groupIdentifier :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
groupIdentifier mtrDoorLockClusterAppleSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams groupIdentifierSelector

-- | @- setGroupIdentifier:@
setGroupIdentifier :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setGroupIdentifier mtrDoorLockClusterAppleSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams setGroupIdentifierSelector (toNSData value)

-- | @- groupResolvingKey@
groupResolvingKey :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSData)
groupResolvingKey mtrDoorLockClusterAppleSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams groupResolvingKeySelector

-- | @- setGroupResolvingKey:@
setGroupResolvingKey :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setGroupResolvingKey mtrDoorLockClusterAppleSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams setGroupResolvingKeySelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterAppleSetAliroReaderConfigParams =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterAppleSetAliroReaderConfigParams mtrDoorLockClusterAppleSetAliroReaderConfigParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroReaderConfigParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterAppleSetAliroReaderConfigParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroReaderConfigParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

