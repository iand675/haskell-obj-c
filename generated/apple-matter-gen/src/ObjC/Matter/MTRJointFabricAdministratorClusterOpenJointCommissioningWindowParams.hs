{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams
  ( MTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams
  , IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams(..)
  , commissioningTimeout
  , setCommissioningTimeout
  , pakePasscodeVerifier
  , setPakePasscodeVerifier
  , discriminator
  , setDiscriminator
  , iterations
  , setIterations
  , salt
  , setSalt
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , commissioningTimeoutSelector
  , discriminatorSelector
  , iterationsSelector
  , pakePasscodeVerifierSelector
  , saltSelector
  , serverSideProcessingTimeoutSelector
  , setCommissioningTimeoutSelector
  , setDiscriminatorSelector
  , setIterationsSelector
  , setPakePasscodeVerifierSelector
  , setSaltSelector
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

-- | @- commissioningTimeout@
commissioningTimeout :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSNumber)
commissioningTimeout mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams commissioningTimeoutSelector

-- | @- setCommissioningTimeout:@
setCommissioningTimeout :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSNumber value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setCommissioningTimeout mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setCommissioningTimeoutSelector (toNSNumber value)

-- | @- pakePasscodeVerifier@
pakePasscodeVerifier :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSData)
pakePasscodeVerifier mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams pakePasscodeVerifierSelector

-- | @- setPakePasscodeVerifier:@
setPakePasscodeVerifier :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSData value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setPakePasscodeVerifier mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setPakePasscodeVerifierSelector (toNSData value)

-- | @- discriminator@
discriminator :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSNumber)
discriminator mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams discriminatorSelector

-- | @- setDiscriminator:@
setDiscriminator :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSNumber value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setDiscriminator mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setDiscriminatorSelector (toNSNumber value)

-- | @- iterations@
iterations :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSNumber)
iterations mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams iterationsSelector

-- | @- setIterations:@
setIterations :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSNumber value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setIterations mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setIterationsSelector (toNSNumber value)

-- | @- salt@
salt :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSData)
salt mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams saltSelector

-- | @- setSalt:@
setSalt :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSData value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setSalt mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setSaltSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSNumber value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams, IsNSNumber value) => mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams value =
  sendMessage mtrJointFabricAdministratorClusterOpenJointCommissioningWindowParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commissioningTimeout@
commissioningTimeoutSelector :: Selector '[] (Id NSNumber)
commissioningTimeoutSelector = mkSelector "commissioningTimeout"

-- | @Selector@ for @setCommissioningTimeout:@
setCommissioningTimeoutSelector :: Selector '[Id NSNumber] ()
setCommissioningTimeoutSelector = mkSelector "setCommissioningTimeout:"

-- | @Selector@ for @pakePasscodeVerifier@
pakePasscodeVerifierSelector :: Selector '[] (Id NSData)
pakePasscodeVerifierSelector = mkSelector "pakePasscodeVerifier"

-- | @Selector@ for @setPakePasscodeVerifier:@
setPakePasscodeVerifierSelector :: Selector '[Id NSData] ()
setPakePasscodeVerifierSelector = mkSelector "setPakePasscodeVerifier:"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector '[] (Id NSNumber)
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @setDiscriminator:@
setDiscriminatorSelector :: Selector '[Id NSNumber] ()
setDiscriminatorSelector = mkSelector "setDiscriminator:"

-- | @Selector@ for @iterations@
iterationsSelector :: Selector '[] (Id NSNumber)
iterationsSelector = mkSelector "iterations"

-- | @Selector@ for @setIterations:@
setIterationsSelector :: Selector '[Id NSNumber] ()
setIterationsSelector = mkSelector "setIterations:"

-- | @Selector@ for @salt@
saltSelector :: Selector '[] (Id NSData)
saltSelector = mkSelector "salt"

-- | @Selector@ for @setSalt:@
setSaltSelector :: Selector '[Id NSData] ()
setSaltSelector = mkSelector "setSalt:"

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

