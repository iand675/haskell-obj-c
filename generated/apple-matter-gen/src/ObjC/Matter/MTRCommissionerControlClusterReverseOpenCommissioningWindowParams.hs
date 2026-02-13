{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterReverseOpenCommissioningWindowParams@.
module ObjC.Matter.MTRCommissionerControlClusterReverseOpenCommissioningWindowParams
  ( MTRCommissionerControlClusterReverseOpenCommissioningWindowParams
  , IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams(..)
  , initWithResponseValue_error
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
  , commissioningTimeoutSelector
  , discriminatorSelector
  , initWithResponseValue_errorSelector
  , iterationsSelector
  , pakePasscodeVerifierSelector
  , saltSelector
  , setCommissioningTimeoutSelector
  , setDiscriminatorSelector
  , setIterationsSelector
  , setPakePasscodeVerifierSelector
  , setSaltSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCommissionerControlClusterReverseOpenCommissioningWindowParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> responseValue -> error_ -> IO (Id MTRCommissionerControlClusterReverseOpenCommissioningWindowParams)
initWithResponseValue_error mtrCommissionerControlClusterReverseOpenCommissioningWindowParams responseValue error_ =
  sendOwnedMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- commissioningTimeout@
commissioningTimeout :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSNumber)
commissioningTimeout mtrCommissionerControlClusterReverseOpenCommissioningWindowParams =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams commissioningTimeoutSelector

-- | @- setCommissioningTimeout:@
setCommissioningTimeout :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSNumber value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setCommissioningTimeout mtrCommissionerControlClusterReverseOpenCommissioningWindowParams value =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams setCommissioningTimeoutSelector (toNSNumber value)

-- | @- pakePasscodeVerifier@
pakePasscodeVerifier :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSData)
pakePasscodeVerifier mtrCommissionerControlClusterReverseOpenCommissioningWindowParams =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams pakePasscodeVerifierSelector

-- | @- setPakePasscodeVerifier:@
setPakePasscodeVerifier :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSData value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setPakePasscodeVerifier mtrCommissionerControlClusterReverseOpenCommissioningWindowParams value =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams setPakePasscodeVerifierSelector (toNSData value)

-- | @- discriminator@
discriminator :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSNumber)
discriminator mtrCommissionerControlClusterReverseOpenCommissioningWindowParams =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams discriminatorSelector

-- | @- setDiscriminator:@
setDiscriminator :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSNumber value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setDiscriminator mtrCommissionerControlClusterReverseOpenCommissioningWindowParams value =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams setDiscriminatorSelector (toNSNumber value)

-- | @- iterations@
iterations :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSNumber)
iterations mtrCommissionerControlClusterReverseOpenCommissioningWindowParams =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams iterationsSelector

-- | @- setIterations:@
setIterations :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSNumber value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setIterations mtrCommissionerControlClusterReverseOpenCommissioningWindowParams value =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams setIterationsSelector (toNSNumber value)

-- | @- salt@
salt :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSData)
salt mtrCommissionerControlClusterReverseOpenCommissioningWindowParams =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams saltSelector

-- | @- setSalt:@
setSalt :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSData value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setSalt mtrCommissionerControlClusterReverseOpenCommissioningWindowParams value =
  sendMessage mtrCommissionerControlClusterReverseOpenCommissioningWindowParams setSaltSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCommissionerControlClusterReverseOpenCommissioningWindowParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

