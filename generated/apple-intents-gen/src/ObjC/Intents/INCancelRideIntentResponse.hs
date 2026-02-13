{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCancelRideIntentResponse@.
module ObjC.Intents.INCancelRideIntentResponse
  ( INCancelRideIntentResponse
  , IsINCancelRideIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , cancellationFee
  , setCancellationFee
  , cancellationFeeThreshold
  , setCancellationFeeThreshold
  , cancellationFeeSelector
  , cancellationFeeThresholdSelector
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setCancellationFeeSelector
  , setCancellationFeeThresholdSelector

  -- * Enum types
  , INCancelRideIntentResponseCode(INCancelRideIntentResponseCode)
  , pattern INCancelRideIntentResponseCodeUnspecified
  , pattern INCancelRideIntentResponseCodeReady
  , pattern INCancelRideIntentResponseCodeSuccess
  , pattern INCancelRideIntentResponseCodeFailure

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO (Id INCancelRideIntentResponse)
init_ inCancelRideIntentResponse =
  sendOwnedMessage inCancelRideIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCancelRideIntentResponse inCancelRideIntentResponse, IsNSUserActivity userActivity) => inCancelRideIntentResponse -> INCancelRideIntentResponseCode -> userActivity -> IO (Id INCancelRideIntentResponse)
initWithCode_userActivity inCancelRideIntentResponse code userActivity =
  sendOwnedMessage inCancelRideIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO INCancelRideIntentResponseCode
code inCancelRideIntentResponse =
  sendMessage inCancelRideIntentResponse codeSelector

-- | @- cancellationFee@
cancellationFee :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO (Id INCurrencyAmount)
cancellationFee inCancelRideIntentResponse =
  sendMessage inCancelRideIntentResponse cancellationFeeSelector

-- | @- setCancellationFee:@
setCancellationFee :: (IsINCancelRideIntentResponse inCancelRideIntentResponse, IsINCurrencyAmount value) => inCancelRideIntentResponse -> value -> IO ()
setCancellationFee inCancelRideIntentResponse value =
  sendMessage inCancelRideIntentResponse setCancellationFeeSelector (toINCurrencyAmount value)

-- | @- cancellationFeeThreshold@
cancellationFeeThreshold :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO (Id NSDateComponents)
cancellationFeeThreshold inCancelRideIntentResponse =
  sendMessage inCancelRideIntentResponse cancellationFeeThresholdSelector

-- | @- setCancellationFeeThreshold:@
setCancellationFeeThreshold :: (IsINCancelRideIntentResponse inCancelRideIntentResponse, IsNSDateComponents value) => inCancelRideIntentResponse -> value -> IO ()
setCancellationFeeThreshold inCancelRideIntentResponse value =
  sendMessage inCancelRideIntentResponse setCancellationFeeThresholdSelector (toNSDateComponents value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCancelRideIntentResponse)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INCancelRideIntentResponseCode, Id NSUserActivity] (Id INCancelRideIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INCancelRideIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @cancellationFee@
cancellationFeeSelector :: Selector '[] (Id INCurrencyAmount)
cancellationFeeSelector = mkSelector "cancellationFee"

-- | @Selector@ for @setCancellationFee:@
setCancellationFeeSelector :: Selector '[Id INCurrencyAmount] ()
setCancellationFeeSelector = mkSelector "setCancellationFee:"

-- | @Selector@ for @cancellationFeeThreshold@
cancellationFeeThresholdSelector :: Selector '[] (Id NSDateComponents)
cancellationFeeThresholdSelector = mkSelector "cancellationFeeThreshold"

-- | @Selector@ for @setCancellationFeeThreshold:@
setCancellationFeeThresholdSelector :: Selector '[Id NSDateComponents] ()
setCancellationFeeThresholdSelector = mkSelector "setCancellationFeeThreshold:"

