{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , cancellationFeeSelector
  , setCancellationFeeSelector
  , cancellationFeeThresholdSelector
  , setCancellationFeeThresholdSelector

  -- * Enum types
  , INCancelRideIntentResponseCode(INCancelRideIntentResponseCode)
  , pattern INCancelRideIntentResponseCodeUnspecified
  , pattern INCancelRideIntentResponseCodeReady
  , pattern INCancelRideIntentResponseCodeSuccess
  , pattern INCancelRideIntentResponseCodeFailure

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO (Id INCancelRideIntentResponse)
init_ inCancelRideIntentResponse  =
  sendMsg inCancelRideIntentResponse (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCancelRideIntentResponse inCancelRideIntentResponse, IsNSUserActivity userActivity) => inCancelRideIntentResponse -> INCancelRideIntentResponseCode -> userActivity -> IO (Id INCancelRideIntentResponse)
initWithCode_userActivity inCancelRideIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inCancelRideIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO INCancelRideIntentResponseCode
code inCancelRideIntentResponse  =
  fmap (coerce :: CLong -> INCancelRideIntentResponseCode) $ sendMsg inCancelRideIntentResponse (mkSelector "code") retCLong []

-- | @- cancellationFee@
cancellationFee :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO (Id INCurrencyAmount)
cancellationFee inCancelRideIntentResponse  =
  sendMsg inCancelRideIntentResponse (mkSelector "cancellationFee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCancellationFee:@
setCancellationFee :: (IsINCancelRideIntentResponse inCancelRideIntentResponse, IsINCurrencyAmount value) => inCancelRideIntentResponse -> value -> IO ()
setCancellationFee inCancelRideIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inCancelRideIntentResponse (mkSelector "setCancellationFee:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cancellationFeeThreshold@
cancellationFeeThreshold :: IsINCancelRideIntentResponse inCancelRideIntentResponse => inCancelRideIntentResponse -> IO (Id NSDateComponents)
cancellationFeeThreshold inCancelRideIntentResponse  =
  sendMsg inCancelRideIntentResponse (mkSelector "cancellationFeeThreshold") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCancellationFeeThreshold:@
setCancellationFeeThreshold :: (IsINCancelRideIntentResponse inCancelRideIntentResponse, IsNSDateComponents value) => inCancelRideIntentResponse -> value -> IO ()
setCancellationFeeThreshold inCancelRideIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inCancelRideIntentResponse (mkSelector "setCancellationFeeThreshold:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @cancellationFee@
cancellationFeeSelector :: Selector
cancellationFeeSelector = mkSelector "cancellationFee"

-- | @Selector@ for @setCancellationFee:@
setCancellationFeeSelector :: Selector
setCancellationFeeSelector = mkSelector "setCancellationFee:"

-- | @Selector@ for @cancellationFeeThreshold@
cancellationFeeThresholdSelector :: Selector
cancellationFeeThresholdSelector = mkSelector "cancellationFeeThreshold"

-- | @Selector@ for @setCancellationFeeThreshold:@
setCancellationFeeThresholdSelector :: Selector
setCancellationFeeThresholdSelector = mkSelector "setCancellationFeeThreshold:"

