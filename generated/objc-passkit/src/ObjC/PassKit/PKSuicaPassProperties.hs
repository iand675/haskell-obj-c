{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKSuicaPassProperties@.
module ObjC.PassKit.PKSuicaPassProperties
  ( PKSuicaPassProperties
  , IsPKSuicaPassProperties(..)
  , passPropertiesForPass
  , transitBalance
  , transitBalanceCurrencyCode
  , inStation
  , inShinkansenStation
  , balanceAllowedForCommute
  , lowBalanceGateNotificationEnabled
  , greenCarTicketUsed
  , blacklisted
  , passPropertiesForPassSelector
  , transitBalanceSelector
  , transitBalanceCurrencyCodeSelector
  , inStationSelector
  , inShinkansenStationSelector
  , balanceAllowedForCommuteSelector
  , lowBalanceGateNotificationEnabledSelector
  , greenCarTicketUsedSelector
  , blacklistedSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Properties for a given pass, or nil if the pass doesn’t support the set of properties being requested
--
-- ObjC selector: @+ passPropertiesForPass:@
passPropertiesForPass :: IsPKPass pass => pass -> IO (Id PKSuicaPassProperties)
passPropertiesForPass pass =
  do
    cls' <- getRequiredClass "PKSuicaPassProperties"
    withObjCPtr pass $ \raw_pass ->
      sendClassMsg cls' (mkSelector "passPropertiesForPass:") (retPtr retVoid) [argPtr (castPtr raw_pass :: Ptr ())] >>= retainedObject . castPtr

-- | @- transitBalance@
transitBalance :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO (Id NSDecimalNumber)
transitBalance pkSuicaPassProperties  =
  sendMsg pkSuicaPassProperties (mkSelector "transitBalance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transitBalanceCurrencyCode@
transitBalanceCurrencyCode :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO (Id NSString)
transitBalanceCurrencyCode pkSuicaPassProperties  =
  sendMsg pkSuicaPassProperties (mkSelector "transitBalanceCurrencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- inStation@
inStation :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
inStation pkSuicaPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkSuicaPassProperties (mkSelector "inStation") retCULong []

-- | Note: isInShinkansenStation is not a subset of isInStation.
--
-- ObjC selector: @- inShinkansenStation@
inShinkansenStation :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
inShinkansenStation pkSuicaPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkSuicaPassProperties (mkSelector "inShinkansenStation") retCULong []

-- | @- balanceAllowedForCommute@
balanceAllowedForCommute :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
balanceAllowedForCommute pkSuicaPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkSuicaPassProperties (mkSelector "balanceAllowedForCommute") retCULong []

-- | @- lowBalanceGateNotificationEnabled@
lowBalanceGateNotificationEnabled :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
lowBalanceGateNotificationEnabled pkSuicaPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkSuicaPassProperties (mkSelector "lowBalanceGateNotificationEnabled") retCULong []

-- | @- greenCarTicketUsed@
greenCarTicketUsed :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
greenCarTicketUsed pkSuicaPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkSuicaPassProperties (mkSelector "greenCarTicketUsed") retCULong []

-- | @- blacklisted@
blacklisted :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
blacklisted pkSuicaPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkSuicaPassProperties (mkSelector "blacklisted") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @passPropertiesForPass:@
passPropertiesForPassSelector :: Selector
passPropertiesForPassSelector = mkSelector "passPropertiesForPass:"

-- | @Selector@ for @transitBalance@
transitBalanceSelector :: Selector
transitBalanceSelector = mkSelector "transitBalance"

-- | @Selector@ for @transitBalanceCurrencyCode@
transitBalanceCurrencyCodeSelector :: Selector
transitBalanceCurrencyCodeSelector = mkSelector "transitBalanceCurrencyCode"

-- | @Selector@ for @inStation@
inStationSelector :: Selector
inStationSelector = mkSelector "inStation"

-- | @Selector@ for @inShinkansenStation@
inShinkansenStationSelector :: Selector
inShinkansenStationSelector = mkSelector "inShinkansenStation"

-- | @Selector@ for @balanceAllowedForCommute@
balanceAllowedForCommuteSelector :: Selector
balanceAllowedForCommuteSelector = mkSelector "balanceAllowedForCommute"

-- | @Selector@ for @lowBalanceGateNotificationEnabled@
lowBalanceGateNotificationEnabledSelector :: Selector
lowBalanceGateNotificationEnabledSelector = mkSelector "lowBalanceGateNotificationEnabled"

-- | @Selector@ for @greenCarTicketUsed@
greenCarTicketUsedSelector :: Selector
greenCarTicketUsedSelector = mkSelector "greenCarTicketUsed"

-- | @Selector@ for @blacklisted@
blacklistedSelector :: Selector
blacklistedSelector = mkSelector "blacklisted"

