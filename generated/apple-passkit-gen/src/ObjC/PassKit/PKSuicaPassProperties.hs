{-# LANGUAGE DataKinds #-}
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
  , balanceAllowedForCommuteSelector
  , blacklistedSelector
  , greenCarTicketUsedSelector
  , inShinkansenStationSelector
  , inStationSelector
  , lowBalanceGateNotificationEnabledSelector
  , passPropertiesForPassSelector
  , transitBalanceCurrencyCodeSelector
  , transitBalanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' passPropertiesForPassSelector (toPKPass pass)

-- | @- transitBalance@
transitBalance :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO (Id NSDecimalNumber)
transitBalance pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties transitBalanceSelector

-- | @- transitBalanceCurrencyCode@
transitBalanceCurrencyCode :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO (Id NSString)
transitBalanceCurrencyCode pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties transitBalanceCurrencyCodeSelector

-- | @- inStation@
inStation :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
inStation pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties inStationSelector

-- | Note: isInShinkansenStation is not a subset of isInStation.
--
-- ObjC selector: @- inShinkansenStation@
inShinkansenStation :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
inShinkansenStation pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties inShinkansenStationSelector

-- | @- balanceAllowedForCommute@
balanceAllowedForCommute :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
balanceAllowedForCommute pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties balanceAllowedForCommuteSelector

-- | @- lowBalanceGateNotificationEnabled@
lowBalanceGateNotificationEnabled :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
lowBalanceGateNotificationEnabled pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties lowBalanceGateNotificationEnabledSelector

-- | @- greenCarTicketUsed@
greenCarTicketUsed :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
greenCarTicketUsed pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties greenCarTicketUsedSelector

-- | @- blacklisted@
blacklisted :: IsPKSuicaPassProperties pkSuicaPassProperties => pkSuicaPassProperties -> IO Bool
blacklisted pkSuicaPassProperties =
  sendMessage pkSuicaPassProperties blacklistedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @passPropertiesForPass:@
passPropertiesForPassSelector :: Selector '[Id PKPass] (Id PKSuicaPassProperties)
passPropertiesForPassSelector = mkSelector "passPropertiesForPass:"

-- | @Selector@ for @transitBalance@
transitBalanceSelector :: Selector '[] (Id NSDecimalNumber)
transitBalanceSelector = mkSelector "transitBalance"

-- | @Selector@ for @transitBalanceCurrencyCode@
transitBalanceCurrencyCodeSelector :: Selector '[] (Id NSString)
transitBalanceCurrencyCodeSelector = mkSelector "transitBalanceCurrencyCode"

-- | @Selector@ for @inStation@
inStationSelector :: Selector '[] Bool
inStationSelector = mkSelector "inStation"

-- | @Selector@ for @inShinkansenStation@
inShinkansenStationSelector :: Selector '[] Bool
inShinkansenStationSelector = mkSelector "inShinkansenStation"

-- | @Selector@ for @balanceAllowedForCommute@
balanceAllowedForCommuteSelector :: Selector '[] Bool
balanceAllowedForCommuteSelector = mkSelector "balanceAllowedForCommute"

-- | @Selector@ for @lowBalanceGateNotificationEnabled@
lowBalanceGateNotificationEnabledSelector :: Selector '[] Bool
lowBalanceGateNotificationEnabledSelector = mkSelector "lowBalanceGateNotificationEnabled"

-- | @Selector@ for @greenCarTicketUsed@
greenCarTicketUsedSelector :: Selector '[] Bool
greenCarTicketUsedSelector = mkSelector "greenCarTicketUsed"

-- | @Selector@ for @blacklisted@
blacklistedSelector :: Selector '[] Bool
blacklistedSelector = mkSelector "blacklisted"

