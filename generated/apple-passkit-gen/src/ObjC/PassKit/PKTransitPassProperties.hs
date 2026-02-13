{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKTransitPassProperties@.
module ObjC.PassKit.PKTransitPassProperties
  ( PKTransitPassProperties
  , IsPKTransitPassProperties(..)
  , transitBalance
  , transitBalanceCurrencyCode
  , blacklisted
  , expirationDate
  , blocked
  , inStation
  , blacklistedSelector
  , blockedSelector
  , expirationDateSelector
  , inStationSelector
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

-- | @- transitBalance@
transitBalance :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO (Id NSDecimalNumber)
transitBalance pkTransitPassProperties =
  sendMessage pkTransitPassProperties transitBalanceSelector

-- | @- transitBalanceCurrencyCode@
transitBalanceCurrencyCode :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO (Id NSString)
transitBalanceCurrencyCode pkTransitPassProperties =
  sendMessage pkTransitPassProperties transitBalanceCurrencyCodeSelector

-- | @- blacklisted@
blacklisted :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO Bool
blacklisted pkTransitPassProperties =
  sendMessage pkTransitPassProperties blacklistedSelector

-- | @- expirationDate@
expirationDate :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO (Id NSDate)
expirationDate pkTransitPassProperties =
  sendMessage pkTransitPassProperties expirationDateSelector

-- | @- blocked@
blocked :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO Bool
blocked pkTransitPassProperties =
  sendMessage pkTransitPassProperties blockedSelector

-- | @- inStation@
inStation :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO Bool
inStation pkTransitPassProperties =
  sendMessage pkTransitPassProperties inStationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transitBalance@
transitBalanceSelector :: Selector '[] (Id NSDecimalNumber)
transitBalanceSelector = mkSelector "transitBalance"

-- | @Selector@ for @transitBalanceCurrencyCode@
transitBalanceCurrencyCodeSelector :: Selector '[] (Id NSString)
transitBalanceCurrencyCodeSelector = mkSelector "transitBalanceCurrencyCode"

-- | @Selector@ for @blacklisted@
blacklistedSelector :: Selector '[] Bool
blacklistedSelector = mkSelector "blacklisted"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @blocked@
blockedSelector :: Selector '[] Bool
blockedSelector = mkSelector "blocked"

-- | @Selector@ for @inStation@
inStationSelector :: Selector '[] Bool
inStationSelector = mkSelector "inStation"

