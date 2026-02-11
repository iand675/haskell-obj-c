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
  , transitBalanceSelector
  , transitBalanceCurrencyCodeSelector
  , blacklistedSelector
  , expirationDateSelector
  , blockedSelector
  , inStationSelector


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

-- | @- transitBalance@
transitBalance :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO (Id NSDecimalNumber)
transitBalance pkTransitPassProperties  =
  sendMsg pkTransitPassProperties (mkSelector "transitBalance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transitBalanceCurrencyCode@
transitBalanceCurrencyCode :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO (Id NSString)
transitBalanceCurrencyCode pkTransitPassProperties  =
  sendMsg pkTransitPassProperties (mkSelector "transitBalanceCurrencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- blacklisted@
blacklisted :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO Bool
blacklisted pkTransitPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkTransitPassProperties (mkSelector "blacklisted") retCULong []

-- | @- expirationDate@
expirationDate :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO (Id NSDate)
expirationDate pkTransitPassProperties  =
  sendMsg pkTransitPassProperties (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- blocked@
blocked :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO Bool
blocked pkTransitPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkTransitPassProperties (mkSelector "blocked") retCULong []

-- | @- inStation@
inStation :: IsPKTransitPassProperties pkTransitPassProperties => pkTransitPassProperties -> IO Bool
inStation pkTransitPassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkTransitPassProperties (mkSelector "inStation") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transitBalance@
transitBalanceSelector :: Selector
transitBalanceSelector = mkSelector "transitBalance"

-- | @Selector@ for @transitBalanceCurrencyCode@
transitBalanceCurrencyCodeSelector :: Selector
transitBalanceCurrencyCodeSelector = mkSelector "transitBalanceCurrencyCode"

-- | @Selector@ for @blacklisted@
blacklistedSelector :: Selector
blacklistedSelector = mkSelector "blacklisted"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @blocked@
blockedSelector :: Selector
blockedSelector = mkSelector "blocked"

-- | @Selector@ for @inStation@
inStationSelector :: Selector
inStationSelector = mkSelector "inStation"

