{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKStoredValuePassProperties@.
module ObjC.PassKit.PKStoredValuePassProperties
  ( PKStoredValuePassProperties
  , IsPKStoredValuePassProperties(..)
  , passPropertiesForPass
  , blacklisted
  , blocked
  , expirationDate
  , balances
  , balancesSelector
  , blacklistedSelector
  , blockedSelector
  , expirationDateSelector
  , passPropertiesForPassSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ passPropertiesForPass:@
passPropertiesForPass :: IsPKPass pass => pass -> IO (Id PKStoredValuePassProperties)
passPropertiesForPass pass =
  do
    cls' <- getRequiredClass "PKStoredValuePassProperties"
    sendClassMessage cls' passPropertiesForPassSelector (toPKPass pass)

-- | @- blacklisted@
blacklisted :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO Bool
blacklisted pkStoredValuePassProperties =
  sendMessage pkStoredValuePassProperties blacklistedSelector

-- | @- blocked@
blocked :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO Bool
blocked pkStoredValuePassProperties =
  sendMessage pkStoredValuePassProperties blockedSelector

-- | @- expirationDate@
expirationDate :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO (Id NSDate)
expirationDate pkStoredValuePassProperties =
  sendMessage pkStoredValuePassProperties expirationDateSelector

-- | @- balances@
balances :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO (Id NSArray)
balances pkStoredValuePassProperties =
  sendMessage pkStoredValuePassProperties balancesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @passPropertiesForPass:@
passPropertiesForPassSelector :: Selector '[Id PKPass] (Id PKStoredValuePassProperties)
passPropertiesForPassSelector = mkSelector "passPropertiesForPass:"

-- | @Selector@ for @blacklisted@
blacklistedSelector :: Selector '[] Bool
blacklistedSelector = mkSelector "blacklisted"

-- | @Selector@ for @blocked@
blockedSelector :: Selector '[] Bool
blockedSelector = mkSelector "blocked"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @balances@
balancesSelector :: Selector '[] (Id NSArray)
balancesSelector = mkSelector "balances"

