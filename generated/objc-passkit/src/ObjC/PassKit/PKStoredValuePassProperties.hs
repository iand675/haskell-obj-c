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
  , passPropertiesForPassSelector
  , blacklistedSelector
  , blockedSelector
  , expirationDateSelector
  , balancesSelector


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

-- | @+ passPropertiesForPass:@
passPropertiesForPass :: IsPKPass pass => pass -> IO (Id PKStoredValuePassProperties)
passPropertiesForPass pass =
  do
    cls' <- getRequiredClass "PKStoredValuePassProperties"
    withObjCPtr pass $ \raw_pass ->
      sendClassMsg cls' (mkSelector "passPropertiesForPass:") (retPtr retVoid) [argPtr (castPtr raw_pass :: Ptr ())] >>= retainedObject . castPtr

-- | @- blacklisted@
blacklisted :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO Bool
blacklisted pkStoredValuePassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkStoredValuePassProperties (mkSelector "blacklisted") retCULong []

-- | @- blocked@
blocked :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO Bool
blocked pkStoredValuePassProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkStoredValuePassProperties (mkSelector "blocked") retCULong []

-- | @- expirationDate@
expirationDate :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO (Id NSDate)
expirationDate pkStoredValuePassProperties  =
  sendMsg pkStoredValuePassProperties (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- balances@
balances :: IsPKStoredValuePassProperties pkStoredValuePassProperties => pkStoredValuePassProperties -> IO (Id NSArray)
balances pkStoredValuePassProperties  =
  sendMsg pkStoredValuePassProperties (mkSelector "balances") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @passPropertiesForPass:@
passPropertiesForPassSelector :: Selector
passPropertiesForPassSelector = mkSelector "passPropertiesForPass:"

-- | @Selector@ for @blacklisted@
blacklistedSelector :: Selector
blacklistedSelector = mkSelector "blacklisted"

-- | @Selector@ for @blocked@
blockedSelector :: Selector
blockedSelector = mkSelector "blocked"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @balances@
balancesSelector :: Selector
balancesSelector = mkSelector "balances"

