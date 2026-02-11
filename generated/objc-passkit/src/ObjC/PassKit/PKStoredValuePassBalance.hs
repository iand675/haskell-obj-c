{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKStoredValuePassBalance@.
module ObjC.PassKit.PKStoredValuePassBalance
  ( PKStoredValuePassBalance
  , IsPKStoredValuePassBalance(..)
  , init_
  , new
  , isEqualToBalance
  , currencyCode
  , balanceType
  , expiryDate
  , initSelector
  , newSelector
  , isEqualToBalanceSelector
  , currencyCodeSelector
  , balanceTypeSelector
  , expiryDateSelector


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

-- | @- init@
init_ :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id PKStoredValuePassBalance)
init_ pkStoredValuePassBalance  =
  sendMsg pkStoredValuePassBalance (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKStoredValuePassBalance)
new  =
  do
    cls' <- getRequiredClass "PKStoredValuePassBalance"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- isEqualToBalance:@
isEqualToBalance :: (IsPKStoredValuePassBalance pkStoredValuePassBalance, IsPKStoredValuePassBalance balance) => pkStoredValuePassBalance -> balance -> IO Bool
isEqualToBalance pkStoredValuePassBalance  balance =
withObjCPtr balance $ \raw_balance ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkStoredValuePassBalance (mkSelector "isEqualToBalance:") retCULong [argPtr (castPtr raw_balance :: Ptr ())]

-- | @- currencyCode@
currencyCode :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSString)
currencyCode pkStoredValuePassBalance  =
  sendMsg pkStoredValuePassBalance (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- balanceType@
balanceType :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSString)
balanceType pkStoredValuePassBalance  =
  sendMsg pkStoredValuePassBalance (mkSelector "balanceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- expiryDate@
expiryDate :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSDate)
expiryDate pkStoredValuePassBalance  =
  sendMsg pkStoredValuePassBalance (mkSelector "expiryDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @isEqualToBalance:@
isEqualToBalanceSelector :: Selector
isEqualToBalanceSelector = mkSelector "isEqualToBalance:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @balanceType@
balanceTypeSelector :: Selector
balanceTypeSelector = mkSelector "balanceType"

-- | @Selector@ for @expiryDate@
expiryDateSelector :: Selector
expiryDateSelector = mkSelector "expiryDate"

