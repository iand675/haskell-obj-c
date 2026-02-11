{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentAmount@.
module ObjC.Intents.INPaymentAmount
  ( INPaymentAmount
  , IsINPaymentAmount(..)
  , init_
  , initWithAmountType_amount
  , amount
  , amountType
  , initSelector
  , initWithAmountType_amountSelector
  , amountSelector
  , amountTypeSelector

  -- * Enum types
  , INAmountType(INAmountType)
  , pattern INAmountTypeUnknown
  , pattern INAmountTypeMinimumDue
  , pattern INAmountTypeAmountDue
  , pattern INAmountTypeCurrentBalance
  , pattern INAmountTypeMaximumTransferAmount
  , pattern INAmountTypeMinimumTransferAmount
  , pattern INAmountTypeStatementBalance

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
init_ :: IsINPaymentAmount inPaymentAmount => inPaymentAmount -> IO (Id INPaymentAmount)
init_ inPaymentAmount  =
  sendMsg inPaymentAmount (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAmountType:amount:@
initWithAmountType_amount :: (IsINPaymentAmount inPaymentAmount, IsINCurrencyAmount amount) => inPaymentAmount -> INAmountType -> amount -> IO (Id INPaymentAmount)
initWithAmountType_amount inPaymentAmount  amountType amount =
withObjCPtr amount $ \raw_amount ->
    sendMsg inPaymentAmount (mkSelector "initWithAmountType:amount:") (retPtr retVoid) [argCLong (coerce amountType), argPtr (castPtr raw_amount :: Ptr ())] >>= ownedObject . castPtr

-- | @- amount@
amount :: IsINPaymentAmount inPaymentAmount => inPaymentAmount -> IO (Id INCurrencyAmount)
amount inPaymentAmount  =
  sendMsg inPaymentAmount (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- amountType@
amountType :: IsINPaymentAmount inPaymentAmount => inPaymentAmount -> IO INAmountType
amountType inPaymentAmount  =
  fmap (coerce :: CLong -> INAmountType) $ sendMsg inPaymentAmount (mkSelector "amountType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAmountType:amount:@
initWithAmountType_amountSelector :: Selector
initWithAmountType_amountSelector = mkSelector "initWithAmountType:amount:"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @amountType@
amountTypeSelector :: Selector
amountTypeSelector = mkSelector "amountType"

