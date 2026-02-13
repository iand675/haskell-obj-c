{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentMerchantSession@.
module ObjC.PassKit.PKPaymentMerchantSession
  ( PKPaymentMerchantSession
  , IsPKPaymentMerchantSession(..)
  , initWithDictionary
  , initWithDictionarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDictionary:@
initWithDictionary :: (IsPKPaymentMerchantSession pkPaymentMerchantSession, IsNSDictionary dictionary) => pkPaymentMerchantSession -> dictionary -> IO (Id PKPaymentMerchantSession)
initWithDictionary pkPaymentMerchantSession dictionary =
  sendOwnedMessage pkPaymentMerchantSession initWithDictionarySelector (toNSDictionary dictionary)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id PKPaymentMerchantSession)
initWithDictionarySelector = mkSelector "initWithDictionary:"

