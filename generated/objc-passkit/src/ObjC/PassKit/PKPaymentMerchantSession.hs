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

-- | @- initWithDictionary:@
initWithDictionary :: (IsPKPaymentMerchantSession pkPaymentMerchantSession, IsNSDictionary dictionary) => pkPaymentMerchantSession -> dictionary -> IO (Id PKPaymentMerchantSession)
initWithDictionary pkPaymentMerchantSession  dictionary =
withObjCPtr dictionary $ \raw_dictionary ->
    sendMsg pkPaymentMerchantSession (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_dictionary :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

