{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentRequestMerchantSessionUpdate@.
module ObjC.PassKit.PKPaymentRequestMerchantSessionUpdate
  ( PKPaymentRequestMerchantSessionUpdate
  , IsPKPaymentRequestMerchantSessionUpdate(..)
  , initWithStatus_merchantSession
  , status
  , setStatus
  , session
  , setSession
  , initWithStatus_merchantSessionSelector
  , statusSelector
  , setStatusSelector
  , sessionSelector
  , setSessionSelector

  -- * Enum types
  , PKPaymentAuthorizationStatus(PKPaymentAuthorizationStatus)
  , pattern PKPaymentAuthorizationStatusSuccess
  , pattern PKPaymentAuthorizationStatusFailure
  , pattern PKPaymentAuthorizationStatusInvalidBillingPostalAddress
  , pattern PKPaymentAuthorizationStatusInvalidShippingPostalAddress
  , pattern PKPaymentAuthorizationStatusInvalidShippingContact
  , pattern PKPaymentAuthorizationStatusPINRequired
  , pattern PKPaymentAuthorizationStatusPINIncorrect
  , pattern PKPaymentAuthorizationStatusPINLockout

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
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithStatus:merchantSession:@
initWithStatus_merchantSession :: (IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate, IsPKPaymentMerchantSession session) => pkPaymentRequestMerchantSessionUpdate -> PKPaymentAuthorizationStatus -> session -> IO (Id PKPaymentRequestMerchantSessionUpdate)
initWithStatus_merchantSession pkPaymentRequestMerchantSessionUpdate  status session =
withObjCPtr session $ \raw_session ->
    sendMsg pkPaymentRequestMerchantSessionUpdate (mkSelector "initWithStatus:merchantSession:") (retPtr retVoid) [argCLong (coerce status), argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate => pkPaymentRequestMerchantSessionUpdate -> IO PKPaymentAuthorizationStatus
status pkPaymentRequestMerchantSessionUpdate  =
  fmap (coerce :: CLong -> PKPaymentAuthorizationStatus) $ sendMsg pkPaymentRequestMerchantSessionUpdate (mkSelector "status") retCLong []

-- | @- setStatus:@
setStatus :: IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate => pkPaymentRequestMerchantSessionUpdate -> PKPaymentAuthorizationStatus -> IO ()
setStatus pkPaymentRequestMerchantSessionUpdate  value =
  sendMsg pkPaymentRequestMerchantSessionUpdate (mkSelector "setStatus:") retVoid [argCLong (coerce value)]

-- | @- session@
session :: IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate => pkPaymentRequestMerchantSessionUpdate -> IO (Id PKPaymentMerchantSession)
session pkPaymentRequestMerchantSessionUpdate  =
  sendMsg pkPaymentRequestMerchantSessionUpdate (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSession:@
setSession :: (IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate, IsPKPaymentMerchantSession value) => pkPaymentRequestMerchantSessionUpdate -> value -> IO ()
setSession pkPaymentRequestMerchantSessionUpdate  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentRequestMerchantSessionUpdate (mkSelector "setSession:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStatus:merchantSession:@
initWithStatus_merchantSessionSelector :: Selector
initWithStatus_merchantSessionSelector = mkSelector "initWithStatus:merchantSession:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @setSession:@
setSessionSelector :: Selector
setSessionSelector = mkSelector "setSession:"

