{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , sessionSelector
  , setSessionSelector
  , setStatusSelector
  , statusSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithStatus:merchantSession:@
initWithStatus_merchantSession :: (IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate, IsPKPaymentMerchantSession session) => pkPaymentRequestMerchantSessionUpdate -> PKPaymentAuthorizationStatus -> session -> IO (Id PKPaymentRequestMerchantSessionUpdate)
initWithStatus_merchantSession pkPaymentRequestMerchantSessionUpdate status session =
  sendOwnedMessage pkPaymentRequestMerchantSessionUpdate initWithStatus_merchantSessionSelector status (toPKPaymentMerchantSession session)

-- | @- status@
status :: IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate => pkPaymentRequestMerchantSessionUpdate -> IO PKPaymentAuthorizationStatus
status pkPaymentRequestMerchantSessionUpdate =
  sendMessage pkPaymentRequestMerchantSessionUpdate statusSelector

-- | @- setStatus:@
setStatus :: IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate => pkPaymentRequestMerchantSessionUpdate -> PKPaymentAuthorizationStatus -> IO ()
setStatus pkPaymentRequestMerchantSessionUpdate value =
  sendMessage pkPaymentRequestMerchantSessionUpdate setStatusSelector value

-- | @- session@
session :: IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate => pkPaymentRequestMerchantSessionUpdate -> IO (Id PKPaymentMerchantSession)
session pkPaymentRequestMerchantSessionUpdate =
  sendMessage pkPaymentRequestMerchantSessionUpdate sessionSelector

-- | @- setSession:@
setSession :: (IsPKPaymentRequestMerchantSessionUpdate pkPaymentRequestMerchantSessionUpdate, IsPKPaymentMerchantSession value) => pkPaymentRequestMerchantSessionUpdate -> value -> IO ()
setSession pkPaymentRequestMerchantSessionUpdate value =
  sendMessage pkPaymentRequestMerchantSessionUpdate setSessionSelector (toPKPaymentMerchantSession value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStatus:merchantSession:@
initWithStatus_merchantSessionSelector :: Selector '[PKPaymentAuthorizationStatus, Id PKPaymentMerchantSession] (Id PKPaymentRequestMerchantSessionUpdate)
initWithStatus_merchantSessionSelector = mkSelector "initWithStatus:merchantSession:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] PKPaymentAuthorizationStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[PKPaymentAuthorizationStatus] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @session@
sessionSelector :: Selector '[] (Id PKPaymentMerchantSession)
sessionSelector = mkSelector "session"

-- | @Selector@ for @setSession:@
setSessionSelector :: Selector '[Id PKPaymentMerchantSession] ()
setSessionSelector = mkSelector "setSession:"

