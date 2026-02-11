{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentStatusResolutionResult@.
module ObjC.Intents.INPaymentStatusResolutionResult
  ( INPaymentStatusResolutionResult
  , IsINPaymentStatusResolutionResult(..)
  , successWithResolvedPaymentStatus
  , successWithResolvedValue
  , confirmationRequiredWithPaymentStatusToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedPaymentStatusSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithPaymentStatusToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INPaymentStatus(INPaymentStatus)
  , pattern INPaymentStatusUnknown
  , pattern INPaymentStatusPending
  , pattern INPaymentStatusCompleted
  , pattern INPaymentStatusCanceled
  , pattern INPaymentStatusFailed
  , pattern INPaymentStatusUnpaid

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

-- | @+ successWithResolvedPaymentStatus:@
successWithResolvedPaymentStatus :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
successWithResolvedPaymentStatus resolvedPaymentStatus =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedPaymentStatus:") (retPtr retVoid) [argCLong (coerce resolvedPaymentStatus)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPaymentStatusToConfirm:@
confirmationRequiredWithPaymentStatusToConfirm :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
confirmationRequiredWithPaymentStatusToConfirm paymentStatusToConfirm =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithPaymentStatusToConfirm:") (retPtr retVoid) [argCLong (coerce paymentStatusToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INPaymentStatus -> IO (Id INPaymentStatusResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INPaymentStatusResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentStatus:@
successWithResolvedPaymentStatusSelector :: Selector
successWithResolvedPaymentStatusSelector = mkSelector "successWithResolvedPaymentStatus:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithPaymentStatusToConfirm:@
confirmationRequiredWithPaymentStatusToConfirmSelector :: Selector
confirmationRequiredWithPaymentStatusToConfirmSelector = mkSelector "confirmationRequiredWithPaymentStatusToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

