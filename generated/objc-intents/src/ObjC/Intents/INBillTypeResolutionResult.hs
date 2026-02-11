{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBillTypeResolutionResult@.
module ObjC.Intents.INBillTypeResolutionResult
  ( INBillTypeResolutionResult
  , IsINBillTypeResolutionResult(..)
  , successWithResolvedBillType
  , successWithResolvedValue
  , confirmationRequiredWithBillTypeToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedBillTypeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithBillTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INBillType(INBillType)
  , pattern INBillTypeUnknown
  , pattern INBillTypeAutoInsurance
  , pattern INBillTypeCable
  , pattern INBillTypeCarLease
  , pattern INBillTypeCarLoan
  , pattern INBillTypeCreditCard
  , pattern INBillTypeElectricity
  , pattern INBillTypeGas
  , pattern INBillTypeGarbageAndRecycling
  , pattern INBillTypeHealthInsurance
  , pattern INBillTypeHomeInsurance
  , pattern INBillTypeInternet
  , pattern INBillTypeLifeInsurance
  , pattern INBillTypeMortgage
  , pattern INBillTypeMusicStreaming
  , pattern INBillTypePhone
  , pattern INBillTypeRent
  , pattern INBillTypeSewer
  , pattern INBillTypeStudentLoan
  , pattern INBillTypeTrafficTicket
  , pattern INBillTypeTuition
  , pattern INBillTypeUtilities
  , pattern INBillTypeWater

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

-- | @+ successWithResolvedBillType:@
successWithResolvedBillType :: INBillType -> IO (Id INBillTypeResolutionResult)
successWithResolvedBillType resolvedBillType =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedBillType:") (retPtr retVoid) [argCLong (coerce resolvedBillType)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INBillType -> IO (Id INBillTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithBillTypeToConfirm:@
confirmationRequiredWithBillTypeToConfirm :: INBillType -> IO (Id INBillTypeResolutionResult)
confirmationRequiredWithBillTypeToConfirm billTypeToConfirm =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithBillTypeToConfirm:") (retPtr retVoid) [argCLong (coerce billTypeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INBillType -> IO (Id INBillTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedBillType:@
successWithResolvedBillTypeSelector :: Selector
successWithResolvedBillTypeSelector = mkSelector "successWithResolvedBillType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithBillTypeToConfirm:@
confirmationRequiredWithBillTypeToConfirmSelector :: Selector
confirmationRequiredWithBillTypeToConfirmSelector = mkSelector "confirmationRequiredWithBillTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

