{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithBillTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedBillTypeSelector
  , successWithResolvedValueSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' successWithResolvedBillTypeSelector resolvedBillType

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INBillType -> IO (Id INBillTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithBillTypeToConfirm:@
confirmationRequiredWithBillTypeToConfirm :: INBillType -> IO (Id INBillTypeResolutionResult)
confirmationRequiredWithBillTypeToConfirm billTypeToConfirm =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithBillTypeToConfirmSelector billTypeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INBillType -> IO (Id INBillTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INBillTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedBillType:@
successWithResolvedBillTypeSelector :: Selector '[INBillType] (Id INBillTypeResolutionResult)
successWithResolvedBillTypeSelector = mkSelector "successWithResolvedBillType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INBillType] (Id INBillTypeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithBillTypeToConfirm:@
confirmationRequiredWithBillTypeToConfirmSelector :: Selector '[INBillType] (Id INBillTypeResolutionResult)
confirmationRequiredWithBillTypeToConfirmSelector = mkSelector "confirmationRequiredWithBillTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INBillType] (Id INBillTypeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

