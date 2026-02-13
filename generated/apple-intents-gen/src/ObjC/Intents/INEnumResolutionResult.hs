{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEnumResolutionResult@.
module ObjC.Intents.INEnumResolutionResult
  ( INEnumResolutionResult
  , IsINEnumResolutionResult(..)
  , successWithResolvedValue
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: CLong -> IO (Id INEnumResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INEnumResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: CLong -> IO (Id INEnumResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INEnumResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[CLong] (Id INEnumResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[CLong] (Id INEnumResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

