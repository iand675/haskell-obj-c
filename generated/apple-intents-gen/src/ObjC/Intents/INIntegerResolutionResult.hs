{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntegerResolutionResult@.
module ObjC.Intents.INIntegerResolutionResult
  ( INIntegerResolutionResult
  , IsINIntegerResolutionResult(..)
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
successWithResolvedValue :: CLong -> IO (Id INIntegerResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INIntegerResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: IsNSNumber valueToConfirm => valueToConfirm -> IO (Id INIntegerResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INIntegerResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector (toNSNumber valueToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[CLong] (Id INIntegerResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[Id NSNumber] (Id INIntegerResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

