{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDoubleResolutionResult@.
module ObjC.Intents.INDoubleResolutionResult
  ( INDoubleResolutionResult
  , IsINDoubleResolutionResult(..)
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
successWithResolvedValue :: CDouble -> IO (Id INDoubleResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INDoubleResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: IsNSNumber valueToConfirm => valueToConfirm -> IO (Id INDoubleResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INDoubleResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector (toNSNumber valueToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[CDouble] (Id INDoubleResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[Id NSNumber] (Id INDoubleResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

