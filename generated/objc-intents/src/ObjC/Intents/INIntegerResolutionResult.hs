{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntegerResolutionResult@.
module ObjC.Intents.INIntegerResolutionResult
  ( INIntegerResolutionResult
  , IsINIntegerResolutionResult(..)
  , successWithResolvedValue
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedValueSelector
  , confirmationRequiredWithValueToConfirmSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: CLong -> IO (Id INIntegerResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INIntegerResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (fromIntegral resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: IsNSNumber valueToConfirm => valueToConfirm -> IO (Id INIntegerResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INIntegerResolutionResult"
    withObjCPtr valueToConfirm $ \raw_valueToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_valueToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

