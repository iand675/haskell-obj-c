{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntentResolutionResult@.
module ObjC.Intents.INIntentResolutionResult
  ( INIntentResolutionResult
  , IsINIntentResolutionResult(..)
  , init_
  , needsValue
  , notRequired
  , unsupported
  , unsupportedWithReason
  , confirmationRequiredWithItemToConfirm_forReason
  , confirmationRequiredWithItemToConfirm_forReasonSelector
  , initSelector
  , needsValueSelector
  , notRequiredSelector
  , unsupportedSelector
  , unsupportedWithReasonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINIntentResolutionResult inIntentResolutionResult => inIntentResolutionResult -> IO (Id INIntentResolutionResult)
init_ inIntentResolutionResult =
  sendOwnedMessage inIntentResolutionResult initSelector

-- | @+ needsValue@
needsValue :: IO (Id INIntentResolutionResult)
needsValue  =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMessage cls' needsValueSelector

-- | @+ notRequired@
notRequired :: IO (Id INIntentResolutionResult)
notRequired  =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMessage cls' notRequiredSelector

-- | @+ unsupported@
unsupported :: IO (Id INIntentResolutionResult)
unsupported  =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMessage cls' unsupportedSelector

-- | @+ unsupportedWithReason:@
unsupportedWithReason :: CLong -> IO (Id INIntentResolutionResult)
unsupportedWithReason reason =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMessage cls' unsupportedWithReasonSelector reason

-- | @+ confirmationRequiredWithItemToConfirm:forReason:@
confirmationRequiredWithItemToConfirm_forReason :: RawId -> CLong -> IO (Id INIntentResolutionResult)
confirmationRequiredWithItemToConfirm_forReason itemToConfirm reason =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMessage cls' confirmationRequiredWithItemToConfirm_forReasonSelector itemToConfirm reason

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INIntentResolutionResult)
initSelector = mkSelector "init"

-- | @Selector@ for @needsValue@
needsValueSelector :: Selector '[] (Id INIntentResolutionResult)
needsValueSelector = mkSelector "needsValue"

-- | @Selector@ for @notRequired@
notRequiredSelector :: Selector '[] (Id INIntentResolutionResult)
notRequiredSelector = mkSelector "notRequired"

-- | @Selector@ for @unsupported@
unsupportedSelector :: Selector '[] (Id INIntentResolutionResult)
unsupportedSelector = mkSelector "unsupported"

-- | @Selector@ for @unsupportedWithReason:@
unsupportedWithReasonSelector :: Selector '[CLong] (Id INIntentResolutionResult)
unsupportedWithReasonSelector = mkSelector "unsupportedWithReason:"

-- | @Selector@ for @confirmationRequiredWithItemToConfirm:forReason:@
confirmationRequiredWithItemToConfirm_forReasonSelector :: Selector '[RawId, CLong] (Id INIntentResolutionResult)
confirmationRequiredWithItemToConfirm_forReasonSelector = mkSelector "confirmationRequiredWithItemToConfirm:forReason:"

