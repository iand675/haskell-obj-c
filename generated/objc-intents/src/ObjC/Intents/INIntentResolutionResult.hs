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
  , initSelector
  , needsValueSelector
  , notRequiredSelector
  , unsupportedSelector
  , unsupportedWithReasonSelector
  , confirmationRequiredWithItemToConfirm_forReasonSelector


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

-- | @- init@
init_ :: IsINIntentResolutionResult inIntentResolutionResult => inIntentResolutionResult -> IO (Id INIntentResolutionResult)
init_ inIntentResolutionResult  =
  sendMsg inIntentResolutionResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ needsValue@
needsValue :: IO (Id INIntentResolutionResult)
needsValue  =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMsg cls' (mkSelector "needsValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ notRequired@
notRequired :: IO (Id INIntentResolutionResult)
notRequired  =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMsg cls' (mkSelector "notRequired") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ unsupported@
unsupported :: IO (Id INIntentResolutionResult)
unsupported  =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMsg cls' (mkSelector "unsupported") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ unsupportedWithReason:@
unsupportedWithReason :: CLong -> IO (Id INIntentResolutionResult)
unsupportedWithReason reason =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedWithReason:") (retPtr retVoid) [argCLong (fromIntegral reason)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithItemToConfirm:forReason:@
confirmationRequiredWithItemToConfirm_forReason :: RawId -> CLong -> IO (Id INIntentResolutionResult)
confirmationRequiredWithItemToConfirm_forReason itemToConfirm reason =
  do
    cls' <- getRequiredClass "INIntentResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithItemToConfirm:forReason:") (retPtr retVoid) [argPtr (castPtr (unRawId itemToConfirm) :: Ptr ()), argCLong (fromIntegral reason)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @needsValue@
needsValueSelector :: Selector
needsValueSelector = mkSelector "needsValue"

-- | @Selector@ for @notRequired@
notRequiredSelector :: Selector
notRequiredSelector = mkSelector "notRequired"

-- | @Selector@ for @unsupported@
unsupportedSelector :: Selector
unsupportedSelector = mkSelector "unsupported"

-- | @Selector@ for @unsupportedWithReason:@
unsupportedWithReasonSelector :: Selector
unsupportedWithReasonSelector = mkSelector "unsupportedWithReason:"

-- | @Selector@ for @confirmationRequiredWithItemToConfirm:forReason:@
confirmationRequiredWithItemToConfirm_forReasonSelector :: Selector
confirmationRequiredWithItemToConfirm_forReasonSelector = mkSelector "confirmationRequiredWithItemToConfirm:forReason:"

