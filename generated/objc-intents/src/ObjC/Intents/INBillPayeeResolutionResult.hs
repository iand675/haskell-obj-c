{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBillPayeeResolutionResult@.
module ObjC.Intents.INBillPayeeResolutionResult
  ( INBillPayeeResolutionResult
  , IsINBillPayeeResolutionResult(..)
  , successWithResolvedBillPayee
  , disambiguationWithBillPayeesToDisambiguate
  , confirmationRequiredWithBillPayeeToConfirm
  , successWithResolvedBillPayeeSelector
  , disambiguationWithBillPayeesToDisambiguateSelector
  , confirmationRequiredWithBillPayeeToConfirmSelector


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

-- | @+ successWithResolvedBillPayee:@
successWithResolvedBillPayee :: IsINBillPayee resolvedBillPayee => resolvedBillPayee -> IO (Id INBillPayeeResolutionResult)
successWithResolvedBillPayee resolvedBillPayee =
  do
    cls' <- getRequiredClass "INBillPayeeResolutionResult"
    withObjCPtr resolvedBillPayee $ \raw_resolvedBillPayee ->
      sendClassMsg cls' (mkSelector "successWithResolvedBillPayee:") (retPtr retVoid) [argPtr (castPtr raw_resolvedBillPayee :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithBillPayeesToDisambiguate:@
disambiguationWithBillPayeesToDisambiguate :: IsNSArray billPayeesToDisambiguate => billPayeesToDisambiguate -> IO (Id INBillPayeeResolutionResult)
disambiguationWithBillPayeesToDisambiguate billPayeesToDisambiguate =
  do
    cls' <- getRequiredClass "INBillPayeeResolutionResult"
    withObjCPtr billPayeesToDisambiguate $ \raw_billPayeesToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithBillPayeesToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_billPayeesToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithBillPayeeToConfirm:@
confirmationRequiredWithBillPayeeToConfirm :: IsINBillPayee billPayeeToConfirm => billPayeeToConfirm -> IO (Id INBillPayeeResolutionResult)
confirmationRequiredWithBillPayeeToConfirm billPayeeToConfirm =
  do
    cls' <- getRequiredClass "INBillPayeeResolutionResult"
    withObjCPtr billPayeeToConfirm $ \raw_billPayeeToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithBillPayeeToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_billPayeeToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedBillPayee:@
successWithResolvedBillPayeeSelector :: Selector
successWithResolvedBillPayeeSelector = mkSelector "successWithResolvedBillPayee:"

-- | @Selector@ for @disambiguationWithBillPayeesToDisambiguate:@
disambiguationWithBillPayeesToDisambiguateSelector :: Selector
disambiguationWithBillPayeesToDisambiguateSelector = mkSelector "disambiguationWithBillPayeesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithBillPayeeToConfirm:@
confirmationRequiredWithBillPayeeToConfirmSelector :: Selector
confirmationRequiredWithBillPayeeToConfirmSelector = mkSelector "confirmationRequiredWithBillPayeeToConfirm:"

