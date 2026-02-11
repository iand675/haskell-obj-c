{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLengthResolutionResult@.
module ObjC.Intents.INLengthResolutionResult
  ( INLengthResolutionResult
  , IsINLengthResolutionResult(..)
  , successWithResolvedLength
  , disambiguationWithLengthsToDisambiguate
  , confirmationRequiredWithLengthToConfirm
  , successWithResolvedLengthSelector
  , disambiguationWithLengthsToDisambiguateSelector
  , confirmationRequiredWithLengthToConfirmSelector


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

-- | @+ successWithResolvedLength:@
successWithResolvedLength :: IsNSMeasurement resolvedLength => resolvedLength -> IO (Id INLengthResolutionResult)
successWithResolvedLength resolvedLength =
  do
    cls' <- getRequiredClass "INLengthResolutionResult"
    withObjCPtr resolvedLength $ \raw_resolvedLength ->
      sendClassMsg cls' (mkSelector "successWithResolvedLength:") (retPtr retVoid) [argPtr (castPtr raw_resolvedLength :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithLengthsToDisambiguate:@
disambiguationWithLengthsToDisambiguate :: IsNSArray lengthsToDisambiguate => lengthsToDisambiguate -> IO (Id INLengthResolutionResult)
disambiguationWithLengthsToDisambiguate lengthsToDisambiguate =
  do
    cls' <- getRequiredClass "INLengthResolutionResult"
    withObjCPtr lengthsToDisambiguate $ \raw_lengthsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithLengthsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_lengthsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithLengthToConfirm:@
confirmationRequiredWithLengthToConfirm :: IsNSMeasurement lengthToConfirm => lengthToConfirm -> IO (Id INLengthResolutionResult)
confirmationRequiredWithLengthToConfirm lengthToConfirm =
  do
    cls' <- getRequiredClass "INLengthResolutionResult"
    withObjCPtr lengthToConfirm $ \raw_lengthToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithLengthToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_lengthToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedLength:@
successWithResolvedLengthSelector :: Selector
successWithResolvedLengthSelector = mkSelector "successWithResolvedLength:"

-- | @Selector@ for @disambiguationWithLengthsToDisambiguate:@
disambiguationWithLengthsToDisambiguateSelector :: Selector
disambiguationWithLengthsToDisambiguateSelector = mkSelector "disambiguationWithLengthsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithLengthToConfirm:@
confirmationRequiredWithLengthToConfirmSelector :: Selector
confirmationRequiredWithLengthToConfirmSelector = mkSelector "confirmationRequiredWithLengthToConfirm:"

