{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMassResolutionResult@.
module ObjC.Intents.INMassResolutionResult
  ( INMassResolutionResult
  , IsINMassResolutionResult(..)
  , successWithResolvedMass
  , disambiguationWithMassToDisambiguate
  , confirmationRequiredWithMassToConfirm
  , successWithResolvedMassSelector
  , disambiguationWithMassToDisambiguateSelector
  , confirmationRequiredWithMassToConfirmSelector


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

-- | @+ successWithResolvedMass:@
successWithResolvedMass :: IsNSMeasurement resolvedMass => resolvedMass -> IO (Id INMassResolutionResult)
successWithResolvedMass resolvedMass =
  do
    cls' <- getRequiredClass "INMassResolutionResult"
    withObjCPtr resolvedMass $ \raw_resolvedMass ->
      sendClassMsg cls' (mkSelector "successWithResolvedMass:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMass :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithMassToDisambiguate:@
disambiguationWithMassToDisambiguate :: IsNSArray massToDisambiguate => massToDisambiguate -> IO (Id INMassResolutionResult)
disambiguationWithMassToDisambiguate massToDisambiguate =
  do
    cls' <- getRequiredClass "INMassResolutionResult"
    withObjCPtr massToDisambiguate $ \raw_massToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithMassToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_massToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithMassToConfirm:@
confirmationRequiredWithMassToConfirm :: IsNSMeasurement massToConfirm => massToConfirm -> IO (Id INMassResolutionResult)
confirmationRequiredWithMassToConfirm massToConfirm =
  do
    cls' <- getRequiredClass "INMassResolutionResult"
    withObjCPtr massToConfirm $ \raw_massToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithMassToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_massToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMass:@
successWithResolvedMassSelector :: Selector
successWithResolvedMassSelector = mkSelector "successWithResolvedMass:"

-- | @Selector@ for @disambiguationWithMassToDisambiguate:@
disambiguationWithMassToDisambiguateSelector :: Selector
disambiguationWithMassToDisambiguateSelector = mkSelector "disambiguationWithMassToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithMassToConfirm:@
confirmationRequiredWithMassToConfirmSelector :: Selector
confirmationRequiredWithMassToConfirmSelector = mkSelector "confirmationRequiredWithMassToConfirm:"

