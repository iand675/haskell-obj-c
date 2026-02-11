{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INVolumeResolutionResult@.
module ObjC.Intents.INVolumeResolutionResult
  ( INVolumeResolutionResult
  , IsINVolumeResolutionResult(..)
  , successWithResolvedVolume
  , disambiguationWithVolumeToDisambiguate
  , confirmationRequiredWithVolumeToConfirm
  , successWithResolvedVolumeSelector
  , disambiguationWithVolumeToDisambiguateSelector
  , confirmationRequiredWithVolumeToConfirmSelector


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

-- | @+ successWithResolvedVolume:@
successWithResolvedVolume :: IsNSMeasurement resolvedVolume => resolvedVolume -> IO (Id INVolumeResolutionResult)
successWithResolvedVolume resolvedVolume =
  do
    cls' <- getRequiredClass "INVolumeResolutionResult"
    withObjCPtr resolvedVolume $ \raw_resolvedVolume ->
      sendClassMsg cls' (mkSelector "successWithResolvedVolume:") (retPtr retVoid) [argPtr (castPtr raw_resolvedVolume :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithVolumeToDisambiguate:@
disambiguationWithVolumeToDisambiguate :: IsNSArray volumeToDisambiguate => volumeToDisambiguate -> IO (Id INVolumeResolutionResult)
disambiguationWithVolumeToDisambiguate volumeToDisambiguate =
  do
    cls' <- getRequiredClass "INVolumeResolutionResult"
    withObjCPtr volumeToDisambiguate $ \raw_volumeToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithVolumeToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_volumeToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithVolumeToConfirm:@
confirmationRequiredWithVolumeToConfirm :: IsNSMeasurement volumeToConfirm => volumeToConfirm -> IO (Id INVolumeResolutionResult)
confirmationRequiredWithVolumeToConfirm volumeToConfirm =
  do
    cls' <- getRequiredClass "INVolumeResolutionResult"
    withObjCPtr volumeToConfirm $ \raw_volumeToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithVolumeToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_volumeToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedVolume:@
successWithResolvedVolumeSelector :: Selector
successWithResolvedVolumeSelector = mkSelector "successWithResolvedVolume:"

-- | @Selector@ for @disambiguationWithVolumeToDisambiguate:@
disambiguationWithVolumeToDisambiguateSelector :: Selector
disambiguationWithVolumeToDisambiguateSelector = mkSelector "disambiguationWithVolumeToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithVolumeToConfirm:@
confirmationRequiredWithVolumeToConfirmSelector :: Selector
confirmationRequiredWithVolumeToConfirmSelector = mkSelector "confirmationRequiredWithVolumeToConfirm:"

