{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpeedResolutionResult@.
module ObjC.Intents.INSpeedResolutionResult
  ( INSpeedResolutionResult
  , IsINSpeedResolutionResult(..)
  , successWithResolvedSpeed
  , disambiguationWithSpeedToDisambiguate
  , confirmationRequiredWithSpeedToConfirm
  , successWithResolvedSpeedSelector
  , disambiguationWithSpeedToDisambiguateSelector
  , confirmationRequiredWithSpeedToConfirmSelector


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

-- | @+ successWithResolvedSpeed:@
successWithResolvedSpeed :: IsNSMeasurement resolvedSpeed => resolvedSpeed -> IO (Id INSpeedResolutionResult)
successWithResolvedSpeed resolvedSpeed =
  do
    cls' <- getRequiredClass "INSpeedResolutionResult"
    withObjCPtr resolvedSpeed $ \raw_resolvedSpeed ->
      sendClassMsg cls' (mkSelector "successWithResolvedSpeed:") (retPtr retVoid) [argPtr (castPtr raw_resolvedSpeed :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithSpeedToDisambiguate:@
disambiguationWithSpeedToDisambiguate :: IsNSArray speedToDisambiguate => speedToDisambiguate -> IO (Id INSpeedResolutionResult)
disambiguationWithSpeedToDisambiguate speedToDisambiguate =
  do
    cls' <- getRequiredClass "INSpeedResolutionResult"
    withObjCPtr speedToDisambiguate $ \raw_speedToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithSpeedToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_speedToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithSpeedToConfirm:@
confirmationRequiredWithSpeedToConfirm :: IsNSMeasurement speedToConfirm => speedToConfirm -> IO (Id INSpeedResolutionResult)
confirmationRequiredWithSpeedToConfirm speedToConfirm =
  do
    cls' <- getRequiredClass "INSpeedResolutionResult"
    withObjCPtr speedToConfirm $ \raw_speedToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithSpeedToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_speedToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedSpeed:@
successWithResolvedSpeedSelector :: Selector
successWithResolvedSpeedSelector = mkSelector "successWithResolvedSpeed:"

-- | @Selector@ for @disambiguationWithSpeedToDisambiguate:@
disambiguationWithSpeedToDisambiguateSelector :: Selector
disambiguationWithSpeedToDisambiguateSelector = mkSelector "disambiguationWithSpeedToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithSpeedToConfirm:@
confirmationRequiredWithSpeedToConfirmSelector :: Selector
confirmationRequiredWithSpeedToConfirmSelector = mkSelector "confirmationRequiredWithSpeedToConfirm:"

