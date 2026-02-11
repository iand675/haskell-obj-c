{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlacemarkResolutionResult@.
module ObjC.Intents.INPlacemarkResolutionResult
  ( INPlacemarkResolutionResult
  , IsINPlacemarkResolutionResult(..)
  , successWithResolvedPlacemark
  , disambiguationWithPlacemarksToDisambiguate
  , confirmationRequiredWithPlacemarkToConfirm
  , successWithResolvedPlacemarkSelector
  , disambiguationWithPlacemarksToDisambiguateSelector
  , confirmationRequiredWithPlacemarkToConfirmSelector


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
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedPlacemark:@
successWithResolvedPlacemark :: IsCLPlacemark resolvedPlacemark => resolvedPlacemark -> IO (Id INPlacemarkResolutionResult)
successWithResolvedPlacemark resolvedPlacemark =
  do
    cls' <- getRequiredClass "INPlacemarkResolutionResult"
    withObjCPtr resolvedPlacemark $ \raw_resolvedPlacemark ->
      sendClassMsg cls' (mkSelector "successWithResolvedPlacemark:") (retPtr retVoid) [argPtr (castPtr raw_resolvedPlacemark :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithPlacemarksToDisambiguate:@
disambiguationWithPlacemarksToDisambiguate :: IsNSArray placemarksToDisambiguate => placemarksToDisambiguate -> IO (Id INPlacemarkResolutionResult)
disambiguationWithPlacemarksToDisambiguate placemarksToDisambiguate =
  do
    cls' <- getRequiredClass "INPlacemarkResolutionResult"
    withObjCPtr placemarksToDisambiguate $ \raw_placemarksToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithPlacemarksToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_placemarksToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPlacemarkToConfirm:@
confirmationRequiredWithPlacemarkToConfirm :: IsCLPlacemark placemarkToConfirm => placemarkToConfirm -> IO (Id INPlacemarkResolutionResult)
confirmationRequiredWithPlacemarkToConfirm placemarkToConfirm =
  do
    cls' <- getRequiredClass "INPlacemarkResolutionResult"
    withObjCPtr placemarkToConfirm $ \raw_placemarkToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithPlacemarkToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_placemarkToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPlacemark:@
successWithResolvedPlacemarkSelector :: Selector
successWithResolvedPlacemarkSelector = mkSelector "successWithResolvedPlacemark:"

-- | @Selector@ for @disambiguationWithPlacemarksToDisambiguate:@
disambiguationWithPlacemarksToDisambiguateSelector :: Selector
disambiguationWithPlacemarksToDisambiguateSelector = mkSelector "disambiguationWithPlacemarksToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPlacemarkToConfirm:@
confirmationRequiredWithPlacemarkToConfirmSelector :: Selector
confirmationRequiredWithPlacemarkToConfirmSelector = mkSelector "confirmationRequiredWithPlacemarkToConfirm:"

