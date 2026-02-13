{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithPlacemarkToConfirmSelector
  , disambiguationWithPlacemarksToDisambiguateSelector
  , successWithResolvedPlacemarkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' successWithResolvedPlacemarkSelector (toCLPlacemark resolvedPlacemark)

-- | @+ disambiguationWithPlacemarksToDisambiguate:@
disambiguationWithPlacemarksToDisambiguate :: IsNSArray placemarksToDisambiguate => placemarksToDisambiguate -> IO (Id INPlacemarkResolutionResult)
disambiguationWithPlacemarksToDisambiguate placemarksToDisambiguate =
  do
    cls' <- getRequiredClass "INPlacemarkResolutionResult"
    sendClassMessage cls' disambiguationWithPlacemarksToDisambiguateSelector (toNSArray placemarksToDisambiguate)

-- | @+ confirmationRequiredWithPlacemarkToConfirm:@
confirmationRequiredWithPlacemarkToConfirm :: IsCLPlacemark placemarkToConfirm => placemarkToConfirm -> IO (Id INPlacemarkResolutionResult)
confirmationRequiredWithPlacemarkToConfirm placemarkToConfirm =
  do
    cls' <- getRequiredClass "INPlacemarkResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPlacemarkToConfirmSelector (toCLPlacemark placemarkToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPlacemark:@
successWithResolvedPlacemarkSelector :: Selector '[Id CLPlacemark] (Id INPlacemarkResolutionResult)
successWithResolvedPlacemarkSelector = mkSelector "successWithResolvedPlacemark:"

-- | @Selector@ for @disambiguationWithPlacemarksToDisambiguate:@
disambiguationWithPlacemarksToDisambiguateSelector :: Selector '[Id NSArray] (Id INPlacemarkResolutionResult)
disambiguationWithPlacemarksToDisambiguateSelector = mkSelector "disambiguationWithPlacemarksToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPlacemarkToConfirm:@
confirmationRequiredWithPlacemarkToConfirmSelector :: Selector '[Id CLPlacemark] (Id INPlacemarkResolutionResult)
confirmationRequiredWithPlacemarkToConfirmSelector = mkSelector "confirmationRequiredWithPlacemarkToConfirm:"

