{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaDestinationResolutionResult@.
module ObjC.Intents.INMediaDestinationResolutionResult
  ( INMediaDestinationResolutionResult
  , IsINMediaDestinationResolutionResult(..)
  , successWithResolvedMediaDestination
  , disambiguationWithMediaDestinationsToDisambiguate
  , confirmationRequiredWithMediaDestinationToConfirm
  , confirmationRequiredWithMediaDestinationToConfirmSelector
  , disambiguationWithMediaDestinationsToDisambiguateSelector
  , successWithResolvedMediaDestinationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedMediaDestination:@
successWithResolvedMediaDestination :: IsINMediaDestination resolvedMediaDestination => resolvedMediaDestination -> IO (Id INMediaDestinationResolutionResult)
successWithResolvedMediaDestination resolvedMediaDestination =
  do
    cls' <- getRequiredClass "INMediaDestinationResolutionResult"
    sendClassMessage cls' successWithResolvedMediaDestinationSelector (toINMediaDestination resolvedMediaDestination)

-- | @+ disambiguationWithMediaDestinationsToDisambiguate:@
disambiguationWithMediaDestinationsToDisambiguate :: IsNSArray mediaDestinationsToDisambiguate => mediaDestinationsToDisambiguate -> IO (Id INMediaDestinationResolutionResult)
disambiguationWithMediaDestinationsToDisambiguate mediaDestinationsToDisambiguate =
  do
    cls' <- getRequiredClass "INMediaDestinationResolutionResult"
    sendClassMessage cls' disambiguationWithMediaDestinationsToDisambiguateSelector (toNSArray mediaDestinationsToDisambiguate)

-- | @+ confirmationRequiredWithMediaDestinationToConfirm:@
confirmationRequiredWithMediaDestinationToConfirm :: IsINMediaDestination mediaDestinationToConfirm => mediaDestinationToConfirm -> IO (Id INMediaDestinationResolutionResult)
confirmationRequiredWithMediaDestinationToConfirm mediaDestinationToConfirm =
  do
    cls' <- getRequiredClass "INMediaDestinationResolutionResult"
    sendClassMessage cls' confirmationRequiredWithMediaDestinationToConfirmSelector (toINMediaDestination mediaDestinationToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMediaDestination:@
successWithResolvedMediaDestinationSelector :: Selector '[Id INMediaDestination] (Id INMediaDestinationResolutionResult)
successWithResolvedMediaDestinationSelector = mkSelector "successWithResolvedMediaDestination:"

-- | @Selector@ for @disambiguationWithMediaDestinationsToDisambiguate:@
disambiguationWithMediaDestinationsToDisambiguateSelector :: Selector '[Id NSArray] (Id INMediaDestinationResolutionResult)
disambiguationWithMediaDestinationsToDisambiguateSelector = mkSelector "disambiguationWithMediaDestinationsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithMediaDestinationToConfirm:@
confirmationRequiredWithMediaDestinationToConfirmSelector :: Selector '[Id INMediaDestination] (Id INMediaDestinationResolutionResult)
confirmationRequiredWithMediaDestinationToConfirmSelector = mkSelector "confirmationRequiredWithMediaDestinationToConfirm:"

