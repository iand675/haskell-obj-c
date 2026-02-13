{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateComponentsResolutionResult@.
module ObjC.Intents.INDateComponentsResolutionResult
  ( INDateComponentsResolutionResult
  , IsINDateComponentsResolutionResult(..)
  , successWithResolvedDateComponents
  , disambiguationWithDateComponentsToDisambiguate
  , confirmationRequiredWithDateComponentsToConfirm
  , confirmationRequiredWithDateComponentsToConfirmSelector
  , disambiguationWithDateComponentsToDisambiguateSelector
  , successWithResolvedDateComponentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedDateComponents:@
successWithResolvedDateComponents :: IsNSDateComponents resolvedDateComponents => resolvedDateComponents -> IO (Id INDateComponentsResolutionResult)
successWithResolvedDateComponents resolvedDateComponents =
  do
    cls' <- getRequiredClass "INDateComponentsResolutionResult"
    sendClassMessage cls' successWithResolvedDateComponentsSelector (toNSDateComponents resolvedDateComponents)

-- | @+ disambiguationWithDateComponentsToDisambiguate:@
disambiguationWithDateComponentsToDisambiguate :: IsNSArray dateComponentsToDisambiguate => dateComponentsToDisambiguate -> IO (Id INDateComponentsResolutionResult)
disambiguationWithDateComponentsToDisambiguate dateComponentsToDisambiguate =
  do
    cls' <- getRequiredClass "INDateComponentsResolutionResult"
    sendClassMessage cls' disambiguationWithDateComponentsToDisambiguateSelector (toNSArray dateComponentsToDisambiguate)

-- | @+ confirmationRequiredWithDateComponentsToConfirm:@
confirmationRequiredWithDateComponentsToConfirm :: IsNSDateComponents dateComponentsToConfirm => dateComponentsToConfirm -> IO (Id INDateComponentsResolutionResult)
confirmationRequiredWithDateComponentsToConfirm dateComponentsToConfirm =
  do
    cls' <- getRequiredClass "INDateComponentsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithDateComponentsToConfirmSelector (toNSDateComponents dateComponentsToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedDateComponents:@
successWithResolvedDateComponentsSelector :: Selector '[Id NSDateComponents] (Id INDateComponentsResolutionResult)
successWithResolvedDateComponentsSelector = mkSelector "successWithResolvedDateComponents:"

-- | @Selector@ for @disambiguationWithDateComponentsToDisambiguate:@
disambiguationWithDateComponentsToDisambiguateSelector :: Selector '[Id NSArray] (Id INDateComponentsResolutionResult)
disambiguationWithDateComponentsToDisambiguateSelector = mkSelector "disambiguationWithDateComponentsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithDateComponentsToConfirm:@
confirmationRequiredWithDateComponentsToConfirmSelector :: Selector '[Id NSDateComponents] (Id INDateComponentsResolutionResult)
confirmationRequiredWithDateComponentsToConfirmSelector = mkSelector "confirmationRequiredWithDateComponentsToConfirm:"

