{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPersonResolutionResult@.
module ObjC.Intents.INPersonResolutionResult
  ( INPersonResolutionResult
  , IsINPersonResolutionResult(..)
  , successWithResolvedPerson
  , disambiguationWithPeopleToDisambiguate
  , confirmationRequiredWithPersonToConfirm
  , confirmationRequiredWithPersonToConfirmSelector
  , disambiguationWithPeopleToDisambiguateSelector
  , successWithResolvedPersonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedPerson:@
successWithResolvedPerson :: IsINPerson resolvedPerson => resolvedPerson -> IO (Id INPersonResolutionResult)
successWithResolvedPerson resolvedPerson =
  do
    cls' <- getRequiredClass "INPersonResolutionResult"
    sendClassMessage cls' successWithResolvedPersonSelector (toINPerson resolvedPerson)

-- | @+ disambiguationWithPeopleToDisambiguate:@
disambiguationWithPeopleToDisambiguate :: IsNSArray peopleToDisambiguate => peopleToDisambiguate -> IO (Id INPersonResolutionResult)
disambiguationWithPeopleToDisambiguate peopleToDisambiguate =
  do
    cls' <- getRequiredClass "INPersonResolutionResult"
    sendClassMessage cls' disambiguationWithPeopleToDisambiguateSelector (toNSArray peopleToDisambiguate)

-- | @+ confirmationRequiredWithPersonToConfirm:@
confirmationRequiredWithPersonToConfirm :: IsINPerson personToConfirm => personToConfirm -> IO (Id INPersonResolutionResult)
confirmationRequiredWithPersonToConfirm personToConfirm =
  do
    cls' <- getRequiredClass "INPersonResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPersonToConfirmSelector (toINPerson personToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPerson:@
successWithResolvedPersonSelector :: Selector '[Id INPerson] (Id INPersonResolutionResult)
successWithResolvedPersonSelector = mkSelector "successWithResolvedPerson:"

-- | @Selector@ for @disambiguationWithPeopleToDisambiguate:@
disambiguationWithPeopleToDisambiguateSelector :: Selector '[Id NSArray] (Id INPersonResolutionResult)
disambiguationWithPeopleToDisambiguateSelector = mkSelector "disambiguationWithPeopleToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPersonToConfirm:@
confirmationRequiredWithPersonToConfirmSelector :: Selector '[Id INPerson] (Id INPersonResolutionResult)
confirmationRequiredWithPersonToConfirmSelector = mkSelector "confirmationRequiredWithPersonToConfirm:"

