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
  , successWithResolvedPersonSelector
  , disambiguationWithPeopleToDisambiguateSelector
  , confirmationRequiredWithPersonToConfirmSelector


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

-- | @+ successWithResolvedPerson:@
successWithResolvedPerson :: IsINPerson resolvedPerson => resolvedPerson -> IO (Id INPersonResolutionResult)
successWithResolvedPerson resolvedPerson =
  do
    cls' <- getRequiredClass "INPersonResolutionResult"
    withObjCPtr resolvedPerson $ \raw_resolvedPerson ->
      sendClassMsg cls' (mkSelector "successWithResolvedPerson:") (retPtr retVoid) [argPtr (castPtr raw_resolvedPerson :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithPeopleToDisambiguate:@
disambiguationWithPeopleToDisambiguate :: IsNSArray peopleToDisambiguate => peopleToDisambiguate -> IO (Id INPersonResolutionResult)
disambiguationWithPeopleToDisambiguate peopleToDisambiguate =
  do
    cls' <- getRequiredClass "INPersonResolutionResult"
    withObjCPtr peopleToDisambiguate $ \raw_peopleToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithPeopleToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_peopleToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPersonToConfirm:@
confirmationRequiredWithPersonToConfirm :: IsINPerson personToConfirm => personToConfirm -> IO (Id INPersonResolutionResult)
confirmationRequiredWithPersonToConfirm personToConfirm =
  do
    cls' <- getRequiredClass "INPersonResolutionResult"
    withObjCPtr personToConfirm $ \raw_personToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithPersonToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_personToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPerson:@
successWithResolvedPersonSelector :: Selector
successWithResolvedPersonSelector = mkSelector "successWithResolvedPerson:"

-- | @Selector@ for @disambiguationWithPeopleToDisambiguate:@
disambiguationWithPeopleToDisambiguateSelector :: Selector
disambiguationWithPeopleToDisambiguateSelector = mkSelector "disambiguationWithPeopleToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPersonToConfirm:@
confirmationRequiredWithPersonToConfirmSelector :: Selector
confirmationRequiredWithPersonToConfirmSelector = mkSelector "confirmationRequiredWithPersonToConfirm:"

