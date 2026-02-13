{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INObjectResolutionResult@.
module ObjC.Intents.INObjectResolutionResult
  ( INObjectResolutionResult
  , IsINObjectResolutionResult(..)
  , successWithResolvedObject
  , disambiguationWithObjectsToDisambiguate
  , confirmationRequiredWithObjectToConfirm
  , confirmationRequiredWithObjectToConfirmSelector
  , disambiguationWithObjectsToDisambiguateSelector
  , successWithResolvedObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedObject:@
successWithResolvedObject :: IsINObject resolvedObject => resolvedObject -> IO (Id INObjectResolutionResult)
successWithResolvedObject resolvedObject =
  do
    cls' <- getRequiredClass "INObjectResolutionResult"
    sendClassMessage cls' successWithResolvedObjectSelector (toINObject resolvedObject)

-- | @+ disambiguationWithObjectsToDisambiguate:@
disambiguationWithObjectsToDisambiguate :: IsNSArray objectsToDisambiguate => objectsToDisambiguate -> IO (Id INObjectResolutionResult)
disambiguationWithObjectsToDisambiguate objectsToDisambiguate =
  do
    cls' <- getRequiredClass "INObjectResolutionResult"
    sendClassMessage cls' disambiguationWithObjectsToDisambiguateSelector (toNSArray objectsToDisambiguate)

-- | @+ confirmationRequiredWithObjectToConfirm:@
confirmationRequiredWithObjectToConfirm :: IsINObject objectToConfirm => objectToConfirm -> IO (Id INObjectResolutionResult)
confirmationRequiredWithObjectToConfirm objectToConfirm =
  do
    cls' <- getRequiredClass "INObjectResolutionResult"
    sendClassMessage cls' confirmationRequiredWithObjectToConfirmSelector (toINObject objectToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedObject:@
successWithResolvedObjectSelector :: Selector '[Id INObject] (Id INObjectResolutionResult)
successWithResolvedObjectSelector = mkSelector "successWithResolvedObject:"

-- | @Selector@ for @disambiguationWithObjectsToDisambiguate:@
disambiguationWithObjectsToDisambiguateSelector :: Selector '[Id NSArray] (Id INObjectResolutionResult)
disambiguationWithObjectsToDisambiguateSelector = mkSelector "disambiguationWithObjectsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithObjectToConfirm:@
confirmationRequiredWithObjectToConfirmSelector :: Selector '[Id INObject] (Id INObjectResolutionResult)
confirmationRequiredWithObjectToConfirmSelector = mkSelector "confirmationRequiredWithObjectToConfirm:"

