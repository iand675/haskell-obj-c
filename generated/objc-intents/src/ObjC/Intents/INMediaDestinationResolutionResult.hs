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
  , successWithResolvedMediaDestinationSelector
  , disambiguationWithMediaDestinationsToDisambiguateSelector
  , confirmationRequiredWithMediaDestinationToConfirmSelector


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

-- | @+ successWithResolvedMediaDestination:@
successWithResolvedMediaDestination :: IsINMediaDestination resolvedMediaDestination => resolvedMediaDestination -> IO (Id INMediaDestinationResolutionResult)
successWithResolvedMediaDestination resolvedMediaDestination =
  do
    cls' <- getRequiredClass "INMediaDestinationResolutionResult"
    withObjCPtr resolvedMediaDestination $ \raw_resolvedMediaDestination ->
      sendClassMsg cls' (mkSelector "successWithResolvedMediaDestination:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMediaDestination :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithMediaDestinationsToDisambiguate:@
disambiguationWithMediaDestinationsToDisambiguate :: IsNSArray mediaDestinationsToDisambiguate => mediaDestinationsToDisambiguate -> IO (Id INMediaDestinationResolutionResult)
disambiguationWithMediaDestinationsToDisambiguate mediaDestinationsToDisambiguate =
  do
    cls' <- getRequiredClass "INMediaDestinationResolutionResult"
    withObjCPtr mediaDestinationsToDisambiguate $ \raw_mediaDestinationsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithMediaDestinationsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_mediaDestinationsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithMediaDestinationToConfirm:@
confirmationRequiredWithMediaDestinationToConfirm :: IsINMediaDestination mediaDestinationToConfirm => mediaDestinationToConfirm -> IO (Id INMediaDestinationResolutionResult)
confirmationRequiredWithMediaDestinationToConfirm mediaDestinationToConfirm =
  do
    cls' <- getRequiredClass "INMediaDestinationResolutionResult"
    withObjCPtr mediaDestinationToConfirm $ \raw_mediaDestinationToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithMediaDestinationToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_mediaDestinationToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMediaDestination:@
successWithResolvedMediaDestinationSelector :: Selector
successWithResolvedMediaDestinationSelector = mkSelector "successWithResolvedMediaDestination:"

-- | @Selector@ for @disambiguationWithMediaDestinationsToDisambiguate:@
disambiguationWithMediaDestinationsToDisambiguateSelector :: Selector
disambiguationWithMediaDestinationsToDisambiguateSelector = mkSelector "disambiguationWithMediaDestinationsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithMediaDestinationToConfirm:@
confirmationRequiredWithMediaDestinationToConfirmSelector :: Selector
confirmationRequiredWithMediaDestinationToConfirmSelector = mkSelector "confirmationRequiredWithMediaDestinationToConfirm:"

