{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INURLResolutionResult@.
module ObjC.Intents.INURLResolutionResult
  ( INURLResolutionResult
  , IsINURLResolutionResult(..)
  , successWithResolvedURL
  , disambiguationWithURLsToDisambiguate
  , confirmationRequiredWithURLToConfirm
  , successWithResolvedURLSelector
  , disambiguationWithURLsToDisambiguateSelector
  , confirmationRequiredWithURLToConfirmSelector


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

-- | @+ successWithResolvedURL:@
successWithResolvedURL :: IsNSURL resolvedURL => resolvedURL -> IO (Id INURLResolutionResult)
successWithResolvedURL resolvedURL =
  do
    cls' <- getRequiredClass "INURLResolutionResult"
    withObjCPtr resolvedURL $ \raw_resolvedURL ->
      sendClassMsg cls' (mkSelector "successWithResolvedURL:") (retPtr retVoid) [argPtr (castPtr raw_resolvedURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithURLsToDisambiguate:@
disambiguationWithURLsToDisambiguate :: IsNSArray urlsToDisambiguate => urlsToDisambiguate -> IO (Id INURLResolutionResult)
disambiguationWithURLsToDisambiguate urlsToDisambiguate =
  do
    cls' <- getRequiredClass "INURLResolutionResult"
    withObjCPtr urlsToDisambiguate $ \raw_urlsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithURLsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_urlsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithURLToConfirm:@
confirmationRequiredWithURLToConfirm :: IsNSURL urlToConfirm => urlToConfirm -> IO (Id INURLResolutionResult)
confirmationRequiredWithURLToConfirm urlToConfirm =
  do
    cls' <- getRequiredClass "INURLResolutionResult"
    withObjCPtr urlToConfirm $ \raw_urlToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithURLToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_urlToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedURL:@
successWithResolvedURLSelector :: Selector
successWithResolvedURLSelector = mkSelector "successWithResolvedURL:"

-- | @Selector@ for @disambiguationWithURLsToDisambiguate:@
disambiguationWithURLsToDisambiguateSelector :: Selector
disambiguationWithURLsToDisambiguateSelector = mkSelector "disambiguationWithURLsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithURLToConfirm:@
confirmationRequiredWithURLToConfirmSelector :: Selector
confirmationRequiredWithURLToConfirmSelector = mkSelector "confirmationRequiredWithURLToConfirm:"

