{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaItemResolutionResult@.
module ObjC.Intents.INMediaItemResolutionResult
  ( INMediaItemResolutionResult
  , IsINMediaItemResolutionResult(..)
  , successWithResolvedMediaItem
  , successesWithResolvedMediaItems
  , disambiguationWithMediaItemsToDisambiguate
  , confirmationRequiredWithMediaItemToConfirm
  , successWithResolvedMediaItemSelector
  , successesWithResolvedMediaItemsSelector
  , disambiguationWithMediaItemsToDisambiguateSelector
  , confirmationRequiredWithMediaItemToConfirmSelector


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

-- | @+ successWithResolvedMediaItem:@
successWithResolvedMediaItem :: IsINMediaItem resolvedMediaItem => resolvedMediaItem -> IO (Id INMediaItemResolutionResult)
successWithResolvedMediaItem resolvedMediaItem =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    withObjCPtr resolvedMediaItem $ \raw_resolvedMediaItem ->
      sendClassMsg cls' (mkSelector "successWithResolvedMediaItem:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMediaItem :: Ptr ())] >>= retainedObject . castPtr

-- | @+ successesWithResolvedMediaItems:@
successesWithResolvedMediaItems :: IsNSArray resolvedMediaItems => resolvedMediaItems -> IO (Id NSArray)
successesWithResolvedMediaItems resolvedMediaItems =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    withObjCPtr resolvedMediaItems $ \raw_resolvedMediaItems ->
      sendClassMsg cls' (mkSelector "successesWithResolvedMediaItems:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMediaItems :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithMediaItemsToDisambiguate:@
disambiguationWithMediaItemsToDisambiguate :: IsNSArray mediaItemsToDisambiguate => mediaItemsToDisambiguate -> IO (Id INMediaItemResolutionResult)
disambiguationWithMediaItemsToDisambiguate mediaItemsToDisambiguate =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    withObjCPtr mediaItemsToDisambiguate $ \raw_mediaItemsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithMediaItemsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_mediaItemsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithMediaItemToConfirm:@
confirmationRequiredWithMediaItemToConfirm :: IsINMediaItem mediaItemToConfirm => mediaItemToConfirm -> IO (Id INMediaItemResolutionResult)
confirmationRequiredWithMediaItemToConfirm mediaItemToConfirm =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    withObjCPtr mediaItemToConfirm $ \raw_mediaItemToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithMediaItemToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_mediaItemToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMediaItem:@
successWithResolvedMediaItemSelector :: Selector
successWithResolvedMediaItemSelector = mkSelector "successWithResolvedMediaItem:"

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @disambiguationWithMediaItemsToDisambiguate:@
disambiguationWithMediaItemsToDisambiguateSelector :: Selector
disambiguationWithMediaItemsToDisambiguateSelector = mkSelector "disambiguationWithMediaItemsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithMediaItemToConfirm:@
confirmationRequiredWithMediaItemToConfirmSelector :: Selector
confirmationRequiredWithMediaItemToConfirmSelector = mkSelector "confirmationRequiredWithMediaItemToConfirm:"

