{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithMediaItemToConfirmSelector
  , disambiguationWithMediaItemsToDisambiguateSelector
  , successWithResolvedMediaItemSelector
  , successesWithResolvedMediaItemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedMediaItem:@
successWithResolvedMediaItem :: IsINMediaItem resolvedMediaItem => resolvedMediaItem -> IO (Id INMediaItemResolutionResult)
successWithResolvedMediaItem resolvedMediaItem =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    sendClassMessage cls' successWithResolvedMediaItemSelector (toINMediaItem resolvedMediaItem)

-- | @+ successesWithResolvedMediaItems:@
successesWithResolvedMediaItems :: IsNSArray resolvedMediaItems => resolvedMediaItems -> IO (Id NSArray)
successesWithResolvedMediaItems resolvedMediaItems =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    sendClassMessage cls' successesWithResolvedMediaItemsSelector (toNSArray resolvedMediaItems)

-- | @+ disambiguationWithMediaItemsToDisambiguate:@
disambiguationWithMediaItemsToDisambiguate :: IsNSArray mediaItemsToDisambiguate => mediaItemsToDisambiguate -> IO (Id INMediaItemResolutionResult)
disambiguationWithMediaItemsToDisambiguate mediaItemsToDisambiguate =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    sendClassMessage cls' disambiguationWithMediaItemsToDisambiguateSelector (toNSArray mediaItemsToDisambiguate)

-- | @+ confirmationRequiredWithMediaItemToConfirm:@
confirmationRequiredWithMediaItemToConfirm :: IsINMediaItem mediaItemToConfirm => mediaItemToConfirm -> IO (Id INMediaItemResolutionResult)
confirmationRequiredWithMediaItemToConfirm mediaItemToConfirm =
  do
    cls' <- getRequiredClass "INMediaItemResolutionResult"
    sendClassMessage cls' confirmationRequiredWithMediaItemToConfirmSelector (toINMediaItem mediaItemToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMediaItem:@
successWithResolvedMediaItemSelector :: Selector '[Id INMediaItem] (Id INMediaItemResolutionResult)
successWithResolvedMediaItemSelector = mkSelector "successWithResolvedMediaItem:"

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector '[Id NSArray] (Id NSArray)
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @disambiguationWithMediaItemsToDisambiguate:@
disambiguationWithMediaItemsToDisambiguateSelector :: Selector '[Id NSArray] (Id INMediaItemResolutionResult)
disambiguationWithMediaItemsToDisambiguateSelector = mkSelector "disambiguationWithMediaItemsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithMediaItemToConfirm:@
confirmationRequiredWithMediaItemToConfirmSelector :: Selector '[Id INMediaItem] (Id INMediaItemResolutionResult)
confirmationRequiredWithMediaItemToConfirmSelector = mkSelector "confirmationRequiredWithMediaItemToConfirm:"

