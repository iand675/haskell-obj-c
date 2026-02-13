{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaMediaItemResolutionResult@.
module ObjC.Intents.INAddMediaMediaItemResolutionResult
  ( INAddMediaMediaItemResolutionResult
  , IsINAddMediaMediaItemResolutionResult(..)
  , successesWithResolvedMediaItems
  , unsupportedForReason
  , initWithMediaItemResolutionResult
  , initWithMediaItemResolutionResultSelector
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INAddMediaMediaItemUnsupportedReason(INAddMediaMediaItemUnsupportedReason)
  , pattern INAddMediaMediaItemUnsupportedReasonLoginRequired
  , pattern INAddMediaMediaItemUnsupportedReasonSubscriptionRequired
  , pattern INAddMediaMediaItemUnsupportedReasonUnsupportedMediaType
  , pattern INAddMediaMediaItemUnsupportedReasonExplicitContentSettings
  , pattern INAddMediaMediaItemUnsupportedReasonCellularDataSettings
  , pattern INAddMediaMediaItemUnsupportedReasonRestrictedContent
  , pattern INAddMediaMediaItemUnsupportedReasonServiceUnavailable
  , pattern INAddMediaMediaItemUnsupportedReasonRegionRestriction

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ successesWithResolvedMediaItems:@
successesWithResolvedMediaItems :: IsNSArray resolvedMediaItems => resolvedMediaItems -> IO (Id NSArray)
successesWithResolvedMediaItems resolvedMediaItems =
  do
    cls' <- getRequiredClass "INAddMediaMediaItemResolutionResult"
    sendClassMessage cls' successesWithResolvedMediaItemsSelector (toNSArray resolvedMediaItems)

-- | @+ unsupportedForReason:@
unsupportedForReason :: INAddMediaMediaItemUnsupportedReason -> IO (Id INAddMediaMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INAddMediaMediaItemResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINAddMediaMediaItemResolutionResult inAddMediaMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inAddMediaMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INAddMediaMediaItemResolutionResult)
initWithMediaItemResolutionResult inAddMediaMediaItemResolutionResult mediaItemResolutionResult =
  sendOwnedMessage inAddMediaMediaItemResolutionResult initWithMediaItemResolutionResultSelector (toINMediaItemResolutionResult mediaItemResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector '[Id NSArray] (Id NSArray)
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INAddMediaMediaItemUnsupportedReason] (Id INAddMediaMediaItemResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResultSelector :: Selector '[Id INMediaItemResolutionResult] (Id INAddMediaMediaItemResolutionResult)
initWithMediaItemResolutionResultSelector = mkSelector "initWithMediaItemResolutionResult:"

