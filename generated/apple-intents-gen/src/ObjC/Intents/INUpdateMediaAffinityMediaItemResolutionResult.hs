{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUpdateMediaAffinityMediaItemResolutionResult@.
module ObjC.Intents.INUpdateMediaAffinityMediaItemResolutionResult
  ( INUpdateMediaAffinityMediaItemResolutionResult
  , IsINUpdateMediaAffinityMediaItemResolutionResult(..)
  , successesWithResolvedMediaItems
  , unsupportedForReason
  , initWithMediaItemResolutionResult
  , initWithMediaItemResolutionResultSelector
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INUpdateMediaAffinityMediaItemUnsupportedReason(INUpdateMediaAffinityMediaItemUnsupportedReason)
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonLoginRequired
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonSubscriptionRequired
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonUnsupportedMediaType
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonExplicitContentSettings
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonCellularDataSettings
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonRestrictedContent
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonServiceUnavailable
  , pattern INUpdateMediaAffinityMediaItemUnsupportedReasonRegionRestriction

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
    cls' <- getRequiredClass "INUpdateMediaAffinityMediaItemResolutionResult"
    sendClassMessage cls' successesWithResolvedMediaItemsSelector (toNSArray resolvedMediaItems)

-- | @+ unsupportedForReason:@
unsupportedForReason :: INUpdateMediaAffinityMediaItemUnsupportedReason -> IO (Id INUpdateMediaAffinityMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INUpdateMediaAffinityMediaItemResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINUpdateMediaAffinityMediaItemResolutionResult inUpdateMediaAffinityMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inUpdateMediaAffinityMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INUpdateMediaAffinityMediaItemResolutionResult)
initWithMediaItemResolutionResult inUpdateMediaAffinityMediaItemResolutionResult mediaItemResolutionResult =
  sendOwnedMessage inUpdateMediaAffinityMediaItemResolutionResult initWithMediaItemResolutionResultSelector (toINMediaItemResolutionResult mediaItemResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector '[Id NSArray] (Id NSArray)
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INUpdateMediaAffinityMediaItemUnsupportedReason] (Id INUpdateMediaAffinityMediaItemResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResultSelector :: Selector '[Id INMediaItemResolutionResult] (Id INUpdateMediaAffinityMediaItemResolutionResult)
initWithMediaItemResolutionResultSelector = mkSelector "initWithMediaItemResolutionResult:"

