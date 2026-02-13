{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForMediaMediaItemResolutionResult@.
module ObjC.Intents.INSearchForMediaMediaItemResolutionResult
  ( INSearchForMediaMediaItemResolutionResult
  , IsINSearchForMediaMediaItemResolutionResult(..)
  , successesWithResolvedMediaItems
  , unsupportedForReason
  , initWithMediaItemResolutionResult
  , initWithMediaItemResolutionResultSelector
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INSearchForMediaMediaItemUnsupportedReason(INSearchForMediaMediaItemUnsupportedReason)
  , pattern INSearchForMediaMediaItemUnsupportedReasonLoginRequired
  , pattern INSearchForMediaMediaItemUnsupportedReasonSubscriptionRequired
  , pattern INSearchForMediaMediaItemUnsupportedReasonUnsupportedMediaType
  , pattern INSearchForMediaMediaItemUnsupportedReasonExplicitContentSettings
  , pattern INSearchForMediaMediaItemUnsupportedReasonCellularDataSettings
  , pattern INSearchForMediaMediaItemUnsupportedReasonRestrictedContent
  , pattern INSearchForMediaMediaItemUnsupportedReasonServiceUnavailable
  , pattern INSearchForMediaMediaItemUnsupportedReasonRegionRestriction

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
    cls' <- getRequiredClass "INSearchForMediaMediaItemResolutionResult"
    sendClassMessage cls' successesWithResolvedMediaItemsSelector (toNSArray resolvedMediaItems)

-- | @+ unsupportedForReason:@
unsupportedForReason :: INSearchForMediaMediaItemUnsupportedReason -> IO (Id INSearchForMediaMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSearchForMediaMediaItemResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINSearchForMediaMediaItemResolutionResult inSearchForMediaMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inSearchForMediaMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INSearchForMediaMediaItemResolutionResult)
initWithMediaItemResolutionResult inSearchForMediaMediaItemResolutionResult mediaItemResolutionResult =
  sendOwnedMessage inSearchForMediaMediaItemResolutionResult initWithMediaItemResolutionResultSelector (toINMediaItemResolutionResult mediaItemResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector '[Id NSArray] (Id NSArray)
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INSearchForMediaMediaItemUnsupportedReason] (Id INSearchForMediaMediaItemResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResultSelector :: Selector '[Id INMediaItemResolutionResult] (Id INSearchForMediaMediaItemResolutionResult)
initWithMediaItemResolutionResultSelector = mkSelector "initWithMediaItemResolutionResult:"

