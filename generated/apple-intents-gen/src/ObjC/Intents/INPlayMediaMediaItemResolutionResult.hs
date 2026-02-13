{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlayMediaMediaItemResolutionResult@.
module ObjC.Intents.INPlayMediaMediaItemResolutionResult
  ( INPlayMediaMediaItemResolutionResult
  , IsINPlayMediaMediaItemResolutionResult(..)
  , successesWithResolvedMediaItems
  , unsupportedForReason
  , initWithMediaItemResolutionResult
  , initWithMediaItemResolutionResultSelector
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INPlayMediaMediaItemUnsupportedReason(INPlayMediaMediaItemUnsupportedReason)
  , pattern INPlayMediaMediaItemUnsupportedReasonLoginRequired
  , pattern INPlayMediaMediaItemUnsupportedReasonSubscriptionRequired
  , pattern INPlayMediaMediaItemUnsupportedReasonUnsupportedMediaType
  , pattern INPlayMediaMediaItemUnsupportedReasonExplicitContentSettings
  , pattern INPlayMediaMediaItemUnsupportedReasonCellularDataSettings
  , pattern INPlayMediaMediaItemUnsupportedReasonRestrictedContent
  , pattern INPlayMediaMediaItemUnsupportedReasonServiceUnavailable
  , pattern INPlayMediaMediaItemUnsupportedReasonRegionRestriction

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
    cls' <- getRequiredClass "INPlayMediaMediaItemResolutionResult"
    sendClassMessage cls' successesWithResolvedMediaItemsSelector (toNSArray resolvedMediaItems)

-- | @+ unsupportedForReason:@
unsupportedForReason :: INPlayMediaMediaItemUnsupportedReason -> IO (Id INPlayMediaMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INPlayMediaMediaItemResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINPlayMediaMediaItemResolutionResult inPlayMediaMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inPlayMediaMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INPlayMediaMediaItemResolutionResult)
initWithMediaItemResolutionResult inPlayMediaMediaItemResolutionResult mediaItemResolutionResult =
  sendOwnedMessage inPlayMediaMediaItemResolutionResult initWithMediaItemResolutionResultSelector (toINMediaItemResolutionResult mediaItemResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector '[Id NSArray] (Id NSArray)
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INPlayMediaMediaItemUnsupportedReason] (Id INPlayMediaMediaItemResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResultSelector :: Selector '[Id INMediaItemResolutionResult] (Id INPlayMediaMediaItemResolutionResult)
initWithMediaItemResolutionResultSelector = mkSelector "initWithMediaItemResolutionResult:"

