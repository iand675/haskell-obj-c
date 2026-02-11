{-# LANGUAGE PatternSynonyms #-}
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
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector
  , initWithMediaItemResolutionResultSelector

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
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ successesWithResolvedMediaItems:@
successesWithResolvedMediaItems :: IsNSArray resolvedMediaItems => resolvedMediaItems -> IO (Id NSArray)
successesWithResolvedMediaItems resolvedMediaItems =
  do
    cls' <- getRequiredClass "INUpdateMediaAffinityMediaItemResolutionResult"
    withObjCPtr resolvedMediaItems $ \raw_resolvedMediaItems ->
      sendClassMsg cls' (mkSelector "successesWithResolvedMediaItems:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMediaItems :: Ptr ())] >>= retainedObject . castPtr

-- | @+ unsupportedForReason:@
unsupportedForReason :: INUpdateMediaAffinityMediaItemUnsupportedReason -> IO (Id INUpdateMediaAffinityMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INUpdateMediaAffinityMediaItemResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINUpdateMediaAffinityMediaItemResolutionResult inUpdateMediaAffinityMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inUpdateMediaAffinityMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INUpdateMediaAffinityMediaItemResolutionResult)
initWithMediaItemResolutionResult inUpdateMediaAffinityMediaItemResolutionResult  mediaItemResolutionResult =
withObjCPtr mediaItemResolutionResult $ \raw_mediaItemResolutionResult ->
    sendMsg inUpdateMediaAffinityMediaItemResolutionResult (mkSelector "initWithMediaItemResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_mediaItemResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successesWithResolvedMediaItems:@
successesWithResolvedMediaItemsSelector :: Selector
successesWithResolvedMediaItemsSelector = mkSelector "successesWithResolvedMediaItems:"

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResultSelector :: Selector
initWithMediaItemResolutionResultSelector = mkSelector "initWithMediaItemResolutionResult:"

