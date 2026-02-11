{-# LANGUAGE PatternSynonyms #-}
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
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector
  , initWithMediaItemResolutionResultSelector

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
    cls' <- getRequiredClass "INAddMediaMediaItemResolutionResult"
    withObjCPtr resolvedMediaItems $ \raw_resolvedMediaItems ->
      sendClassMsg cls' (mkSelector "successesWithResolvedMediaItems:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMediaItems :: Ptr ())] >>= retainedObject . castPtr

-- | @+ unsupportedForReason:@
unsupportedForReason :: INAddMediaMediaItemUnsupportedReason -> IO (Id INAddMediaMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INAddMediaMediaItemResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINAddMediaMediaItemResolutionResult inAddMediaMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inAddMediaMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INAddMediaMediaItemResolutionResult)
initWithMediaItemResolutionResult inAddMediaMediaItemResolutionResult  mediaItemResolutionResult =
withObjCPtr mediaItemResolutionResult $ \raw_mediaItemResolutionResult ->
    sendMsg inAddMediaMediaItemResolutionResult (mkSelector "initWithMediaItemResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_mediaItemResolutionResult :: Ptr ())] >>= ownedObject . castPtr

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

