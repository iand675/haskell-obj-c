{-# LANGUAGE PatternSynonyms #-}
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
  , successesWithResolvedMediaItemsSelector
  , unsupportedForReasonSelector
  , initWithMediaItemResolutionResultSelector

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
    cls' <- getRequiredClass "INPlayMediaMediaItemResolutionResult"
    withObjCPtr resolvedMediaItems $ \raw_resolvedMediaItems ->
      sendClassMsg cls' (mkSelector "successesWithResolvedMediaItems:") (retPtr retVoid) [argPtr (castPtr raw_resolvedMediaItems :: Ptr ())] >>= retainedObject . castPtr

-- | @+ unsupportedForReason:@
unsupportedForReason :: INPlayMediaMediaItemUnsupportedReason -> IO (Id INPlayMediaMediaItemResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INPlayMediaMediaItemResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithMediaItemResolutionResult:@
initWithMediaItemResolutionResult :: (IsINPlayMediaMediaItemResolutionResult inPlayMediaMediaItemResolutionResult, IsINMediaItemResolutionResult mediaItemResolutionResult) => inPlayMediaMediaItemResolutionResult -> mediaItemResolutionResult -> IO (Id INPlayMediaMediaItemResolutionResult)
initWithMediaItemResolutionResult inPlayMediaMediaItemResolutionResult  mediaItemResolutionResult =
withObjCPtr mediaItemResolutionResult $ \raw_mediaItemResolutionResult ->
    sendMsg inPlayMediaMediaItemResolutionResult (mkSelector "initWithMediaItemResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_mediaItemResolutionResult :: Ptr ())] >>= ownedObject . castPtr

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

