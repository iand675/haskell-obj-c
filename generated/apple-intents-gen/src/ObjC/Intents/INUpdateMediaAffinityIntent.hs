{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUpdateMediaAffinityIntent@.
module ObjC.Intents.INUpdateMediaAffinityIntent
  ( INUpdateMediaAffinityIntent
  , IsINUpdateMediaAffinityIntent(..)
  , initWithMediaItems_mediaSearch_affinityType
  , mediaItems
  , mediaSearch
  , affinityType
  , affinityTypeSelector
  , initWithMediaItems_mediaSearch_affinityTypeSelector
  , mediaItemsSelector
  , mediaSearchSelector

  -- * Enum types
  , INMediaAffinityType(INMediaAffinityType)
  , pattern INMediaAffinityTypeUnknown
  , pattern INMediaAffinityTypeLike
  , pattern INMediaAffinityTypeDislike

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

-- | @- initWithMediaItems:mediaSearch:affinityType:@
initWithMediaItems_mediaSearch_affinityType :: (IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent, IsNSArray mediaItems, IsINMediaSearch mediaSearch) => inUpdateMediaAffinityIntent -> mediaItems -> mediaSearch -> INMediaAffinityType -> IO (Id INUpdateMediaAffinityIntent)
initWithMediaItems_mediaSearch_affinityType inUpdateMediaAffinityIntent mediaItems mediaSearch affinityType =
  sendOwnedMessage inUpdateMediaAffinityIntent initWithMediaItems_mediaSearch_affinityTypeSelector (toNSArray mediaItems) (toINMediaSearch mediaSearch) affinityType

-- | @- mediaItems@
mediaItems :: IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent => inUpdateMediaAffinityIntent -> IO (Id NSArray)
mediaItems inUpdateMediaAffinityIntent =
  sendMessage inUpdateMediaAffinityIntent mediaItemsSelector

-- | @- mediaSearch@
mediaSearch :: IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent => inUpdateMediaAffinityIntent -> IO (Id INMediaSearch)
mediaSearch inUpdateMediaAffinityIntent =
  sendMessage inUpdateMediaAffinityIntent mediaSearchSelector

-- | @- affinityType@
affinityType :: IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent => inUpdateMediaAffinityIntent -> IO INMediaAffinityType
affinityType inUpdateMediaAffinityIntent =
  sendMessage inUpdateMediaAffinityIntent affinityTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaSearch:affinityType:@
initWithMediaItems_mediaSearch_affinityTypeSelector :: Selector '[Id NSArray, Id INMediaSearch, INMediaAffinityType] (Id INUpdateMediaAffinityIntent)
initWithMediaItems_mediaSearch_affinityTypeSelector = mkSelector "initWithMediaItems:mediaSearch:affinityType:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector '[] (Id NSArray)
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector '[] (Id INMediaSearch)
mediaSearchSelector = mkSelector "mediaSearch"

-- | @Selector@ for @affinityType@
affinityTypeSelector :: Selector '[] INMediaAffinityType
affinityTypeSelector = mkSelector "affinityType"

