{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForMediaIntent@.
module ObjC.Intents.INSearchForMediaIntent
  ( INSearchForMediaIntent
  , IsINSearchForMediaIntent(..)
  , initWithMediaItems_mediaSearch
  , mediaItems
  , mediaSearch
  , initWithMediaItems_mediaSearchSelector
  , mediaItemsSelector
  , mediaSearchSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMediaItems:mediaSearch:@
initWithMediaItems_mediaSearch :: (IsINSearchForMediaIntent inSearchForMediaIntent, IsNSArray mediaItems, IsINMediaSearch mediaSearch) => inSearchForMediaIntent -> mediaItems -> mediaSearch -> IO (Id INSearchForMediaIntent)
initWithMediaItems_mediaSearch inSearchForMediaIntent mediaItems mediaSearch =
  sendOwnedMessage inSearchForMediaIntent initWithMediaItems_mediaSearchSelector (toNSArray mediaItems) (toINMediaSearch mediaSearch)

-- | @- mediaItems@
mediaItems :: IsINSearchForMediaIntent inSearchForMediaIntent => inSearchForMediaIntent -> IO (Id NSArray)
mediaItems inSearchForMediaIntent =
  sendMessage inSearchForMediaIntent mediaItemsSelector

-- | @- mediaSearch@
mediaSearch :: IsINSearchForMediaIntent inSearchForMediaIntent => inSearchForMediaIntent -> IO (Id INMediaSearch)
mediaSearch inSearchForMediaIntent =
  sendMessage inSearchForMediaIntent mediaSearchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaSearch:@
initWithMediaItems_mediaSearchSelector :: Selector '[Id NSArray, Id INMediaSearch] (Id INSearchForMediaIntent)
initWithMediaItems_mediaSearchSelector = mkSelector "initWithMediaItems:mediaSearch:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector '[] (Id NSArray)
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector '[] (Id INMediaSearch)
mediaSearchSelector = mkSelector "mediaSearch"

