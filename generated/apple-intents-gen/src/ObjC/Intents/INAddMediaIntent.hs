{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaIntent@.
module ObjC.Intents.INAddMediaIntent
  ( INAddMediaIntent
  , IsINAddMediaIntent(..)
  , initWithMediaItems_mediaSearch_mediaDestination
  , mediaItems
  , mediaSearch
  , mediaDestination
  , initWithMediaItems_mediaSearch_mediaDestinationSelector
  , mediaDestinationSelector
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

-- | @- initWithMediaItems:mediaSearch:mediaDestination:@
initWithMediaItems_mediaSearch_mediaDestination :: (IsINAddMediaIntent inAddMediaIntent, IsNSArray mediaItems, IsINMediaSearch mediaSearch, IsINMediaDestination mediaDestination) => inAddMediaIntent -> mediaItems -> mediaSearch -> mediaDestination -> IO (Id INAddMediaIntent)
initWithMediaItems_mediaSearch_mediaDestination inAddMediaIntent mediaItems mediaSearch mediaDestination =
  sendOwnedMessage inAddMediaIntent initWithMediaItems_mediaSearch_mediaDestinationSelector (toNSArray mediaItems) (toINMediaSearch mediaSearch) (toINMediaDestination mediaDestination)

-- | @- mediaItems@
mediaItems :: IsINAddMediaIntent inAddMediaIntent => inAddMediaIntent -> IO (Id NSArray)
mediaItems inAddMediaIntent =
  sendMessage inAddMediaIntent mediaItemsSelector

-- | @- mediaSearch@
mediaSearch :: IsINAddMediaIntent inAddMediaIntent => inAddMediaIntent -> IO (Id INMediaSearch)
mediaSearch inAddMediaIntent =
  sendMessage inAddMediaIntent mediaSearchSelector

-- | @- mediaDestination@
mediaDestination :: IsINAddMediaIntent inAddMediaIntent => inAddMediaIntent -> IO (Id INMediaDestination)
mediaDestination inAddMediaIntent =
  sendMessage inAddMediaIntent mediaDestinationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaSearch:mediaDestination:@
initWithMediaItems_mediaSearch_mediaDestinationSelector :: Selector '[Id NSArray, Id INMediaSearch, Id INMediaDestination] (Id INAddMediaIntent)
initWithMediaItems_mediaSearch_mediaDestinationSelector = mkSelector "initWithMediaItems:mediaSearch:mediaDestination:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector '[] (Id NSArray)
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector '[] (Id INMediaSearch)
mediaSearchSelector = mkSelector "mediaSearch"

-- | @Selector@ for @mediaDestination@
mediaDestinationSelector :: Selector '[] (Id INMediaDestination)
mediaDestinationSelector = mkSelector "mediaDestination"

