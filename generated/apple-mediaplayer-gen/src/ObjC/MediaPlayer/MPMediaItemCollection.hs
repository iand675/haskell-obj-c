{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaItemCollection@.
module ObjC.MediaPlayer.MPMediaItemCollection
  ( MPMediaItemCollection
  , IsMPMediaItemCollection(..)
  , collectionWithItems
  , initWithItems
  , items
  , representativeItem
  , count
  , mediaTypes
  , collectionWithItemsSelector
  , countSelector
  , initWithItemsSelector
  , itemsSelector
  , mediaTypesSelector
  , representativeItemSelector

  -- * Enum types
  , MPMediaType(MPMediaType)
  , pattern MPMediaTypeMusic
  , pattern MPMediaTypePodcast
  , pattern MPMediaTypeAudioBook
  , pattern MPMediaTypeAudioITunesU
  , pattern MPMediaTypeAnyAudio
  , pattern MPMediaTypeMovie
  , pattern MPMediaTypeTVShow
  , pattern MPMediaTypeVideoPodcast
  , pattern MPMediaTypeMusicVideo
  , pattern MPMediaTypeVideoITunesU
  , pattern MPMediaTypeHomeVideo
  , pattern MPMediaTypeAnyVideo
  , pattern MPMediaTypeAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ collectionWithItems:@
collectionWithItems :: IsNSArray items => items -> IO (Id MPMediaItemCollection)
collectionWithItems items =
  do
    cls' <- getRequiredClass "MPMediaItemCollection"
    sendClassMessage cls' collectionWithItemsSelector (toNSArray items)

-- | @- initWithItems:@
initWithItems :: (IsMPMediaItemCollection mpMediaItemCollection, IsNSArray items) => mpMediaItemCollection -> items -> IO (Id MPMediaItemCollection)
initWithItems mpMediaItemCollection items =
  sendOwnedMessage mpMediaItemCollection initWithItemsSelector (toNSArray items)

-- | @- items@
items :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO (Id NSArray)
items mpMediaItemCollection =
  sendMessage mpMediaItemCollection itemsSelector

-- | @- representativeItem@
representativeItem :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO (Id MPMediaItem)
representativeItem mpMediaItemCollection =
  sendMessage mpMediaItemCollection representativeItemSelector

-- | @- count@
count :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO CULong
count mpMediaItemCollection =
  sendMessage mpMediaItemCollection countSelector

-- | @- mediaTypes@
mediaTypes :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO MPMediaType
mediaTypes mpMediaItemCollection =
  sendMessage mpMediaItemCollection mediaTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collectionWithItems:@
collectionWithItemsSelector :: Selector '[Id NSArray] (Id MPMediaItemCollection)
collectionWithItemsSelector = mkSelector "collectionWithItems:"

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector '[Id NSArray] (Id MPMediaItemCollection)
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @representativeItem@
representativeItemSelector :: Selector '[] (Id MPMediaItem)
representativeItemSelector = mkSelector "representativeItem"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @mediaTypes@
mediaTypesSelector :: Selector '[] MPMediaType
mediaTypesSelector = mkSelector "mediaTypes"

