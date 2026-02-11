{-# LANGUAGE PatternSynonyms #-}
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
  , initWithItemsSelector
  , itemsSelector
  , representativeItemSelector
  , countSelector
  , mediaTypesSelector

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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ collectionWithItems:@
collectionWithItems :: IsNSArray items => items -> IO (Id MPMediaItemCollection)
collectionWithItems items =
  do
    cls' <- getRequiredClass "MPMediaItemCollection"
    withObjCPtr items $ \raw_items ->
      sendClassMsg cls' (mkSelector "collectionWithItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithItems:@
initWithItems :: (IsMPMediaItemCollection mpMediaItemCollection, IsNSArray items) => mpMediaItemCollection -> items -> IO (Id MPMediaItemCollection)
initWithItems mpMediaItemCollection  items =
withObjCPtr items $ \raw_items ->
    sendMsg mpMediaItemCollection (mkSelector "initWithItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= ownedObject . castPtr

-- | @- items@
items :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO (Id NSArray)
items mpMediaItemCollection  =
  sendMsg mpMediaItemCollection (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- representativeItem@
representativeItem :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO (Id MPMediaItem)
representativeItem mpMediaItemCollection  =
  sendMsg mpMediaItemCollection (mkSelector "representativeItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- count@
count :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO CULong
count mpMediaItemCollection  =
  sendMsg mpMediaItemCollection (mkSelector "count") retCULong []

-- | @- mediaTypes@
mediaTypes :: IsMPMediaItemCollection mpMediaItemCollection => mpMediaItemCollection -> IO MPMediaType
mediaTypes mpMediaItemCollection  =
  fmap (coerce :: CULong -> MPMediaType) $ sendMsg mpMediaItemCollection (mkSelector "mediaTypes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collectionWithItems:@
collectionWithItemsSelector :: Selector
collectionWithItemsSelector = mkSelector "collectionWithItems:"

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @representativeItem@
representativeItemSelector :: Selector
representativeItemSelector = mkSelector "representativeItem"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @mediaTypes@
mediaTypesSelector :: Selector
mediaTypesSelector = mkSelector "mediaTypes"

