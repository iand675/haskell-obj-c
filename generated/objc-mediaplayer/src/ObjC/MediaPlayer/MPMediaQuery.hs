{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaQuery@.
module ObjC.MediaPlayer.MPMediaQuery
  ( MPMediaQuery
  , IsMPMediaQuery(..)
  , initWithFilterPredicates
  , addFilterPredicate
  , removeFilterPredicate
  , albumsQuery
  , artistsQuery
  , songsQuery
  , playlistsQuery
  , podcastsQuery
  , audiobooksQuery
  , compilationsQuery
  , composersQuery
  , genresQuery
  , filterPredicates
  , setFilterPredicates
  , items
  , collections
  , groupingType
  , setGroupingType
  , itemSections
  , collectionSections
  , initWithFilterPredicatesSelector
  , addFilterPredicateSelector
  , removeFilterPredicateSelector
  , albumsQuerySelector
  , artistsQuerySelector
  , songsQuerySelector
  , playlistsQuerySelector
  , podcastsQuerySelector
  , audiobooksQuerySelector
  , compilationsQuerySelector
  , composersQuerySelector
  , genresQuerySelector
  , filterPredicatesSelector
  , setFilterPredicatesSelector
  , itemsSelector
  , collectionsSelector
  , groupingTypeSelector
  , setGroupingTypeSelector
  , itemSectionsSelector
  , collectionSectionsSelector

  -- * Enum types
  , MPMediaGrouping(MPMediaGrouping)
  , pattern MPMediaGroupingTitle
  , pattern MPMediaGroupingAlbum
  , pattern MPMediaGroupingArtist
  , pattern MPMediaGroupingAlbumArtist
  , pattern MPMediaGroupingComposer
  , pattern MPMediaGroupingGenre
  , pattern MPMediaGroupingPlaylist
  , pattern MPMediaGroupingPodcastTitle

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

-- | @- initWithFilterPredicates:@
initWithFilterPredicates :: (IsMPMediaQuery mpMediaQuery, IsNSSet filterPredicates) => mpMediaQuery -> filterPredicates -> IO (Id MPMediaQuery)
initWithFilterPredicates mpMediaQuery  filterPredicates =
withObjCPtr filterPredicates $ \raw_filterPredicates ->
    sendMsg mpMediaQuery (mkSelector "initWithFilterPredicates:") (retPtr retVoid) [argPtr (castPtr raw_filterPredicates :: Ptr ())] >>= ownedObject . castPtr

-- | @- addFilterPredicate:@
addFilterPredicate :: (IsMPMediaQuery mpMediaQuery, IsMPMediaPredicate predicate) => mpMediaQuery -> predicate -> IO ()
addFilterPredicate mpMediaQuery  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg mpMediaQuery (mkSelector "addFilterPredicate:") retVoid [argPtr (castPtr raw_predicate :: Ptr ())]

-- | @- removeFilterPredicate:@
removeFilterPredicate :: (IsMPMediaQuery mpMediaQuery, IsMPMediaPredicate predicate) => mpMediaQuery -> predicate -> IO ()
removeFilterPredicate mpMediaQuery  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg mpMediaQuery (mkSelector "removeFilterPredicate:") retVoid [argPtr (castPtr raw_predicate :: Ptr ())]

-- | @+ albumsQuery@
albumsQuery :: IO (Id MPMediaQuery)
albumsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "albumsQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ artistsQuery@
artistsQuery :: IO (Id MPMediaQuery)
artistsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "artistsQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ songsQuery@
songsQuery :: IO (Id MPMediaQuery)
songsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "songsQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ playlistsQuery@
playlistsQuery :: IO (Id MPMediaQuery)
playlistsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "playlistsQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ podcastsQuery@
podcastsQuery :: IO (Id MPMediaQuery)
podcastsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "podcastsQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ audiobooksQuery@
audiobooksQuery :: IO (Id MPMediaQuery)
audiobooksQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "audiobooksQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ compilationsQuery@
compilationsQuery :: IO (Id MPMediaQuery)
compilationsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "compilationsQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ composersQuery@
composersQuery :: IO (Id MPMediaQuery)
composersQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "composersQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ genresQuery@
genresQuery :: IO (Id MPMediaQuery)
genresQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMsg cls' (mkSelector "genresQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- filterPredicates@
filterPredicates :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSSet)
filterPredicates mpMediaQuery  =
  sendMsg mpMediaQuery (mkSelector "filterPredicates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilterPredicates:@
setFilterPredicates :: (IsMPMediaQuery mpMediaQuery, IsNSSet value) => mpMediaQuery -> value -> IO ()
setFilterPredicates mpMediaQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMediaQuery (mkSelector "setFilterPredicates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- items@
items :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
items mpMediaQuery  =
  sendMsg mpMediaQuery (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collections@
collections :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
collections mpMediaQuery  =
  sendMsg mpMediaQuery (mkSelector "collections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupingType@
groupingType :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO MPMediaGrouping
groupingType mpMediaQuery  =
  fmap (coerce :: CLong -> MPMediaGrouping) $ sendMsg mpMediaQuery (mkSelector "groupingType") retCLong []

-- | @- setGroupingType:@
setGroupingType :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> MPMediaGrouping -> IO ()
setGroupingType mpMediaQuery  value =
  sendMsg mpMediaQuery (mkSelector "setGroupingType:") retVoid [argCLong (coerce value)]

-- | @- itemSections@
itemSections :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
itemSections mpMediaQuery  =
  sendMsg mpMediaQuery (mkSelector "itemSections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collectionSections@
collectionSections :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
collectionSections mpMediaQuery  =
  sendMsg mpMediaQuery (mkSelector "collectionSections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFilterPredicates:@
initWithFilterPredicatesSelector :: Selector
initWithFilterPredicatesSelector = mkSelector "initWithFilterPredicates:"

-- | @Selector@ for @addFilterPredicate:@
addFilterPredicateSelector :: Selector
addFilterPredicateSelector = mkSelector "addFilterPredicate:"

-- | @Selector@ for @removeFilterPredicate:@
removeFilterPredicateSelector :: Selector
removeFilterPredicateSelector = mkSelector "removeFilterPredicate:"

-- | @Selector@ for @albumsQuery@
albumsQuerySelector :: Selector
albumsQuerySelector = mkSelector "albumsQuery"

-- | @Selector@ for @artistsQuery@
artistsQuerySelector :: Selector
artistsQuerySelector = mkSelector "artistsQuery"

-- | @Selector@ for @songsQuery@
songsQuerySelector :: Selector
songsQuerySelector = mkSelector "songsQuery"

-- | @Selector@ for @playlistsQuery@
playlistsQuerySelector :: Selector
playlistsQuerySelector = mkSelector "playlistsQuery"

-- | @Selector@ for @podcastsQuery@
podcastsQuerySelector :: Selector
podcastsQuerySelector = mkSelector "podcastsQuery"

-- | @Selector@ for @audiobooksQuery@
audiobooksQuerySelector :: Selector
audiobooksQuerySelector = mkSelector "audiobooksQuery"

-- | @Selector@ for @compilationsQuery@
compilationsQuerySelector :: Selector
compilationsQuerySelector = mkSelector "compilationsQuery"

-- | @Selector@ for @composersQuery@
composersQuerySelector :: Selector
composersQuerySelector = mkSelector "composersQuery"

-- | @Selector@ for @genresQuery@
genresQuerySelector :: Selector
genresQuerySelector = mkSelector "genresQuery"

-- | @Selector@ for @filterPredicates@
filterPredicatesSelector :: Selector
filterPredicatesSelector = mkSelector "filterPredicates"

-- | @Selector@ for @setFilterPredicates:@
setFilterPredicatesSelector :: Selector
setFilterPredicatesSelector = mkSelector "setFilterPredicates:"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @collections@
collectionsSelector :: Selector
collectionsSelector = mkSelector "collections"

-- | @Selector@ for @groupingType@
groupingTypeSelector :: Selector
groupingTypeSelector = mkSelector "groupingType"

-- | @Selector@ for @setGroupingType:@
setGroupingTypeSelector :: Selector
setGroupingTypeSelector = mkSelector "setGroupingType:"

-- | @Selector@ for @itemSections@
itemSectionsSelector :: Selector
itemSectionsSelector = mkSelector "itemSections"

-- | @Selector@ for @collectionSections@
collectionSectionsSelector :: Selector
collectionSectionsSelector = mkSelector "collectionSections"

