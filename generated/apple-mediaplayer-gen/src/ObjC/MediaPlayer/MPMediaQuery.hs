{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addFilterPredicateSelector
  , albumsQuerySelector
  , artistsQuerySelector
  , audiobooksQuerySelector
  , collectionSectionsSelector
  , collectionsSelector
  , compilationsQuerySelector
  , composersQuerySelector
  , filterPredicatesSelector
  , genresQuerySelector
  , groupingTypeSelector
  , initWithFilterPredicatesSelector
  , itemSectionsSelector
  , itemsSelector
  , playlistsQuerySelector
  , podcastsQuerySelector
  , removeFilterPredicateSelector
  , setFilterPredicatesSelector
  , setGroupingTypeSelector
  , songsQuerySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFilterPredicates:@
initWithFilterPredicates :: (IsMPMediaQuery mpMediaQuery, IsNSSet filterPredicates) => mpMediaQuery -> filterPredicates -> IO (Id MPMediaQuery)
initWithFilterPredicates mpMediaQuery filterPredicates =
  sendOwnedMessage mpMediaQuery initWithFilterPredicatesSelector (toNSSet filterPredicates)

-- | @- addFilterPredicate:@
addFilterPredicate :: (IsMPMediaQuery mpMediaQuery, IsMPMediaPredicate predicate) => mpMediaQuery -> predicate -> IO ()
addFilterPredicate mpMediaQuery predicate =
  sendMessage mpMediaQuery addFilterPredicateSelector (toMPMediaPredicate predicate)

-- | @- removeFilterPredicate:@
removeFilterPredicate :: (IsMPMediaQuery mpMediaQuery, IsMPMediaPredicate predicate) => mpMediaQuery -> predicate -> IO ()
removeFilterPredicate mpMediaQuery predicate =
  sendMessage mpMediaQuery removeFilterPredicateSelector (toMPMediaPredicate predicate)

-- | @+ albumsQuery@
albumsQuery :: IO (Id MPMediaQuery)
albumsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' albumsQuerySelector

-- | @+ artistsQuery@
artistsQuery :: IO (Id MPMediaQuery)
artistsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' artistsQuerySelector

-- | @+ songsQuery@
songsQuery :: IO (Id MPMediaQuery)
songsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' songsQuerySelector

-- | @+ playlistsQuery@
playlistsQuery :: IO (Id MPMediaQuery)
playlistsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' playlistsQuerySelector

-- | @+ podcastsQuery@
podcastsQuery :: IO (Id MPMediaQuery)
podcastsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' podcastsQuerySelector

-- | @+ audiobooksQuery@
audiobooksQuery :: IO (Id MPMediaQuery)
audiobooksQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' audiobooksQuerySelector

-- | @+ compilationsQuery@
compilationsQuery :: IO (Id MPMediaQuery)
compilationsQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' compilationsQuerySelector

-- | @+ composersQuery@
composersQuery :: IO (Id MPMediaQuery)
composersQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' composersQuerySelector

-- | @+ genresQuery@
genresQuery :: IO (Id MPMediaQuery)
genresQuery  =
  do
    cls' <- getRequiredClass "MPMediaQuery"
    sendClassMessage cls' genresQuerySelector

-- | @- filterPredicates@
filterPredicates :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSSet)
filterPredicates mpMediaQuery =
  sendMessage mpMediaQuery filterPredicatesSelector

-- | @- setFilterPredicates:@
setFilterPredicates :: (IsMPMediaQuery mpMediaQuery, IsNSSet value) => mpMediaQuery -> value -> IO ()
setFilterPredicates mpMediaQuery value =
  sendMessage mpMediaQuery setFilterPredicatesSelector (toNSSet value)

-- | @- items@
items :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
items mpMediaQuery =
  sendMessage mpMediaQuery itemsSelector

-- | @- collections@
collections :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
collections mpMediaQuery =
  sendMessage mpMediaQuery collectionsSelector

-- | @- groupingType@
groupingType :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO MPMediaGrouping
groupingType mpMediaQuery =
  sendMessage mpMediaQuery groupingTypeSelector

-- | @- setGroupingType:@
setGroupingType :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> MPMediaGrouping -> IO ()
setGroupingType mpMediaQuery value =
  sendMessage mpMediaQuery setGroupingTypeSelector value

-- | @- itemSections@
itemSections :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
itemSections mpMediaQuery =
  sendMessage mpMediaQuery itemSectionsSelector

-- | @- collectionSections@
collectionSections :: IsMPMediaQuery mpMediaQuery => mpMediaQuery -> IO (Id NSArray)
collectionSections mpMediaQuery =
  sendMessage mpMediaQuery collectionSectionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFilterPredicates:@
initWithFilterPredicatesSelector :: Selector '[Id NSSet] (Id MPMediaQuery)
initWithFilterPredicatesSelector = mkSelector "initWithFilterPredicates:"

-- | @Selector@ for @addFilterPredicate:@
addFilterPredicateSelector :: Selector '[Id MPMediaPredicate] ()
addFilterPredicateSelector = mkSelector "addFilterPredicate:"

-- | @Selector@ for @removeFilterPredicate:@
removeFilterPredicateSelector :: Selector '[Id MPMediaPredicate] ()
removeFilterPredicateSelector = mkSelector "removeFilterPredicate:"

-- | @Selector@ for @albumsQuery@
albumsQuerySelector :: Selector '[] (Id MPMediaQuery)
albumsQuerySelector = mkSelector "albumsQuery"

-- | @Selector@ for @artistsQuery@
artistsQuerySelector :: Selector '[] (Id MPMediaQuery)
artistsQuerySelector = mkSelector "artistsQuery"

-- | @Selector@ for @songsQuery@
songsQuerySelector :: Selector '[] (Id MPMediaQuery)
songsQuerySelector = mkSelector "songsQuery"

-- | @Selector@ for @playlistsQuery@
playlistsQuerySelector :: Selector '[] (Id MPMediaQuery)
playlistsQuerySelector = mkSelector "playlistsQuery"

-- | @Selector@ for @podcastsQuery@
podcastsQuerySelector :: Selector '[] (Id MPMediaQuery)
podcastsQuerySelector = mkSelector "podcastsQuery"

-- | @Selector@ for @audiobooksQuery@
audiobooksQuerySelector :: Selector '[] (Id MPMediaQuery)
audiobooksQuerySelector = mkSelector "audiobooksQuery"

-- | @Selector@ for @compilationsQuery@
compilationsQuerySelector :: Selector '[] (Id MPMediaQuery)
compilationsQuerySelector = mkSelector "compilationsQuery"

-- | @Selector@ for @composersQuery@
composersQuerySelector :: Selector '[] (Id MPMediaQuery)
composersQuerySelector = mkSelector "composersQuery"

-- | @Selector@ for @genresQuery@
genresQuerySelector :: Selector '[] (Id MPMediaQuery)
genresQuerySelector = mkSelector "genresQuery"

-- | @Selector@ for @filterPredicates@
filterPredicatesSelector :: Selector '[] (Id NSSet)
filterPredicatesSelector = mkSelector "filterPredicates"

-- | @Selector@ for @setFilterPredicates:@
setFilterPredicatesSelector :: Selector '[Id NSSet] ()
setFilterPredicatesSelector = mkSelector "setFilterPredicates:"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @collections@
collectionsSelector :: Selector '[] (Id NSArray)
collectionsSelector = mkSelector "collections"

-- | @Selector@ for @groupingType@
groupingTypeSelector :: Selector '[] MPMediaGrouping
groupingTypeSelector = mkSelector "groupingType"

-- | @Selector@ for @setGroupingType:@
setGroupingTypeSelector :: Selector '[MPMediaGrouping] ()
setGroupingTypeSelector = mkSelector "setGroupingType:"

-- | @Selector@ for @itemSections@
itemSectionsSelector :: Selector '[] (Id NSArray)
itemSectionsSelector = mkSelector "itemSections"

-- | @Selector@ for @collectionSections@
collectionSectionsSelector :: Selector '[] (Id NSArray)
collectionSectionsSelector = mkSelector "collectionSections"

