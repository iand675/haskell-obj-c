{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaItem@.
module ObjC.MediaPlayer.MPMediaItem
  ( MPMediaItem
  , IsMPMediaItem(..)
  , persistentIDPropertyForGroupingType
  , titlePropertyForGroupingType
  , persistentID
  , mediaType
  , title
  , albumTitle
  , albumPersistentID
  , artist
  , artistPersistentID
  , albumArtist
  , albumArtistPersistentID
  , genre
  , genrePersistentID
  , composer
  , composerPersistentID
  , playbackDuration
  , albumTrackNumber
  , albumTrackCount
  , discNumber
  , discCount
  , artwork
  , explicitItem
  , lyrics
  , compilation
  , releaseDate
  , beatsPerMinute
  , comments
  , assetURL
  , cloudItem
  , protectedAsset
  , podcastTitle
  , podcastPersistentID
  , playCount
  , skipCount
  , rating
  , lastPlayedDate
  , userGrouping
  , bookmarkTime
  , dateAdded
  , playbackStoreID
  , preorder
  , persistentIDPropertyForGroupingTypeSelector
  , titlePropertyForGroupingTypeSelector
  , persistentIDSelector
  , mediaTypeSelector
  , titleSelector
  , albumTitleSelector
  , albumPersistentIDSelector
  , artistSelector
  , artistPersistentIDSelector
  , albumArtistSelector
  , albumArtistPersistentIDSelector
  , genreSelector
  , genrePersistentIDSelector
  , composerSelector
  , composerPersistentIDSelector
  , playbackDurationSelector
  , albumTrackNumberSelector
  , albumTrackCountSelector
  , discNumberSelector
  , discCountSelector
  , artworkSelector
  , explicitItemSelector
  , lyricsSelector
  , compilationSelector
  , releaseDateSelector
  , beatsPerMinuteSelector
  , commentsSelector
  , assetURLSelector
  , cloudItemSelector
  , protectedAssetSelector
  , podcastTitleSelector
  , podcastPersistentIDSelector
  , playCountSelector
  , skipCountSelector
  , ratingSelector
  , lastPlayedDateSelector
  , userGroupingSelector
  , bookmarkTimeSelector
  , dateAddedSelector
  , playbackStoreIDSelector
  , preorderSelector

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

-- | @+ persistentIDPropertyForGroupingType:@
persistentIDPropertyForGroupingType :: MPMediaGrouping -> IO (Id NSString)
persistentIDPropertyForGroupingType groupingType =
  do
    cls' <- getRequiredClass "MPMediaItem"
    sendClassMsg cls' (mkSelector "persistentIDPropertyForGroupingType:") (retPtr retVoid) [argCLong (coerce groupingType)] >>= retainedObject . castPtr

-- | @+ titlePropertyForGroupingType:@
titlePropertyForGroupingType :: MPMediaGrouping -> IO (Id NSString)
titlePropertyForGroupingType groupingType =
  do
    cls' <- getRequiredClass "MPMediaItem"
    sendClassMsg cls' (mkSelector "titlePropertyForGroupingType:") (retPtr retVoid) [argCLong (coerce groupingType)] >>= retainedObject . castPtr

-- | @- persistentID@
persistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
persistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "persistentID") retCULong []

-- | @- mediaType@
mediaType :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO MPMediaType
mediaType mpMediaItem  =
  fmap (coerce :: CULong -> MPMediaType) $ sendMsg mpMediaItem (mkSelector "mediaType") retCULong []

-- | @- title@
title :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
title mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- albumTitle@
albumTitle :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
albumTitle mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "albumTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- albumPersistentID@
albumPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumPersistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "albumPersistentID") retCULong []

-- | @- artist@
artist :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
artist mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "artist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- artistPersistentID@
artistPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
artistPersistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "artistPersistentID") retCULong []

-- | @- albumArtist@
albumArtist :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
albumArtist mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "albumArtist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- albumArtistPersistentID@
albumArtistPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumArtistPersistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "albumArtistPersistentID") retCULong []

-- | @- genre@
genre :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
genre mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "genre") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- genrePersistentID@
genrePersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
genrePersistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "genrePersistentID") retCULong []

-- | @- composer@
composer :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
composer mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "composer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- composerPersistentID@
composerPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
composerPersistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "composerPersistentID") retCULong []

-- | @- playbackDuration@
playbackDuration :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CDouble
playbackDuration mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "playbackDuration") retCDouble []

-- | @- albumTrackNumber@
albumTrackNumber :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumTrackNumber mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "albumTrackNumber") retCULong []

-- | @- albumTrackCount@
albumTrackCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumTrackCount mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "albumTrackCount") retCULong []

-- | @- discNumber@
discNumber :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
discNumber mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "discNumber") retCULong []

-- | @- discCount@
discCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
discCount mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "discCount") retCULong []

-- | @- artwork@
artwork :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id MPMediaItemArtwork)
artwork mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "artwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- explicitItem@
explicitItem :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
explicitItem mpMediaItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpMediaItem (mkSelector "explicitItem") retCULong []

-- | @- lyrics@
lyrics :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
lyrics mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "lyrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- compilation@
compilation :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
compilation mpMediaItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpMediaItem (mkSelector "compilation") retCULong []

-- | @- releaseDate@
releaseDate :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSDate)
releaseDate mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "releaseDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beatsPerMinute@
beatsPerMinute :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
beatsPerMinute mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "beatsPerMinute") retCULong []

-- | @- comments@
comments :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
comments mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "comments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- assetURL@
assetURL :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSURL)
assetURL mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "assetURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cloudItem@
cloudItem :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
cloudItem mpMediaItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpMediaItem (mkSelector "cloudItem") retCULong []

-- | @- protectedAsset@
protectedAsset :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
protectedAsset mpMediaItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpMediaItem (mkSelector "protectedAsset") retCULong []

-- | @- podcastTitle@
podcastTitle :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
podcastTitle mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "podcastTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- podcastPersistentID@
podcastPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
podcastPersistentID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "podcastPersistentID") retCULong []

-- | @- playCount@
playCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
playCount mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "playCount") retCULong []

-- | @- skipCount@
skipCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
skipCount mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "skipCount") retCULong []

-- | @- rating@
rating :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
rating mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "rating") retCULong []

-- | @- lastPlayedDate@
lastPlayedDate :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSDate)
lastPlayedDate mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "lastPlayedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userGrouping@
userGrouping :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
userGrouping mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "userGrouping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bookmarkTime@
bookmarkTime :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CDouble
bookmarkTime mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "bookmarkTime") retCDouble []

-- | @- dateAdded@
dateAdded :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSDate)
dateAdded mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "dateAdded") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playbackStoreID@
playbackStoreID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
playbackStoreID mpMediaItem  =
  sendMsg mpMediaItem (mkSelector "playbackStoreID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- preorder@
preorder :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
preorder mpMediaItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpMediaItem (mkSelector "preorder") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistentIDPropertyForGroupingType:@
persistentIDPropertyForGroupingTypeSelector :: Selector
persistentIDPropertyForGroupingTypeSelector = mkSelector "persistentIDPropertyForGroupingType:"

-- | @Selector@ for @titlePropertyForGroupingType:@
titlePropertyForGroupingTypeSelector :: Selector
titlePropertyForGroupingTypeSelector = mkSelector "titlePropertyForGroupingType:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector
persistentIDSelector = mkSelector "persistentID"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @albumTitle@
albumTitleSelector :: Selector
albumTitleSelector = mkSelector "albumTitle"

-- | @Selector@ for @albumPersistentID@
albumPersistentIDSelector :: Selector
albumPersistentIDSelector = mkSelector "albumPersistentID"

-- | @Selector@ for @artist@
artistSelector :: Selector
artistSelector = mkSelector "artist"

-- | @Selector@ for @artistPersistentID@
artistPersistentIDSelector :: Selector
artistPersistentIDSelector = mkSelector "artistPersistentID"

-- | @Selector@ for @albumArtist@
albumArtistSelector :: Selector
albumArtistSelector = mkSelector "albumArtist"

-- | @Selector@ for @albumArtistPersistentID@
albumArtistPersistentIDSelector :: Selector
albumArtistPersistentIDSelector = mkSelector "albumArtistPersistentID"

-- | @Selector@ for @genre@
genreSelector :: Selector
genreSelector = mkSelector "genre"

-- | @Selector@ for @genrePersistentID@
genrePersistentIDSelector :: Selector
genrePersistentIDSelector = mkSelector "genrePersistentID"

-- | @Selector@ for @composer@
composerSelector :: Selector
composerSelector = mkSelector "composer"

-- | @Selector@ for @composerPersistentID@
composerPersistentIDSelector :: Selector
composerPersistentIDSelector = mkSelector "composerPersistentID"

-- | @Selector@ for @playbackDuration@
playbackDurationSelector :: Selector
playbackDurationSelector = mkSelector "playbackDuration"

-- | @Selector@ for @albumTrackNumber@
albumTrackNumberSelector :: Selector
albumTrackNumberSelector = mkSelector "albumTrackNumber"

-- | @Selector@ for @albumTrackCount@
albumTrackCountSelector :: Selector
albumTrackCountSelector = mkSelector "albumTrackCount"

-- | @Selector@ for @discNumber@
discNumberSelector :: Selector
discNumberSelector = mkSelector "discNumber"

-- | @Selector@ for @discCount@
discCountSelector :: Selector
discCountSelector = mkSelector "discCount"

-- | @Selector@ for @artwork@
artworkSelector :: Selector
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @explicitItem@
explicitItemSelector :: Selector
explicitItemSelector = mkSelector "explicitItem"

-- | @Selector@ for @lyrics@
lyricsSelector :: Selector
lyricsSelector = mkSelector "lyrics"

-- | @Selector@ for @compilation@
compilationSelector :: Selector
compilationSelector = mkSelector "compilation"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @beatsPerMinute@
beatsPerMinuteSelector :: Selector
beatsPerMinuteSelector = mkSelector "beatsPerMinute"

-- | @Selector@ for @comments@
commentsSelector :: Selector
commentsSelector = mkSelector "comments"

-- | @Selector@ for @assetURL@
assetURLSelector :: Selector
assetURLSelector = mkSelector "assetURL"

-- | @Selector@ for @cloudItem@
cloudItemSelector :: Selector
cloudItemSelector = mkSelector "cloudItem"

-- | @Selector@ for @protectedAsset@
protectedAssetSelector :: Selector
protectedAssetSelector = mkSelector "protectedAsset"

-- | @Selector@ for @podcastTitle@
podcastTitleSelector :: Selector
podcastTitleSelector = mkSelector "podcastTitle"

-- | @Selector@ for @podcastPersistentID@
podcastPersistentIDSelector :: Selector
podcastPersistentIDSelector = mkSelector "podcastPersistentID"

-- | @Selector@ for @playCount@
playCountSelector :: Selector
playCountSelector = mkSelector "playCount"

-- | @Selector@ for @skipCount@
skipCountSelector :: Selector
skipCountSelector = mkSelector "skipCount"

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

-- | @Selector@ for @lastPlayedDate@
lastPlayedDateSelector :: Selector
lastPlayedDateSelector = mkSelector "lastPlayedDate"

-- | @Selector@ for @userGrouping@
userGroupingSelector :: Selector
userGroupingSelector = mkSelector "userGrouping"

-- | @Selector@ for @bookmarkTime@
bookmarkTimeSelector :: Selector
bookmarkTimeSelector = mkSelector "bookmarkTime"

-- | @Selector@ for @dateAdded@
dateAddedSelector :: Selector
dateAddedSelector = mkSelector "dateAdded"

-- | @Selector@ for @playbackStoreID@
playbackStoreIDSelector :: Selector
playbackStoreIDSelector = mkSelector "playbackStoreID"

-- | @Selector@ for @preorder@
preorderSelector :: Selector
preorderSelector = mkSelector "preorder"

