{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A media item represents a single piece of media (such as a song, a video, a podcast, etc) in the iTunes library. 			  A media item has an overall unique identifier, accessed using the persistentID property. The media item			  metadata may be accessed through its individual properties or via the ITLibMediaEntity general property accessor			  methods.
--
-- Generated bindings for @ITLibMediaItem@.
module ObjC.ITunesLibrary.ITLibMediaItem
  ( ITLibMediaItem
  , IsITLibMediaItem(..)
  , title
  , sortTitle
  , artist
  , composer
  , sortComposer
  , rating
  , ratingComputed
  , startTime
  , stopTime
  , album
  , genre
  , kind
  , mediaKind
  , fileSize
  , size
  , totalTime
  , trackNumber
  , category
  , description
  , lyricsContentRating
  , contentRating
  , modifiedDate
  , addedDate
  , bitrate
  , sampleRate
  , beatsPerMinute
  , playCount
  , lastPlayedDate
  , playStatus
  , location
  , artworkAvailable
  , artwork
  , comments
  , purchased
  , cloud
  , drmProtected
  , video
  , videoInfo
  , releaseDate
  , year
  , fileType
  , skipCount
  , skipDate
  , voiceOverLanguage
  , volumeAdjustment
  , volumeNormalizationEnergy
  , userDisabled
  , grouping
  , locationType
  , addedDateSelector
  , albumSelector
  , artistSelector
  , artworkAvailableSelector
  , artworkSelector
  , beatsPerMinuteSelector
  , bitrateSelector
  , categorySelector
  , cloudSelector
  , commentsSelector
  , composerSelector
  , contentRatingSelector
  , descriptionSelector
  , drmProtectedSelector
  , fileSizeSelector
  , fileTypeSelector
  , genreSelector
  , groupingSelector
  , kindSelector
  , lastPlayedDateSelector
  , locationSelector
  , locationTypeSelector
  , lyricsContentRatingSelector
  , mediaKindSelector
  , modifiedDateSelector
  , playCountSelector
  , playStatusSelector
  , purchasedSelector
  , ratingComputedSelector
  , ratingSelector
  , releaseDateSelector
  , sampleRateSelector
  , sizeSelector
  , skipCountSelector
  , skipDateSelector
  , sortComposerSelector
  , sortTitleSelector
  , startTimeSelector
  , stopTimeSelector
  , titleSelector
  , totalTimeSelector
  , trackNumberSelector
  , userDisabledSelector
  , videoInfoSelector
  , videoSelector
  , voiceOverLanguageSelector
  , volumeAdjustmentSelector
  , volumeNormalizationEnergySelector
  , yearSelector

  -- * Enum types
  , ITLibMediaItemLocationType(ITLibMediaItemLocationType)
  , pattern ITLibMediaItemLocationTypeUnknown
  , pattern ITLibMediaItemLocationTypeFile
  , pattern ITLibMediaItemLocationTypeURL
  , pattern ITLibMediaItemLocationTypeRemote
  , ITLibMediaItemLyricsContentRating(ITLibMediaItemLyricsContentRating)
  , pattern ITLibMediaItemLyricsContentRatingNone
  , pattern ITLibMediaItemLyricsContentRatingExplicit
  , pattern ITLibMediaItemLyricsContentRatingClean
  , ITLibMediaItemMediaKind(ITLibMediaItemMediaKind)
  , pattern ITLibMediaItemMediaKindUnknown
  , pattern ITLibMediaItemMediaKindSong
  , pattern ITLibMediaItemMediaKindMovie
  , pattern ITLibMediaItemMediaKindPodcast
  , pattern ITLibMediaItemMediaKindAudiobook
  , pattern ITLibMediaItemMediaKindPDFBooklet
  , pattern ITLibMediaItemMediaKindMusicVideo
  , pattern ITLibMediaItemMediaKindTVShow
  , pattern ITLibMediaItemMediaKindInteractiveBooklet
  , pattern ITLibMediaItemMediaKindHomeVideo
  , pattern ITLibMediaItemMediaKindRingtone
  , pattern ITLibMediaItemMediaKindDigitalBooklet
  , pattern ITLibMediaItemMediaKindIOSApplication
  , pattern ITLibMediaItemMediaKindVoiceMemo
  , pattern ITLibMediaItemMediaKindiTunesU
  , pattern ITLibMediaItemMediaKindBook
  , pattern ITLibMediaItemMediaKindPDFBook
  , pattern ITLibMediaItemMediaKindAlertTone
  , ITLibMediaItemPlayStatus(ITLibMediaItemPlayStatus)
  , pattern ITLibMediaItemPlayStatusNone
  , pattern ITLibMediaItemPlayStatusPartiallyPlayed
  , pattern ITLibMediaItemPlayStatusUnplayed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The title of this media item. May be empty.
--
-- ObjC selector: @- title@
title :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
title itLibMediaItem =
  sendMessage itLibMediaItem titleSelector

-- | The title of this media item that should be used for sorting purposes.  If nil, use the title field.
--
-- ObjC selector: @- sortTitle@
sortTitle :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
sortTitle itLibMediaItem =
  sendMessage itLibMediaItem sortTitleSelector

-- | The artist associated with this media item.
--
-- ObjC selector: @- artist@
artist :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibArtist)
artist itLibMediaItem =
  sendMessage itLibMediaItem artistSelector

-- | The name of the composer associated with this media item.  May be empty.
--
-- ObjC selector: @- composer@
composer :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
composer itLibMediaItem =
  sendMessage itLibMediaItem composerSelector

-- | The name of the composer associated with this media item that should be used for sorting purposes. If nil, use the composer field.
--
-- ObjC selector: @- sortComposer@
sortComposer :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
sortComposer itLibMediaItem =
  sendMessage itLibMediaItem sortComposerSelector

-- | The rating of this media item.
--
-- ObjC selector: @- rating@
rating :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CLong
rating itLibMediaItem =
  sendMessage itLibMediaItem ratingSelector

-- | Whether this media item's rating is computed.
--
-- ObjC selector: @- ratingComputed@
ratingComputed :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
ratingComputed itLibMediaItem =
  sendMessage itLibMediaItem ratingComputedSelector

-- | If non-zero, the actual time playback for this media item will start instead of 0:00 (in milliseconds).
--
-- ObjC selector: @- startTime@
startTime :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
startTime itLibMediaItem =
  sendMessage itLibMediaItem startTimeSelector

-- | If non-zero, the actual time playback for this media item will stop vs. the total time (in milliseconds).
--
-- ObjC selector: @- stopTime@
stopTime :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
stopTime itLibMediaItem =
  sendMessage itLibMediaItem stopTimeSelector

-- | The album where this media item belongs.
--
-- ObjC selector: @- album@
album :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibAlbum)
album itLibMediaItem =
  sendMessage itLibMediaItem albumSelector

-- | The genre associated with this media item. May be empty.
--
-- ObjC selector: @- genre@
genre :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
genre itLibMediaItem =
  sendMessage itLibMediaItem genreSelector

-- | This media item's file kind (ex. MPEG audio file).
--
-- ObjC selector: @- kind@
kind :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
kind itLibMediaItem =
  sendMessage itLibMediaItem kindSelector

-- | This media item's media kind.
--
-- ObjC selector: @- mediaKind@
mediaKind :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemMediaKind
mediaKind itLibMediaItem =
  sendMessage itLibMediaItem mediaKindSelector

-- | The size in bytes of this media item on disk.
--
-- ObjC selector: @- fileSize@
fileSize :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
fileSize itLibMediaItem =
  sendMessage itLibMediaItem fileSizeSelector

-- | The size in bytes of this media item on disk. (deprecated: use fileSize instead)
--
-- ObjC selector: @- size@
size :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
size itLibMediaItem =
  sendMessage itLibMediaItem sizeSelector

-- | The length of this media item in milliseconds.
--
-- ObjC selector: @- totalTime@
totalTime :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
totalTime itLibMediaItem =
  sendMessage itLibMediaItem totalTimeSelector

-- | The position of this media item within its album.
--
-- ObjC selector: @- trackNumber@
trackNumber :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
trackNumber itLibMediaItem =
  sendMessage itLibMediaItem trackNumberSelector

-- | The podcast category of this media item (implies this media item is a podcast).
--
-- ObjC selector: @- category@
category :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
category itLibMediaItem =
  sendMessage itLibMediaItem categorySelector

-- | Any podcast description of with this media item (implies this media item is a podcast).
--
-- ObjC selector: @- description@
description :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
description itLibMediaItem =
  sendMessage itLibMediaItem descriptionSelector

-- | The content rating of this media item's lyrics.
--
-- ObjC selector: @- lyricsContentRating@
lyricsContentRating :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemLyricsContentRating
lyricsContentRating itLibMediaItem =
  sendMessage itLibMediaItem lyricsContentRatingSelector

-- | The extended content rating of this media item.
--
-- ObjC selector: @- contentRating@
contentRating :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
contentRating itLibMediaItem =
  sendMessage itLibMediaItem contentRatingSelector

-- | The date and time this media item was last modified.
--
-- ObjC selector: @- modifiedDate@
modifiedDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
modifiedDate itLibMediaItem =
  sendMessage itLibMediaItem modifiedDateSelector

-- | The date and media item this media item was added to the iTunes database.
--
-- ObjC selector: @- addedDate@
addedDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
addedDate itLibMediaItem =
  sendMessage itLibMediaItem addedDateSelector

-- | The bitrate of this media item in kbps.
--
-- ObjC selector: @- bitrate@
bitrate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
bitrate itLibMediaItem =
  sendMessage itLibMediaItem bitrateSelector

-- | The sample rate of this media item in samples per second.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
sampleRate itLibMediaItem =
  sendMessage itLibMediaItem sampleRateSelector

-- | The BPM (beats per minute) of this media item.
--
-- ObjC selector: @- beatsPerMinute@
beatsPerMinute :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
beatsPerMinute itLibMediaItem =
  sendMessage itLibMediaItem beatsPerMinuteSelector

-- | The number of times this media item has been played in iTunes.
--
-- ObjC selector: @- playCount@
playCount :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
playCount itLibMediaItem =
  sendMessage itLibMediaItem playCountSelector

-- | The date and time this media item was last played in iTunes, or nil if this media item has not been played.
--
-- ObjC selector: @- lastPlayedDate@
lastPlayedDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
lastPlayedDate itLibMediaItem =
  sendMessage itLibMediaItem lastPlayedDateSelector

-- | The play status for this media.  Represents partially played and unplayed states for videos and podcasts. Other media kinds always return "none".
--
-- ObjC selector: @- playStatus@
playStatus :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemPlayStatus
playStatus itLibMediaItem =
  sendMessage itLibMediaItem playStatusSelector

-- | The location of this media item on disk.
--
-- ObjC selector: @- location@
location :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSURL)
location itLibMediaItem =
  sendMessage itLibMediaItem locationSelector

-- | Whether this media item has artwork.
--
-- ObjC selector: @- artworkAvailable@
artworkAvailable :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
artworkAvailable itLibMediaItem =
  sendMessage itLibMediaItem artworkAvailableSelector

-- | Whether this media item has artwork.
--
-- ObjC selector: @- artwork@
artwork :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibArtwork)
artwork itLibMediaItem =
  sendMessage itLibMediaItem artworkSelector

-- | Any comments associated with this media item.
--
-- ObjC selector: @- comments@
comments :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
comments itLibMediaItem =
  sendMessage itLibMediaItem commentsSelector

-- | Whether this media item was purchased.
--
-- ObjC selector: @- purchased@
purchased :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
purchased itLibMediaItem =
  sendMessage itLibMediaItem purchasedSelector

-- | Whether this media item is iTunes Match or iTunes in the Cloud.
--
-- ObjC selector: @- cloud@
cloud :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
cloud itLibMediaItem =
  sendMessage itLibMediaItem cloudSelector

-- | Whether this media item is DRM protected.
--
-- ObjC selector: @- drmProtected@
drmProtected :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
drmProtected itLibMediaItem =
  sendMessage itLibMediaItem drmProtectedSelector

-- | Whether this media item is a video media item (video podcast, movie, etc).
--
-- ObjC selector: @- video@
video :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
video itLibMediaItem =
  sendMessage itLibMediaItem videoSelector

-- | The video information of this media item (implies this media item is a video media item).
--
-- ObjC selector: @- videoInfo@
videoInfo :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibMediaItemVideoInfo)
videoInfo itLibMediaItem =
  sendMessage itLibMediaItem videoInfoSelector

-- | The date this media item was released.
--
-- ObjC selector: @- releaseDate@
releaseDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
releaseDate itLibMediaItem =
  sendMessage itLibMediaItem releaseDateSelector

-- | The year when this media item was released.
--
-- ObjC selector: @- year@
year :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
year itLibMediaItem =
  sendMessage itLibMediaItem yearSelector

-- | The type of the file this media item refers to.
--
-- ObjC selector: @- fileType@
fileType :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
fileType itLibMediaItem =
  sendMessage itLibMediaItem fileTypeSelector

-- | The number of times this media item has been skiped.
--
-- ObjC selector: @- skipCount@
skipCount :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
skipCount itLibMediaItem =
  sendMessage itLibMediaItem skipCountSelector

-- | The date and time when this media item was last skipped.
--
-- ObjC selector: @- skipDate@
skipDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
skipDate itLibMediaItem =
  sendMessage itLibMediaItem skipDateSelector

-- | The voice-over language of this media item
--
-- ObjC selector: @- voiceOverLanguage@
voiceOverLanguage :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO RawId
voiceOverLanguage itLibMediaItem =
  sendMessage itLibMediaItem voiceOverLanguageSelector

-- | The volume adjustment used for this media item if any.
--
-- ObjC selector: @- volumeAdjustment@
volumeAdjustment :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CLong
volumeAdjustment itLibMediaItem =
  sendMessage itLibMediaItem volumeAdjustmentSelector

-- | The volume normalization energy applied to this media item.
--
-- ObjC selector: @- volumeNormalizationEnergy@
volumeNormalizationEnergy :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
volumeNormalizationEnergy itLibMediaItem =
  sendMessage itLibMediaItem volumeNormalizationEnergySelector

-- | Whether the user has disabled this media item.
--
-- ObjC selector: @- userDisabled@
userDisabled :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
userDisabled itLibMediaItem =
  sendMessage itLibMediaItem userDisabledSelector

-- | The grouping of this media item.
--
-- ObjC selector: @- grouping@
grouping :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
grouping itLibMediaItem =
  sendMessage itLibMediaItem groupingSelector

-- | The type of this media item with respect to its location.
--
-- ObjC selector: @- locationType@
locationType :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemLocationType
locationType itLibMediaItem =
  sendMessage itLibMediaItem locationTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @sortTitle@
sortTitleSelector :: Selector '[] (Id NSString)
sortTitleSelector = mkSelector "sortTitle"

-- | @Selector@ for @artist@
artistSelector :: Selector '[] (Id ITLibArtist)
artistSelector = mkSelector "artist"

-- | @Selector@ for @composer@
composerSelector :: Selector '[] (Id NSString)
composerSelector = mkSelector "composer"

-- | @Selector@ for @sortComposer@
sortComposerSelector :: Selector '[] (Id NSString)
sortComposerSelector = mkSelector "sortComposer"

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] CLong
ratingSelector = mkSelector "rating"

-- | @Selector@ for @ratingComputed@
ratingComputedSelector :: Selector '[] Bool
ratingComputedSelector = mkSelector "ratingComputed"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] CULong
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @stopTime@
stopTimeSelector :: Selector '[] CULong
stopTimeSelector = mkSelector "stopTime"

-- | @Selector@ for @album@
albumSelector :: Selector '[] (Id ITLibAlbum)
albumSelector = mkSelector "album"

-- | @Selector@ for @genre@
genreSelector :: Selector '[] (Id NSString)
genreSelector = mkSelector "genre"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] (Id NSString)
kindSelector = mkSelector "kind"

-- | @Selector@ for @mediaKind@
mediaKindSelector :: Selector '[] ITLibMediaItemMediaKind
mediaKindSelector = mkSelector "mediaKind"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector '[] CULong
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] CULong
sizeSelector = mkSelector "size"

-- | @Selector@ for @totalTime@
totalTimeSelector :: Selector '[] CULong
totalTimeSelector = mkSelector "totalTime"

-- | @Selector@ for @trackNumber@
trackNumberSelector :: Selector '[] CULong
trackNumberSelector = mkSelector "trackNumber"

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @lyricsContentRating@
lyricsContentRatingSelector :: Selector '[] ITLibMediaItemLyricsContentRating
lyricsContentRatingSelector = mkSelector "lyricsContentRating"

-- | @Selector@ for @contentRating@
contentRatingSelector :: Selector '[] (Id NSString)
contentRatingSelector = mkSelector "contentRating"

-- | @Selector@ for @modifiedDate@
modifiedDateSelector :: Selector '[] (Id NSDate)
modifiedDateSelector = mkSelector "modifiedDate"

-- | @Selector@ for @addedDate@
addedDateSelector :: Selector '[] (Id NSDate)
addedDateSelector = mkSelector "addedDate"

-- | @Selector@ for @bitrate@
bitrateSelector :: Selector '[] CULong
bitrateSelector = mkSelector "bitrate"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector '[] CULong
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @beatsPerMinute@
beatsPerMinuteSelector :: Selector '[] CULong
beatsPerMinuteSelector = mkSelector "beatsPerMinute"

-- | @Selector@ for @playCount@
playCountSelector :: Selector '[] CULong
playCountSelector = mkSelector "playCount"

-- | @Selector@ for @lastPlayedDate@
lastPlayedDateSelector :: Selector '[] (Id NSDate)
lastPlayedDateSelector = mkSelector "lastPlayedDate"

-- | @Selector@ for @playStatus@
playStatusSelector :: Selector '[] ITLibMediaItemPlayStatus
playStatusSelector = mkSelector "playStatus"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id NSURL)
locationSelector = mkSelector "location"

-- | @Selector@ for @artworkAvailable@
artworkAvailableSelector :: Selector '[] Bool
artworkAvailableSelector = mkSelector "artworkAvailable"

-- | @Selector@ for @artwork@
artworkSelector :: Selector '[] (Id ITLibArtwork)
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @comments@
commentsSelector :: Selector '[] (Id NSString)
commentsSelector = mkSelector "comments"

-- | @Selector@ for @purchased@
purchasedSelector :: Selector '[] Bool
purchasedSelector = mkSelector "purchased"

-- | @Selector@ for @cloud@
cloudSelector :: Selector '[] Bool
cloudSelector = mkSelector "cloud"

-- | @Selector@ for @drmProtected@
drmProtectedSelector :: Selector '[] Bool
drmProtectedSelector = mkSelector "drmProtected"

-- | @Selector@ for @video@
videoSelector :: Selector '[] Bool
videoSelector = mkSelector "video"

-- | @Selector@ for @videoInfo@
videoInfoSelector :: Selector '[] (Id ITLibMediaItemVideoInfo)
videoInfoSelector = mkSelector "videoInfo"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector '[] (Id NSDate)
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @year@
yearSelector :: Selector '[] CULong
yearSelector = mkSelector "year"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector '[] CULong
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @skipCount@
skipCountSelector :: Selector '[] CULong
skipCountSelector = mkSelector "skipCount"

-- | @Selector@ for @skipDate@
skipDateSelector :: Selector '[] (Id NSDate)
skipDateSelector = mkSelector "skipDate"

-- | @Selector@ for @voiceOverLanguage@
voiceOverLanguageSelector :: Selector '[] RawId
voiceOverLanguageSelector = mkSelector "voiceOverLanguage"

-- | @Selector@ for @volumeAdjustment@
volumeAdjustmentSelector :: Selector '[] CLong
volumeAdjustmentSelector = mkSelector "volumeAdjustment"

-- | @Selector@ for @volumeNormalizationEnergy@
volumeNormalizationEnergySelector :: Selector '[] CULong
volumeNormalizationEnergySelector = mkSelector "volumeNormalizationEnergy"

-- | @Selector@ for @userDisabled@
userDisabledSelector :: Selector '[] Bool
userDisabledSelector = mkSelector "userDisabled"

-- | @Selector@ for @grouping@
groupingSelector :: Selector '[] (Id NSString)
groupingSelector = mkSelector "grouping"

-- | @Selector@ for @locationType@
locationTypeSelector :: Selector '[] ITLibMediaItemLocationType
locationTypeSelector = mkSelector "locationType"

