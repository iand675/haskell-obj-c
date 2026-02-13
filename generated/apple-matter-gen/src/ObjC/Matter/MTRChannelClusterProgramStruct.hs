{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramStruct@.
module ObjC.Matter.MTRChannelClusterProgramStruct
  ( MTRChannelClusterProgramStruct
  , IsMTRChannelClusterProgramStruct(..)
  , identifier
  , setIdentifier
  , channel
  , setChannel
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , descriptionString
  , setDescriptionString
  , audioLanguages
  , setAudioLanguages
  , ratings
  , setRatings
  , thumbnailUrl
  , setThumbnailUrl
  , posterArtUrl
  , setPosterArtUrl
  , dvbiUrl
  , setDvbiUrl
  , releaseDate
  , setReleaseDate
  , parentalGuidanceText
  , setParentalGuidanceText
  , recordingFlag
  , setRecordingFlag
  , seriesInfo
  , setSeriesInfo
  , categoryList
  , setCategoryList
  , castList
  , setCastList
  , externalIDList
  , setExternalIDList
  , audioLanguagesSelector
  , castListSelector
  , categoryListSelector
  , channelSelector
  , descriptionStringSelector
  , dvbiUrlSelector
  , endTimeSelector
  , externalIDListSelector
  , identifierSelector
  , parentalGuidanceTextSelector
  , posterArtUrlSelector
  , ratingsSelector
  , recordingFlagSelector
  , releaseDateSelector
  , seriesInfoSelector
  , setAudioLanguagesSelector
  , setCastListSelector
  , setCategoryListSelector
  , setChannelSelector
  , setDescriptionStringSelector
  , setDvbiUrlSelector
  , setEndTimeSelector
  , setExternalIDListSelector
  , setIdentifierSelector
  , setParentalGuidanceTextSelector
  , setPosterArtUrlSelector
  , setRatingsSelector
  , setRecordingFlagSelector
  , setReleaseDateSelector
  , setSeriesInfoSelector
  , setStartTimeSelector
  , setSubtitleSelector
  , setThumbnailUrlSelector
  , setTitleSelector
  , startTimeSelector
  , subtitleSelector
  , thumbnailUrlSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
identifier mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setIdentifier mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setIdentifierSelector (toNSString value)

-- | @- channel@
channel :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id MTRChannelClusterChannelInfoStruct)
channel mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct channelSelector

-- | @- setChannel:@
setChannel :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsMTRChannelClusterChannelInfoStruct value) => mtrChannelClusterProgramStruct -> value -> IO ()
setChannel mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setChannelSelector (toMTRChannelClusterChannelInfoStruct value)

-- | @- startTime@
startTime :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSNumber)
startTime mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSNumber value) => mtrChannelClusterProgramStruct -> value -> IO ()
setStartTime mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setStartTimeSelector (toNSNumber value)

-- | @- endTime@
endTime :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSNumber)
endTime mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct endTimeSelector

-- | @- setEndTime:@
setEndTime :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSNumber value) => mtrChannelClusterProgramStruct -> value -> IO ()
setEndTime mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setEndTimeSelector (toNSNumber value)

-- | @- title@
title :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
title mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct titleSelector

-- | @- setTitle:@
setTitle :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setTitle mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setTitleSelector (toNSString value)

-- | @- subtitle@
subtitle :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
subtitle mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct subtitleSelector

-- | @- setSubtitle:@
setSubtitle :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setSubtitle mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setSubtitleSelector (toNSString value)

-- | @- descriptionString@
descriptionString :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
descriptionString mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct descriptionStringSelector

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setDescriptionString mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setDescriptionStringSelector (toNSString value)

-- | @- audioLanguages@
audioLanguages :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
audioLanguages mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct audioLanguagesSelector

-- | @- setAudioLanguages:@
setAudioLanguages :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setAudioLanguages mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setAudioLanguagesSelector (toNSArray value)

-- | @- ratings@
ratings :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
ratings mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct ratingsSelector

-- | @- setRatings:@
setRatings :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setRatings mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setRatingsSelector (toNSArray value)

-- | @- thumbnailUrl@
thumbnailUrl :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
thumbnailUrl mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct thumbnailUrlSelector

-- | @- setThumbnailUrl:@
setThumbnailUrl :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setThumbnailUrl mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setThumbnailUrlSelector (toNSString value)

-- | @- posterArtUrl@
posterArtUrl :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
posterArtUrl mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct posterArtUrlSelector

-- | @- setPosterArtUrl:@
setPosterArtUrl :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setPosterArtUrl mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setPosterArtUrlSelector (toNSString value)

-- | @- dvbiUrl@
dvbiUrl :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
dvbiUrl mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct dvbiUrlSelector

-- | @- setDvbiUrl:@
setDvbiUrl :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setDvbiUrl mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setDvbiUrlSelector (toNSString value)

-- | @- releaseDate@
releaseDate :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
releaseDate mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct releaseDateSelector

-- | @- setReleaseDate:@
setReleaseDate :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setReleaseDate mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setReleaseDateSelector (toNSString value)

-- | @- parentalGuidanceText@
parentalGuidanceText :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSString)
parentalGuidanceText mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct parentalGuidanceTextSelector

-- | @- setParentalGuidanceText:@
setParentalGuidanceText :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSString value) => mtrChannelClusterProgramStruct -> value -> IO ()
setParentalGuidanceText mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setParentalGuidanceTextSelector (toNSString value)

-- | @- recordingFlag@
recordingFlag :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSNumber)
recordingFlag mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct recordingFlagSelector

-- | @- setRecordingFlag:@
setRecordingFlag :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSNumber value) => mtrChannelClusterProgramStruct -> value -> IO ()
setRecordingFlag mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setRecordingFlagSelector (toNSNumber value)

-- | @- seriesInfo@
seriesInfo :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id MTRChannelClusterSeriesInfoStruct)
seriesInfo mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct seriesInfoSelector

-- | @- setSeriesInfo:@
setSeriesInfo :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsMTRChannelClusterSeriesInfoStruct value) => mtrChannelClusterProgramStruct -> value -> IO ()
setSeriesInfo mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setSeriesInfoSelector (toMTRChannelClusterSeriesInfoStruct value)

-- | @- categoryList@
categoryList :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
categoryList mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct categoryListSelector

-- | @- setCategoryList:@
setCategoryList :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setCategoryList mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setCategoryListSelector (toNSArray value)

-- | @- castList@
castList :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
castList mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct castListSelector

-- | @- setCastList:@
setCastList :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setCastList mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setCastListSelector (toNSArray value)

-- | @- externalIDList@
externalIDList :: IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct => mtrChannelClusterProgramStruct -> IO (Id NSArray)
externalIDList mtrChannelClusterProgramStruct =
  sendMessage mtrChannelClusterProgramStruct externalIDListSelector

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRChannelClusterProgramStruct mtrChannelClusterProgramStruct, IsNSArray value) => mtrChannelClusterProgramStruct -> value -> IO ()
setExternalIDList mtrChannelClusterProgramStruct value =
  sendMessage mtrChannelClusterProgramStruct setExternalIDListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] (Id MTRChannelClusterChannelInfoStruct)
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[Id MTRChannelClusterChannelInfoStruct] ()
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector '[] (Id NSNumber)
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector '[Id NSNumber] ()
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector '[] (Id NSString)
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector '[Id NSString] ()
setDescriptionStringSelector = mkSelector "setDescriptionString:"

-- | @Selector@ for @audioLanguages@
audioLanguagesSelector :: Selector '[] (Id NSArray)
audioLanguagesSelector = mkSelector "audioLanguages"

-- | @Selector@ for @setAudioLanguages:@
setAudioLanguagesSelector :: Selector '[Id NSArray] ()
setAudioLanguagesSelector = mkSelector "setAudioLanguages:"

-- | @Selector@ for @ratings@
ratingsSelector :: Selector '[] (Id NSArray)
ratingsSelector = mkSelector "ratings"

-- | @Selector@ for @setRatings:@
setRatingsSelector :: Selector '[Id NSArray] ()
setRatingsSelector = mkSelector "setRatings:"

-- | @Selector@ for @thumbnailUrl@
thumbnailUrlSelector :: Selector '[] (Id NSString)
thumbnailUrlSelector = mkSelector "thumbnailUrl"

-- | @Selector@ for @setThumbnailUrl:@
setThumbnailUrlSelector :: Selector '[Id NSString] ()
setThumbnailUrlSelector = mkSelector "setThumbnailUrl:"

-- | @Selector@ for @posterArtUrl@
posterArtUrlSelector :: Selector '[] (Id NSString)
posterArtUrlSelector = mkSelector "posterArtUrl"

-- | @Selector@ for @setPosterArtUrl:@
setPosterArtUrlSelector :: Selector '[Id NSString] ()
setPosterArtUrlSelector = mkSelector "setPosterArtUrl:"

-- | @Selector@ for @dvbiUrl@
dvbiUrlSelector :: Selector '[] (Id NSString)
dvbiUrlSelector = mkSelector "dvbiUrl"

-- | @Selector@ for @setDvbiUrl:@
setDvbiUrlSelector :: Selector '[Id NSString] ()
setDvbiUrlSelector = mkSelector "setDvbiUrl:"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector '[] (Id NSString)
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @setReleaseDate:@
setReleaseDateSelector :: Selector '[Id NSString] ()
setReleaseDateSelector = mkSelector "setReleaseDate:"

-- | @Selector@ for @parentalGuidanceText@
parentalGuidanceTextSelector :: Selector '[] (Id NSString)
parentalGuidanceTextSelector = mkSelector "parentalGuidanceText"

-- | @Selector@ for @setParentalGuidanceText:@
setParentalGuidanceTextSelector :: Selector '[Id NSString] ()
setParentalGuidanceTextSelector = mkSelector "setParentalGuidanceText:"

-- | @Selector@ for @recordingFlag@
recordingFlagSelector :: Selector '[] (Id NSNumber)
recordingFlagSelector = mkSelector "recordingFlag"

-- | @Selector@ for @setRecordingFlag:@
setRecordingFlagSelector :: Selector '[Id NSNumber] ()
setRecordingFlagSelector = mkSelector "setRecordingFlag:"

-- | @Selector@ for @seriesInfo@
seriesInfoSelector :: Selector '[] (Id MTRChannelClusterSeriesInfoStruct)
seriesInfoSelector = mkSelector "seriesInfo"

-- | @Selector@ for @setSeriesInfo:@
setSeriesInfoSelector :: Selector '[Id MTRChannelClusterSeriesInfoStruct] ()
setSeriesInfoSelector = mkSelector "setSeriesInfo:"

-- | @Selector@ for @categoryList@
categoryListSelector :: Selector '[] (Id NSArray)
categoryListSelector = mkSelector "categoryList"

-- | @Selector@ for @setCategoryList:@
setCategoryListSelector :: Selector '[Id NSArray] ()
setCategoryListSelector = mkSelector "setCategoryList:"

-- | @Selector@ for @castList@
castListSelector :: Selector '[] (Id NSArray)
castListSelector = mkSelector "castList"

-- | @Selector@ for @setCastList:@
setCastListSelector :: Selector '[Id NSArray] ()
setCastListSelector = mkSelector "setCastList:"

-- | @Selector@ for @externalIDList@
externalIDListSelector :: Selector '[] (Id NSArray)
externalIDListSelector = mkSelector "externalIDList"

-- | @Selector@ for @setExternalIDList:@
setExternalIDListSelector :: Selector '[Id NSArray] ()
setExternalIDListSelector = mkSelector "setExternalIDList:"

