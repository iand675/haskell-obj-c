{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | METrackInfo
--
-- A class incorporating track properties parsed from the media asset.
--
-- The METrackInfo properties are parsed asynchronously through the loadTrackInfoWithCompletionHandler method of METrackReader.
--
-- Generated bindings for @METrackInfo@.
module ObjC.MediaExtension.METrackInfo
  ( METrackInfo
  , IsMETrackInfo(..)
  , new
  , init_
  , initWithMediaType_trackID_formatDescriptions
  , mediaType
  , trackID
  , enabled
  , setEnabled
  , formatDescriptions
  , nominalFrameRate
  , setNominalFrameRate
  , requiresFrameReordering
  , setRequiresFrameReordering
  , extendedLanguageTag
  , setExtendedLanguageTag
  , naturalTimescale
  , setNaturalTimescale
  , trackEdits
  , setTrackEdits
  , enabledSelector
  , extendedLanguageTagSelector
  , formatDescriptionsSelector
  , initSelector
  , initWithMediaType_trackID_formatDescriptionsSelector
  , mediaTypeSelector
  , naturalTimescaleSelector
  , newSelector
  , nominalFrameRateSelector
  , requiresFrameReorderingSelector
  , setEnabledSelector
  , setExtendedLanguageTagSelector
  , setNaturalTimescaleSelector
  , setNominalFrameRateSelector
  , setRequiresFrameReorderingSelector
  , setTrackEditsSelector
  , trackEditsSelector
  , trackIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id METrackInfo)
new  =
  do
    cls' <- getRequiredClass "METrackInfo"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id METrackInfo)
init_ meTrackInfo =
  sendOwnedMessage meTrackInfo initSelector

-- | initWithMediaType
--
-- Initializes a new METrackInfo instance.
--
-- The main initializer for the METrackInfo class. After creating the class, the METrackReader should fill in all the relevant properties with the values read in from the media track.
--
-- @mediaType@ — The media type of the track.
--
-- @trackID@ — An integer identifying the track within the media asset.
--
-- @formatDescriptions@ — The format descriptions for the track, as an NSArray.
--
-- Returns: A new instance of METrackInfo.
--
-- ObjC selector: @- initWithMediaType:trackID:formatDescriptions:@
initWithMediaType_trackID_formatDescriptions :: (IsMETrackInfo meTrackInfo, IsNSArray formatDescriptions) => meTrackInfo -> CUInt -> CInt -> formatDescriptions -> IO (Id METrackInfo)
initWithMediaType_trackID_formatDescriptions meTrackInfo mediaType trackID formatDescriptions =
  sendOwnedMessage meTrackInfo initWithMediaType_trackID_formatDescriptionsSelector mediaType trackID (toNSArray formatDescriptions)

-- | mediaType
--
-- The media type of the track.
--
-- This value is set through the class initializer.
--
-- ObjC selector: @- mediaType@
mediaType :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CUInt
mediaType meTrackInfo =
  sendMessage meTrackInfo mediaTypeSelector

-- | trackID
--
-- An integer identifying the track within the media asset.
--
-- The track ID is used to uniquely identify the track within the MEFormatReader. Track IDs must be unique within a media asset but do not need to be unique across assets. If a media format does not have a native concept of track IDs, track IDs may be assigned starting from 1. The track ID value of 0 is reserved to indicate an invalid track ID. This value is set through the class initializer.
--
-- ObjC selector: @- trackID@
trackID :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CInt
trackID meTrackInfo =
  sendMessage meTrackInfo trackIDSelector

-- | enabled
--
-- A BOOL value indicating whether the track is enabled by default.
--
-- ObjC selector: @- enabled@
enabled :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO Bool
enabled meTrackInfo =
  sendMessage meTrackInfo enabledSelector

-- | enabled
--
-- A BOOL value indicating whether the track is enabled by default.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsMETrackInfo meTrackInfo => meTrackInfo -> Bool -> IO ()
setEnabled meTrackInfo value =
  sendMessage meTrackInfo setEnabledSelector value

-- | formatDescriptions
--
-- The format descriptions for the track, as an NSArray.
--
-- This value is set through the class initializer.
--
-- ObjC selector: @- formatDescriptions@
formatDescriptions :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id NSArray)
formatDescriptions meTrackInfo =
  sendMessage meTrackInfo formatDescriptionsSelector

-- | nominalFrameRate
--
-- The frame rate of the track, in frames per second, as a 32-bit floating point number.
--
-- For field-based video tracks that carry one field per media sample, the value of this property is the field rate, not the frame rate. This information from this property may be used by the MediaToolbox to calculate the maximum playback speed.
--
-- ObjC selector: @- nominalFrameRate@
nominalFrameRate :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CFloat
nominalFrameRate meTrackInfo =
  sendMessage meTrackInfo nominalFrameRateSelector

-- | nominalFrameRate
--
-- The frame rate of the track, in frames per second, as a 32-bit floating point number.
--
-- For field-based video tracks that carry one field per media sample, the value of this property is the field rate, not the frame rate. This information from this property may be used by the MediaToolbox to calculate the maximum playback speed.
--
-- ObjC selector: @- setNominalFrameRate:@
setNominalFrameRate :: IsMETrackInfo meTrackInfo => meTrackInfo -> CFloat -> IO ()
setNominalFrameRate meTrackInfo value =
  sendMessage meTrackInfo setNominalFrameRateSelector value

-- | requiresFrameReordering
--
-- Indicates whether frame reordering occurs in the track.
--
-- The value is YES if frame reordering occurs, NO otherwise. This property is only valid for tracks with video media type and should return NO for if implemented for other track types.
--
-- ObjC selector: @- requiresFrameReordering@
requiresFrameReordering :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO Bool
requiresFrameReordering meTrackInfo =
  sendMessage meTrackInfo requiresFrameReorderingSelector

-- | requiresFrameReordering
--
-- Indicates whether frame reordering occurs in the track.
--
-- The value is YES if frame reordering occurs, NO otherwise. This property is only valid for tracks with video media type and should return NO for if implemented for other track types.
--
-- ObjC selector: @- setRequiresFrameReordering:@
setRequiresFrameReordering :: IsMETrackInfo meTrackInfo => meTrackInfo -> Bool -> IO ()
setRequiresFrameReordering meTrackInfo value =
  sendMessage meTrackInfo setRequiresFrameReorderingSelector value

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- This property may be used by the MediaToolbox to group similar language tracks together or to match audio and caption tracks. If no language tag is indicated, this property should be set to nil.
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id NSString)
extendedLanguageTag meTrackInfo =
  sendMessage meTrackInfo extendedLanguageTagSelector

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- This property may be used by the MediaToolbox to group similar language tracks together or to match audio and caption tracks. If no language tag is indicated, this property should be set to nil.
--
-- ObjC selector: @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsMETrackInfo meTrackInfo, IsNSString value) => meTrackInfo -> value -> IO ()
setExtendedLanguageTag meTrackInfo value =
  sendMessage meTrackInfo setExtendedLanguageTagSelector (toNSString value)

-- | naturalTimescale
--
-- The natural timescale of the track, as a CMTimeScale value.
--
-- ObjC selector: @- naturalTimescale@
naturalTimescale :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CInt
naturalTimescale meTrackInfo =
  sendMessage meTrackInfo naturalTimescaleSelector

-- | naturalTimescale
--
-- The natural timescale of the track, as a CMTimeScale value.
--
-- ObjC selector: @- setNaturalTimescale:@
setNaturalTimescale :: IsMETrackInfo meTrackInfo => meTrackInfo -> CInt -> IO ()
setNaturalTimescale meTrackInfo value =
  sendMessage meTrackInfo setNaturalTimescaleSelector value

-- | trackEdits
--
-- Returns the array of edit segments for the given track.
--
-- Each NSValue in the array contains a CMTimeMapping object describing the track edit. The CMTimeMapping.target time ranges for successive edits must partition the time range from 0 to the track's duration. In other words, for edit index = 0 the CMTimeMapping.target.start must be kCMTimeZero, while for edit index > 0, the CMTimeMapping.target.start must match the CMTimeRangeGetEnd(CMTimeMapping.target) for edit (index - 1). It is valid for a track to have an empty trackEdits array; this means that there is nothing at all in the track and the track duration is zero. If this property is implemented for media asset formats that do not support edit segments, it can return nil.
--
-- ObjC selector: @- trackEdits@
trackEdits :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id NSArray)
trackEdits meTrackInfo =
  sendMessage meTrackInfo trackEditsSelector

-- | trackEdits
--
-- Returns the array of edit segments for the given track.
--
-- Each NSValue in the array contains a CMTimeMapping object describing the track edit. The CMTimeMapping.target time ranges for successive edits must partition the time range from 0 to the track's duration. In other words, for edit index = 0 the CMTimeMapping.target.start must be kCMTimeZero, while for edit index > 0, the CMTimeMapping.target.start must match the CMTimeRangeGetEnd(CMTimeMapping.target) for edit (index - 1). It is valid for a track to have an empty trackEdits array; this means that there is nothing at all in the track and the track duration is zero. If this property is implemented for media asset formats that do not support edit segments, it can return nil.
--
-- ObjC selector: @- setTrackEdits:@
setTrackEdits :: (IsMETrackInfo meTrackInfo, IsNSArray value) => meTrackInfo -> value -> IO ()
setTrackEdits meTrackInfo value =
  sendMessage meTrackInfo setTrackEditsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id METrackInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id METrackInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMediaType:trackID:formatDescriptions:@
initWithMediaType_trackID_formatDescriptionsSelector :: Selector '[CUInt, CInt, Id NSArray] (Id METrackInfo)
initWithMediaType_trackID_formatDescriptionsSelector = mkSelector "initWithMediaType:trackID:formatDescriptions:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] CUInt
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @formatDescriptions@
formatDescriptionsSelector :: Selector '[] (Id NSArray)
formatDescriptionsSelector = mkSelector "formatDescriptions"

-- | @Selector@ for @nominalFrameRate@
nominalFrameRateSelector :: Selector '[] CFloat
nominalFrameRateSelector = mkSelector "nominalFrameRate"

-- | @Selector@ for @setNominalFrameRate:@
setNominalFrameRateSelector :: Selector '[CFloat] ()
setNominalFrameRateSelector = mkSelector "setNominalFrameRate:"

-- | @Selector@ for @requiresFrameReordering@
requiresFrameReorderingSelector :: Selector '[] Bool
requiresFrameReorderingSelector = mkSelector "requiresFrameReordering"

-- | @Selector@ for @setRequiresFrameReordering:@
setRequiresFrameReorderingSelector :: Selector '[Bool] ()
setRequiresFrameReorderingSelector = mkSelector "setRequiresFrameReordering:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector '[] (Id NSString)
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector '[Id NSString] ()
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

-- | @Selector@ for @naturalTimescale@
naturalTimescaleSelector :: Selector '[] CInt
naturalTimescaleSelector = mkSelector "naturalTimescale"

-- | @Selector@ for @setNaturalTimescale:@
setNaturalTimescaleSelector :: Selector '[CInt] ()
setNaturalTimescaleSelector = mkSelector "setNaturalTimescale:"

-- | @Selector@ for @trackEdits@
trackEditsSelector :: Selector '[] (Id NSArray)
trackEditsSelector = mkSelector "trackEdits"

-- | @Selector@ for @setTrackEdits:@
setTrackEditsSelector :: Selector '[Id NSArray] ()
setTrackEditsSelector = mkSelector "setTrackEdits:"

