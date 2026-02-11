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
  , newSelector
  , initSelector
  , initWithMediaType_trackID_formatDescriptionsSelector
  , mediaTypeSelector
  , trackIDSelector
  , enabledSelector
  , setEnabledSelector
  , formatDescriptionsSelector
  , nominalFrameRateSelector
  , setNominalFrameRateSelector
  , requiresFrameReorderingSelector
  , setRequiresFrameReorderingSelector
  , extendedLanguageTagSelector
  , setExtendedLanguageTagSelector
  , naturalTimescaleSelector
  , setNaturalTimescaleSelector
  , trackEditsSelector
  , setTrackEditsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id METrackInfo)
new  =
  do
    cls' <- getRequiredClass "METrackInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id METrackInfo)
init_ meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithMediaType_trackID_formatDescriptions meTrackInfo  mediaType trackID formatDescriptions =
  withObjCPtr formatDescriptions $ \raw_formatDescriptions ->
      sendMsg meTrackInfo (mkSelector "initWithMediaType:trackID:formatDescriptions:") (retPtr retVoid) [argCUInt mediaType, argCInt trackID, argPtr (castPtr raw_formatDescriptions :: Ptr ())] >>= ownedObject . castPtr

-- | mediaType
--
-- The media type of the track.
--
-- This value is set through the class initializer.
--
-- ObjC selector: @- mediaType@
mediaType :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CUInt
mediaType meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "mediaType") retCUInt []

-- | trackID
--
-- An integer identifying the track within the media asset.
--
-- The track ID is used to uniquely identify the track within the MEFormatReader. Track IDs must be unique within a media asset but do not need to be unique across assets. If a media format does not have a native concept of track IDs, track IDs may be assigned starting from 1. The track ID value of 0 is reserved to indicate an invalid track ID. This value is set through the class initializer.
--
-- ObjC selector: @- trackID@
trackID :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CInt
trackID meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "trackID") retCInt []

-- | enabled
--
-- A BOOL value indicating whether the track is enabled by default.
--
-- ObjC selector: @- enabled@
enabled :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO Bool
enabled meTrackInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meTrackInfo (mkSelector "enabled") retCULong []

-- | enabled
--
-- A BOOL value indicating whether the track is enabled by default.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsMETrackInfo meTrackInfo => meTrackInfo -> Bool -> IO ()
setEnabled meTrackInfo  value =
    sendMsg meTrackInfo (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | formatDescriptions
--
-- The format descriptions for the track, as an NSArray.
--
-- This value is set through the class initializer.
--
-- ObjC selector: @- formatDescriptions@
formatDescriptions :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id NSArray)
formatDescriptions meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "formatDescriptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nominalFrameRate
--
-- The frame rate of the track, in frames per second, as a 32-bit floating point number.
--
-- For field-based video tracks that carry one field per media sample, the value of this property is the field rate, not the frame rate. This information from this property may be used by the MediaToolbox to calculate the maximum playback speed.
--
-- ObjC selector: @- nominalFrameRate@
nominalFrameRate :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CFloat
nominalFrameRate meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "nominalFrameRate") retCFloat []

-- | nominalFrameRate
--
-- The frame rate of the track, in frames per second, as a 32-bit floating point number.
--
-- For field-based video tracks that carry one field per media sample, the value of this property is the field rate, not the frame rate. This information from this property may be used by the MediaToolbox to calculate the maximum playback speed.
--
-- ObjC selector: @- setNominalFrameRate:@
setNominalFrameRate :: IsMETrackInfo meTrackInfo => meTrackInfo -> CFloat -> IO ()
setNominalFrameRate meTrackInfo  value =
    sendMsg meTrackInfo (mkSelector "setNominalFrameRate:") retVoid [argCFloat value]

-- | requiresFrameReordering
--
-- Indicates whether frame reordering occurs in the track.
--
-- The value is YES if frame reordering occurs, NO otherwise. This property is only valid for tracks with video media type and should return NO for if implemented for other track types.
--
-- ObjC selector: @- requiresFrameReordering@
requiresFrameReordering :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO Bool
requiresFrameReordering meTrackInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meTrackInfo (mkSelector "requiresFrameReordering") retCULong []

-- | requiresFrameReordering
--
-- Indicates whether frame reordering occurs in the track.
--
-- The value is YES if frame reordering occurs, NO otherwise. This property is only valid for tracks with video media type and should return NO for if implemented for other track types.
--
-- ObjC selector: @- setRequiresFrameReordering:@
setRequiresFrameReordering :: IsMETrackInfo meTrackInfo => meTrackInfo -> Bool -> IO ()
setRequiresFrameReordering meTrackInfo  value =
    sendMsg meTrackInfo (mkSelector "setRequiresFrameReordering:") retVoid [argCULong (if value then 1 else 0)]

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- This property may be used by the MediaToolbox to group similar language tracks together or to match audio and caption tracks. If no language tag is indicated, this property should be set to nil.
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id NSString)
extendedLanguageTag meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "extendedLanguageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- This property may be used by the MediaToolbox to group similar language tracks together or to match audio and caption tracks. If no language tag is indicated, this property should be set to nil.
--
-- ObjC selector: @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsMETrackInfo meTrackInfo, IsNSString value) => meTrackInfo -> value -> IO ()
setExtendedLanguageTag meTrackInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg meTrackInfo (mkSelector "setExtendedLanguageTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | naturalTimescale
--
-- The natural timescale of the track, as a CMTimeScale value.
--
-- ObjC selector: @- naturalTimescale@
naturalTimescale :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO CInt
naturalTimescale meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "naturalTimescale") retCInt []

-- | naturalTimescale
--
-- The natural timescale of the track, as a CMTimeScale value.
--
-- ObjC selector: @- setNaturalTimescale:@
setNaturalTimescale :: IsMETrackInfo meTrackInfo => meTrackInfo -> CInt -> IO ()
setNaturalTimescale meTrackInfo  value =
    sendMsg meTrackInfo (mkSelector "setNaturalTimescale:") retVoid [argCInt value]

-- | trackEdits
--
-- Returns the array of edit segments for the given track.
--
-- Each NSValue in the array contains a CMTimeMapping object describing the track edit. The CMTimeMapping.target time ranges for successive edits must partition the time range from 0 to the track's duration. In other words, for edit index = 0 the CMTimeMapping.target.start must be kCMTimeZero, while for edit index > 0, the CMTimeMapping.target.start must match the CMTimeRangeGetEnd(CMTimeMapping.target) for edit (index - 1). It is valid for a track to have an empty trackEdits array; this means that there is nothing at all in the track and the track duration is zero. If this property is implemented for media asset formats that do not support edit segments, it can return nil.
--
-- ObjC selector: @- trackEdits@
trackEdits :: IsMETrackInfo meTrackInfo => meTrackInfo -> IO (Id NSArray)
trackEdits meTrackInfo  =
    sendMsg meTrackInfo (mkSelector "trackEdits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | trackEdits
--
-- Returns the array of edit segments for the given track.
--
-- Each NSValue in the array contains a CMTimeMapping object describing the track edit. The CMTimeMapping.target time ranges for successive edits must partition the time range from 0 to the track's duration. In other words, for edit index = 0 the CMTimeMapping.target.start must be kCMTimeZero, while for edit index > 0, the CMTimeMapping.target.start must match the CMTimeRangeGetEnd(CMTimeMapping.target) for edit (index - 1). It is valid for a track to have an empty trackEdits array; this means that there is nothing at all in the track and the track duration is zero. If this property is implemented for media asset formats that do not support edit segments, it can return nil.
--
-- ObjC selector: @- setTrackEdits:@
setTrackEdits :: (IsMETrackInfo meTrackInfo, IsNSArray value) => meTrackInfo -> value -> IO ()
setTrackEdits meTrackInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg meTrackInfo (mkSelector "setTrackEdits:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMediaType:trackID:formatDescriptions:@
initWithMediaType_trackID_formatDescriptionsSelector :: Selector
initWithMediaType_trackID_formatDescriptionsSelector = mkSelector "initWithMediaType:trackID:formatDescriptions:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @formatDescriptions@
formatDescriptionsSelector :: Selector
formatDescriptionsSelector = mkSelector "formatDescriptions"

-- | @Selector@ for @nominalFrameRate@
nominalFrameRateSelector :: Selector
nominalFrameRateSelector = mkSelector "nominalFrameRate"

-- | @Selector@ for @setNominalFrameRate:@
setNominalFrameRateSelector :: Selector
setNominalFrameRateSelector = mkSelector "setNominalFrameRate:"

-- | @Selector@ for @requiresFrameReordering@
requiresFrameReorderingSelector :: Selector
requiresFrameReorderingSelector = mkSelector "requiresFrameReordering"

-- | @Selector@ for @setRequiresFrameReordering:@
setRequiresFrameReorderingSelector :: Selector
setRequiresFrameReorderingSelector = mkSelector "setRequiresFrameReordering:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

-- | @Selector@ for @naturalTimescale@
naturalTimescaleSelector :: Selector
naturalTimescaleSelector = mkSelector "naturalTimescale"

-- | @Selector@ for @setNaturalTimescale:@
setNaturalTimescaleSelector :: Selector
setNaturalTimescaleSelector = mkSelector "setNaturalTimescale:"

-- | @Selector@ for @trackEdits@
trackEditsSelector :: Selector
trackEditsSelector = mkSelector "trackEdits"

-- | @Selector@ for @setTrackEdits:@
setTrackEditsSelector :: Selector
setTrackEditsSelector = mkSelector "setTrackEdits:"

