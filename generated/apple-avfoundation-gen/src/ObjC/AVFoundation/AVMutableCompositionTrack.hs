{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableCompositionTrack@.
module ObjC.AVFoundation.AVMutableCompositionTrack
  ( AVMutableCompositionTrack
  , IsAVMutableCompositionTrack(..)
  , validateTrackSegments_error
  , addTrackAssociationToTrack_type
  , removeTrackAssociationToTrack_type
  , replaceFormatDescription_withFormatDescription
  , enabled
  , setEnabled
  , naturalTimeScale
  , setNaturalTimeScale
  , languageCode
  , setLanguageCode
  , extendedLanguageTag
  , setExtendedLanguageTag
  , preferredVolume
  , setPreferredVolume
  , segments
  , setSegments
  , addTrackAssociationToTrack_typeSelector
  , enabledSelector
  , extendedLanguageTagSelector
  , languageCodeSelector
  , naturalTimeScaleSelector
  , preferredVolumeSelector
  , removeTrackAssociationToTrack_typeSelector
  , replaceFormatDescription_withFormatDescriptionSelector
  , segmentsSelector
  , setEnabledSelector
  , setExtendedLanguageTagSelector
  , setLanguageCodeSelector
  , setNaturalTimeScaleSelector
  , setPreferredVolumeSelector
  , setSegmentsSelector
  , validateTrackSegments_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | validateTrackSegments:error:
--
-- Tests an array of AVCompositionTrackSegments to determine whether they conform to the timing rules noted above (see the property key "trackSegments").
--
-- @trackSegments@ — The array of AVCompositionTrackSegments to be validated.
--
-- @error@ — If validation fais, returns information about the failure.
--
-- Returns: YES if validation suceeds, otherwise NO.
--
-- The array is tested for suitability for setting as the value of the trackSegments property. If a portion of an existing trackSegments array is to be modified, the modification can be made via an instance of NSMutableArray, and the resulting array can be tested via -validateTrackSegments:error:.
--
-- ObjC selector: @- validateTrackSegments:error:@
validateTrackSegments_error :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSArray trackSegments, IsNSError outError) => avMutableCompositionTrack -> trackSegments -> outError -> IO Bool
validateTrackSegments_error avMutableCompositionTrack trackSegments outError =
  sendMessage avMutableCompositionTrack validateTrackSegments_errorSelector (toNSArray trackSegments) (toNSError outError)

-- | addTrackAssociationToTrack:type:
--
-- Establishes a track association of a specific type between two tracks.
--
-- @compositionTrack@ — An AVCompositionTrack object that is to be associated with the receiver.
--
-- @trackAssociationType@ — The type of track association to add between the receiver and the specified compositionTrack (for instance, AVTrackAssociationTypeChapterList).
--
-- ObjC selector: @- addTrackAssociationToTrack:type:@
addTrackAssociationToTrack_type :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsAVCompositionTrack compositionTrack, IsNSString trackAssociationType) => avMutableCompositionTrack -> compositionTrack -> trackAssociationType -> IO ()
addTrackAssociationToTrack_type avMutableCompositionTrack compositionTrack trackAssociationType =
  sendMessage avMutableCompositionTrack addTrackAssociationToTrack_typeSelector (toAVCompositionTrack compositionTrack) (toNSString trackAssociationType)

-- | removeTrackAssociationToTrack:type:
--
-- Removes a track association of a specific type between two tracks.
--
-- @compositionTrack@ — An AVCompositionTrack object that is associated with the receiver.
--
-- @trackAssociationType@ — The type of track association to remove between the receiver and the specified compositionTrack (for instance, AVTrackAssociationTypeChapterList).
--
-- ObjC selector: @- removeTrackAssociationToTrack:type:@
removeTrackAssociationToTrack_type :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsAVCompositionTrack compositionTrack, IsNSString trackAssociationType) => avMutableCompositionTrack -> compositionTrack -> trackAssociationType -> IO ()
removeTrackAssociationToTrack_type avMutableCompositionTrack compositionTrack trackAssociationType =
  sendMessage avMutableCompositionTrack removeTrackAssociationToTrack_typeSelector (toAVCompositionTrack compositionTrack) (toNSString trackAssociationType)

-- | replaceFormatDescription:withFormatDescription:
--
-- Replaces one of the receiver's format descriptions with another format description or cancels a previous replacement.
--
-- @originalFormatDescription@ — A CMFormatDescription occurring in the underlying asset track.
--
-- @replacementFormatDescription@ — A CMFormatDescription to replace the specified format description or NULL to indicate that a previous replacement of originalFormatDescription should be cancelled.
--
-- You can use this method to make surgical changes to a track's format descriptions, such as adding format description extensions to a format description or changing the audio channel layout of an audio track. You should note that a format description can have extensions of type kCMFormatDescriptionExtension_VerbatimSampleDescription and kCMFormatDescriptionExtension_VerbatimISOSampleEntry; if you modify a copy of a format description, you should delete those extensions from the copy or your changes might be ignored. Also note that format description replacements are not transferred when performing editing operations on AVMutableCompositionTrack objects; for instance, inserting a range of a composition track into another composition track does not transfer any replacement format descriptions.					This method throws an exception if the media type of the replacement does not match the original format description.
--
-- ObjC selector: @- replaceFormatDescription:withFormatDescription:@
replaceFormatDescription_withFormatDescription :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> RawId -> RawId -> IO ()
replaceFormatDescription_withFormatDescription avMutableCompositionTrack originalFormatDescription replacementFormatDescription =
  sendMessage avMutableCompositionTrack replaceFormatDescription_withFormatDescriptionSelector originalFormatDescription replacementFormatDescription

-- | enabled
--
-- Specifies whether the track is enabled or disabled.  Default is YES.
--
-- ObjC selector: @- enabled@
enabled :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO Bool
enabled avMutableCompositionTrack =
  sendMessage avMutableCompositionTrack enabledSelector

-- | enabled
--
-- Specifies whether the track is enabled or disabled.  Default is YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> Bool -> IO ()
setEnabled avMutableCompositionTrack value =
  sendMessage avMutableCompositionTrack setEnabledSelector value

-- | naturalTimeScale
--
-- Indicates a timescale in which time values for the track can be operated upon without extraneous numerical conversion.
--
-- If not set, the value is the naturalTimeScale of the first non-empty edit, or 600 if there are no non-empty edits.      Set to 0 to revert to default behavior.
--
-- ObjC selector: @- naturalTimeScale@
naturalTimeScale :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO CInt
naturalTimeScale avMutableCompositionTrack =
  sendMessage avMutableCompositionTrack naturalTimeScaleSelector

-- | naturalTimeScale
--
-- Indicates a timescale in which time values for the track can be operated upon without extraneous numerical conversion.
--
-- If not set, the value is the naturalTimeScale of the first non-empty edit, or 600 if there are no non-empty edits.      Set to 0 to revert to default behavior.
--
-- ObjC selector: @- setNaturalTimeScale:@
setNaturalTimeScale :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> CInt -> IO ()
setNaturalTimeScale avMutableCompositionTrack value =
  sendMessage avMutableCompositionTrack setNaturalTimeScaleSelector value

-- | languageCode
--
-- Indicates the language associated with the track, as an ISO 639-2/T language code.
--
-- The default value is nil.
--
-- ObjC selector: @- languageCode@
languageCode :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO (Id NSString)
languageCode avMutableCompositionTrack =
  sendMessage avMutableCompositionTrack languageCodeSelector

-- | languageCode
--
-- Indicates the language associated with the track, as an ISO 639-2/T language code.
--
-- The default value is nil.
--
-- ObjC selector: @- setLanguageCode:@
setLanguageCode :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSString value) => avMutableCompositionTrack -> value -> IO ()
setLanguageCode avMutableCompositionTrack value =
  sendMessage avMutableCompositionTrack setLanguageCodeSelector (toNSString value)

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- The default value is nil.
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO (Id NSString)
extendedLanguageTag avMutableCompositionTrack =
  sendMessage avMutableCompositionTrack extendedLanguageTagSelector

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- The default value is nil.
--
-- ObjC selector: @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSString value) => avMutableCompositionTrack -> value -> IO ()
setExtendedLanguageTag avMutableCompositionTrack value =
  sendMessage avMutableCompositionTrack setExtendedLanguageTagSelector (toNSString value)

-- | preferredVolume
--
-- The preferred volume of the audible media data.
--
-- The default value is 1.0.
--
-- ObjC selector: @- preferredVolume@
preferredVolume :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO CFloat
preferredVolume avMutableCompositionTrack =
  sendMessage avMutableCompositionTrack preferredVolumeSelector

-- | preferredVolume
--
-- The preferred volume of the audible media data.
--
-- The default value is 1.0.
--
-- ObjC selector: @- setPreferredVolume:@
setPreferredVolume :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> CFloat -> IO ()
setPreferredVolume avMutableCompositionTrack value =
  sendMessage avMutableCompositionTrack setPreferredVolumeSelector value

-- | segments
--
-- Provides read/write access to the array of track segments, each an instance of AVCompositionTrackSegment.
--
-- Note that timeMapping.target.start of the first AVCompositionTrackSegment must be kCMTimeZero, and the timeMapping.target.start of each subsequent AVCompositionTrackSegment must equal CMTimeRangeGetEnd(the previous AVCompositionTrackSegment's timeMapping.target).      Use -validateTrackSegments:error: to perform a test to ensure that an array of AVCompositionTrackSegments conforms to this rule.
--
-- ObjC selector: @- segments@
segments :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO (Id NSArray)
segments avMutableCompositionTrack =
  sendMessage avMutableCompositionTrack segmentsSelector

-- | segments
--
-- Provides read/write access to the array of track segments, each an instance of AVCompositionTrackSegment.
--
-- Note that timeMapping.target.start of the first AVCompositionTrackSegment must be kCMTimeZero, and the timeMapping.target.start of each subsequent AVCompositionTrackSegment must equal CMTimeRangeGetEnd(the previous AVCompositionTrackSegment's timeMapping.target).      Use -validateTrackSegments:error: to perform a test to ensure that an array of AVCompositionTrackSegments conforms to this rule.
--
-- ObjC selector: @- setSegments:@
setSegments :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSArray value) => avMutableCompositionTrack -> value -> IO ()
setSegments avMutableCompositionTrack value =
  sendMessage avMutableCompositionTrack setSegmentsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @validateTrackSegments:error:@
validateTrackSegments_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
validateTrackSegments_errorSelector = mkSelector "validateTrackSegments:error:"

-- | @Selector@ for @addTrackAssociationToTrack:type:@
addTrackAssociationToTrack_typeSelector :: Selector '[Id AVCompositionTrack, Id NSString] ()
addTrackAssociationToTrack_typeSelector = mkSelector "addTrackAssociationToTrack:type:"

-- | @Selector@ for @removeTrackAssociationToTrack:type:@
removeTrackAssociationToTrack_typeSelector :: Selector '[Id AVCompositionTrack, Id NSString] ()
removeTrackAssociationToTrack_typeSelector = mkSelector "removeTrackAssociationToTrack:type:"

-- | @Selector@ for @replaceFormatDescription:withFormatDescription:@
replaceFormatDescription_withFormatDescriptionSelector :: Selector '[RawId, RawId] ()
replaceFormatDescription_withFormatDescriptionSelector = mkSelector "replaceFormatDescription:withFormatDescription:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @naturalTimeScale@
naturalTimeScaleSelector :: Selector '[] CInt
naturalTimeScaleSelector = mkSelector "naturalTimeScale"

-- | @Selector@ for @setNaturalTimeScale:@
setNaturalTimeScaleSelector :: Selector '[CInt] ()
setNaturalTimeScaleSelector = mkSelector "setNaturalTimeScale:"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector '[] (Id NSString)
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector '[Id NSString] ()
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector '[] (Id NSString)
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector '[Id NSString] ()
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

-- | @Selector@ for @preferredVolume@
preferredVolumeSelector :: Selector '[] CFloat
preferredVolumeSelector = mkSelector "preferredVolume"

-- | @Selector@ for @setPreferredVolume:@
setPreferredVolumeSelector :: Selector '[CFloat] ()
setPreferredVolumeSelector = mkSelector "setPreferredVolume:"

-- | @Selector@ for @segments@
segmentsSelector :: Selector '[] (Id NSArray)
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @setSegments:@
setSegmentsSelector :: Selector '[Id NSArray] ()
setSegmentsSelector = mkSelector "setSegments:"

