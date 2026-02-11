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
  , validateTrackSegments_errorSelector
  , addTrackAssociationToTrack_typeSelector
  , removeTrackAssociationToTrack_typeSelector
  , replaceFormatDescription_withFormatDescriptionSelector
  , enabledSelector
  , setEnabledSelector
  , naturalTimeScaleSelector
  , setNaturalTimeScaleSelector
  , languageCodeSelector
  , setLanguageCodeSelector
  , extendedLanguageTagSelector
  , setExtendedLanguageTagSelector
  , preferredVolumeSelector
  , setPreferredVolumeSelector
  , segmentsSelector
  , setSegmentsSelector


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
validateTrackSegments_error avMutableCompositionTrack  trackSegments outError =
withObjCPtr trackSegments $ \raw_trackSegments ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableCompositionTrack (mkSelector "validateTrackSegments:error:") retCULong [argPtr (castPtr raw_trackSegments :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

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
addTrackAssociationToTrack_type avMutableCompositionTrack  compositionTrack trackAssociationType =
withObjCPtr compositionTrack $ \raw_compositionTrack ->
  withObjCPtr trackAssociationType $ \raw_trackAssociationType ->
      sendMsg avMutableCompositionTrack (mkSelector "addTrackAssociationToTrack:type:") retVoid [argPtr (castPtr raw_compositionTrack :: Ptr ()), argPtr (castPtr raw_trackAssociationType :: Ptr ())]

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
removeTrackAssociationToTrack_type avMutableCompositionTrack  compositionTrack trackAssociationType =
withObjCPtr compositionTrack $ \raw_compositionTrack ->
  withObjCPtr trackAssociationType $ \raw_trackAssociationType ->
      sendMsg avMutableCompositionTrack (mkSelector "removeTrackAssociationToTrack:type:") retVoid [argPtr (castPtr raw_compositionTrack :: Ptr ()), argPtr (castPtr raw_trackAssociationType :: Ptr ())]

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
replaceFormatDescription_withFormatDescription avMutableCompositionTrack  originalFormatDescription replacementFormatDescription =
  sendMsg avMutableCompositionTrack (mkSelector "replaceFormatDescription:withFormatDescription:") retVoid [argPtr (castPtr (unRawId originalFormatDescription) :: Ptr ()), argPtr (castPtr (unRawId replacementFormatDescription) :: Ptr ())]

-- | enabled
--
-- Specifies whether the track is enabled or disabled.  Default is YES.
--
-- ObjC selector: @- enabled@
enabled :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO Bool
enabled avMutableCompositionTrack  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableCompositionTrack (mkSelector "enabled") retCULong []

-- | enabled
--
-- Specifies whether the track is enabled or disabled.  Default is YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> Bool -> IO ()
setEnabled avMutableCompositionTrack  value =
  sendMsg avMutableCompositionTrack (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | naturalTimeScale
--
-- Indicates a timescale in which time values for the track can be operated upon without extraneous numerical conversion.
--
-- If not set, the value is the naturalTimeScale of the first non-empty edit, or 600 if there are no non-empty edits.      Set to 0 to revert to default behavior.
--
-- ObjC selector: @- naturalTimeScale@
naturalTimeScale :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO CInt
naturalTimeScale avMutableCompositionTrack  =
  sendMsg avMutableCompositionTrack (mkSelector "naturalTimeScale") retCInt []

-- | naturalTimeScale
--
-- Indicates a timescale in which time values for the track can be operated upon without extraneous numerical conversion.
--
-- If not set, the value is the naturalTimeScale of the first non-empty edit, or 600 if there are no non-empty edits.      Set to 0 to revert to default behavior.
--
-- ObjC selector: @- setNaturalTimeScale:@
setNaturalTimeScale :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> CInt -> IO ()
setNaturalTimeScale avMutableCompositionTrack  value =
  sendMsg avMutableCompositionTrack (mkSelector "setNaturalTimeScale:") retVoid [argCInt (fromIntegral value)]

-- | languageCode
--
-- Indicates the language associated with the track, as an ISO 639-2/T language code.
--
-- The default value is nil.
--
-- ObjC selector: @- languageCode@
languageCode :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO (Id NSString)
languageCode avMutableCompositionTrack  =
  sendMsg avMutableCompositionTrack (mkSelector "languageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | languageCode
--
-- Indicates the language associated with the track, as an ISO 639-2/T language code.
--
-- The default value is nil.
--
-- ObjC selector: @- setLanguageCode:@
setLanguageCode :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSString value) => avMutableCompositionTrack -> value -> IO ()
setLanguageCode avMutableCompositionTrack  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableCompositionTrack (mkSelector "setLanguageCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- The default value is nil.
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO (Id NSString)
extendedLanguageTag avMutableCompositionTrack  =
  sendMsg avMutableCompositionTrack (mkSelector "extendedLanguageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | extendedLanguageTag
--
-- Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier.
--
-- The default value is nil.
--
-- ObjC selector: @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSString value) => avMutableCompositionTrack -> value -> IO ()
setExtendedLanguageTag avMutableCompositionTrack  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableCompositionTrack (mkSelector "setExtendedLanguageTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | preferredVolume
--
-- The preferred volume of the audible media data.
--
-- The default value is 1.0.
--
-- ObjC selector: @- preferredVolume@
preferredVolume :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO CFloat
preferredVolume avMutableCompositionTrack  =
  sendMsg avMutableCompositionTrack (mkSelector "preferredVolume") retCFloat []

-- | preferredVolume
--
-- The preferred volume of the audible media data.
--
-- The default value is 1.0.
--
-- ObjC selector: @- setPreferredVolume:@
setPreferredVolume :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> CFloat -> IO ()
setPreferredVolume avMutableCompositionTrack  value =
  sendMsg avMutableCompositionTrack (mkSelector "setPreferredVolume:") retVoid [argCFloat (fromIntegral value)]

-- | segments
--
-- Provides read/write access to the array of track segments, each an instance of AVCompositionTrackSegment.
--
-- Note that timeMapping.target.start of the first AVCompositionTrackSegment must be kCMTimeZero, and the timeMapping.target.start of each subsequent AVCompositionTrackSegment must equal CMTimeRangeGetEnd(the previous AVCompositionTrackSegment's timeMapping.target).      Use -validateTrackSegments:error: to perform a test to ensure that an array of AVCompositionTrackSegments conforms to this rule.
--
-- ObjC selector: @- segments@
segments :: IsAVMutableCompositionTrack avMutableCompositionTrack => avMutableCompositionTrack -> IO (Id NSArray)
segments avMutableCompositionTrack  =
  sendMsg avMutableCompositionTrack (mkSelector "segments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | segments
--
-- Provides read/write access to the array of track segments, each an instance of AVCompositionTrackSegment.
--
-- Note that timeMapping.target.start of the first AVCompositionTrackSegment must be kCMTimeZero, and the timeMapping.target.start of each subsequent AVCompositionTrackSegment must equal CMTimeRangeGetEnd(the previous AVCompositionTrackSegment's timeMapping.target).      Use -validateTrackSegments:error: to perform a test to ensure that an array of AVCompositionTrackSegments conforms to this rule.
--
-- ObjC selector: @- setSegments:@
setSegments :: (IsAVMutableCompositionTrack avMutableCompositionTrack, IsNSArray value) => avMutableCompositionTrack -> value -> IO ()
setSegments avMutableCompositionTrack  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableCompositionTrack (mkSelector "setSegments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @validateTrackSegments:error:@
validateTrackSegments_errorSelector :: Selector
validateTrackSegments_errorSelector = mkSelector "validateTrackSegments:error:"

-- | @Selector@ for @addTrackAssociationToTrack:type:@
addTrackAssociationToTrack_typeSelector :: Selector
addTrackAssociationToTrack_typeSelector = mkSelector "addTrackAssociationToTrack:type:"

-- | @Selector@ for @removeTrackAssociationToTrack:type:@
removeTrackAssociationToTrack_typeSelector :: Selector
removeTrackAssociationToTrack_typeSelector = mkSelector "removeTrackAssociationToTrack:type:"

-- | @Selector@ for @replaceFormatDescription:withFormatDescription:@
replaceFormatDescription_withFormatDescriptionSelector :: Selector
replaceFormatDescription_withFormatDescriptionSelector = mkSelector "replaceFormatDescription:withFormatDescription:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @naturalTimeScale@
naturalTimeScaleSelector :: Selector
naturalTimeScaleSelector = mkSelector "naturalTimeScale"

-- | @Selector@ for @setNaturalTimeScale:@
setNaturalTimeScaleSelector :: Selector
setNaturalTimeScaleSelector = mkSelector "setNaturalTimeScale:"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

-- | @Selector@ for @preferredVolume@
preferredVolumeSelector :: Selector
preferredVolumeSelector = mkSelector "preferredVolume"

-- | @Selector@ for @setPreferredVolume:@
setPreferredVolumeSelector :: Selector
setPreferredVolumeSelector = mkSelector "setPreferredVolume:"

-- | @Selector@ for @segments@
segmentsSelector :: Selector
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @setSegments:@
setSegmentsSelector :: Selector
setSegmentsSelector = mkSelector "setSegments:"

