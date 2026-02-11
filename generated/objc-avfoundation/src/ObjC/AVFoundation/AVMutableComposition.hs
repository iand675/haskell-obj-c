{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableComposition@.
module ObjC.AVFoundation.AVMutableComposition
  ( AVMutableComposition
  , IsAVMutableComposition(..)
  , composition
  , compositionWithURLAssetInitializationOptions
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , addMutableTrackWithMediaType_preferredTrackID
  , removeTrack
  , mutableTrackCompatibleWithTrack
  , tracks
  , compositionSelector
  , compositionWithURLAssetInitializationOptionsSelector
  , trackWithTrackIDSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , tracksWithMediaTypeSelector
  , tracksWithMediaCharacteristicSelector
  , addMutableTrackWithMediaType_preferredTrackIDSelector
  , removeTrackSelector
  , mutableTrackCompatibleWithTrackSelector
  , tracksSelector


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

-- | composition
--
-- Returns an empty AVMutableComposition.
--
-- ObjC selector: @+ composition@
composition :: IO (Id AVMutableComposition)
composition  =
  do
    cls' <- getRequiredClass "AVMutableComposition"
    sendClassMsg cls' (mkSelector "composition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | compositionWithURLAssetInitializationOptions:
--
-- Returns an empty AVMutableComposition.
--
-- @URLAssetInitializationOptions@ — Specifies the initialization options that the receiver should use when creating AVURLAssets internally, e.g. AVURLAssetPreferPreciseDurationAndTimingKey. The default behavior for creation of AVURLAssets by an AVMutableComposition is equivalent to the behavior of +[AVURLAsset URLAssetWithURL:options:] when specifying no initialization options.
--
-- AVMutableCompositions create AVURLAssets internally for URLs specified by AVCompositionTrackSegments of AVMutableCompositionTracks, as needed, whenever AVCompositionTrackSegments are added to tracks via -[AVMutableCompositionTrack setSegments:] rather than by inserting timeranges of already existing AVAssets or AVAssetTracks.
--
-- ObjC selector: @+ compositionWithURLAssetInitializationOptions:@
compositionWithURLAssetInitializationOptions :: IsNSDictionary urlAssetInitializationOptions => urlAssetInitializationOptions -> IO (Id AVMutableComposition)
compositionWithURLAssetInitializationOptions urlAssetInitializationOptions =
  do
    cls' <- getRequiredClass "AVMutableComposition"
    withObjCPtr urlAssetInitializationOptions $ \raw_urlAssetInitializationOptions ->
      sendClassMsg cls' (mkSelector "compositionWithURLAssetInitializationOptions:") (retPtr retVoid) [argPtr (castPtr raw_urlAssetInitializationOptions :: Ptr ())] >>= retainedObject . castPtr

-- | trackWithTrackID:
--
-- Provides an instance of AVMutableCompositionTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVMutableCompositionTrack.
--
-- Returns: An instance of AVMutableCompositionTrack; may be nil if no track of the specified trackID is available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVMutableComposition avMutableComposition => avMutableComposition -> CInt -> IO (Id AVMutableCompositionTrack)
trackWithTrackID avMutableComposition  trackID =
  sendMsg avMutableComposition (mkSelector "trackWithTrackID:") (retPtr retVoid) [argCInt (fromIntegral trackID)] >>= retainedObject . castPtr

-- | loadTrackWithTrackID:completionHandler:
--
-- Loads an instance of AVMutableCompositionTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVMutableCompositionTrack.
--
-- @completionHandler@ — A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVMutableComposition avMutableComposition => avMutableComposition -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avMutableComposition  trackID completionHandler =
  sendMsg avMutableComposition (mkSelector "loadTrackWithTrackID:completionHandler:") retVoid [argCInt (fromIntegral trackID), argPtr (castPtr completionHandler :: Ptr ())]

-- | tracksWithMediaType:
--
-- Provides an array of AVMutableCompositionTracks of the asset that present media of the specified media type.
--
-- @mediaType@ — The media type according to which the receiver filters its AVMutableCompositionTracks. (Media types are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVMutableCompositionTracks; may be empty if no tracks of the specified media type are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVMutableComposition avMutableComposition, IsNSString mediaType) => avMutableComposition -> mediaType -> IO (Id NSArray)
tracksWithMediaType avMutableComposition  mediaType =
withObjCPtr mediaType $ \raw_mediaType ->
    sendMsg avMutableComposition (mkSelector "tracksWithMediaType:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ())] >>= retainedObject . castPtr

-- | tracksWithMediaCharacteristic:
--
-- Provides an array of AVMutableCompositionTracks of the asset that present media with the specified characteristic.
--
-- @mediaCharacteristic@ — The media characteristic according to which the receiver filters its AVMutableCompositionTracks. (Media characteristics are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVMutableCompositionTracks; may be empty if no tracks with the specified characteristic are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVMutableComposition avMutableComposition, IsNSString mediaCharacteristic) => avMutableComposition -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avMutableComposition  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    sendMsg avMutableComposition (mkSelector "tracksWithMediaCharacteristic:") (retPtr retVoid) [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())] >>= retainedObject . castPtr

-- | addMutableTrackWithMediaType:preferredTrackID:
--
-- Adds an empty track to a mutable composition.
--
-- @mediaType@ — The media type of the new track.
--
-- @preferredTrackID@ — Specifies the preferred track ID for the new track. If you do not need to specify a preferred track ID, pass kCMPersistentTrackID_Invalid. Otherwise the preferred track ID will be used for the new track, provided that it is not currently in use and has not previously been used.
--
-- Returns: An instance of AVMutableCompositionTrack representing the new track. Its actual trackID is available via its "trackID" key.
--
-- If the specified preferred track ID is not available, or kCMPersistentTrackID_Invalid was passed in, a unique track ID will be generated.
--
-- ObjC selector: @- addMutableTrackWithMediaType:preferredTrackID:@
addMutableTrackWithMediaType_preferredTrackID :: (IsAVMutableComposition avMutableComposition, IsNSString mediaType) => avMutableComposition -> mediaType -> CInt -> IO (Id AVMutableCompositionTrack)
addMutableTrackWithMediaType_preferredTrackID avMutableComposition  mediaType preferredTrackID =
withObjCPtr mediaType $ \raw_mediaType ->
    sendMsg avMutableComposition (mkSelector "addMutableTrackWithMediaType:preferredTrackID:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ()), argCInt (fromIntegral preferredTrackID)] >>= retainedObject . castPtr

-- | removeTrack:
--
-- Removes a track of a mutable composition.
--
-- @track@ — A reference to the AVCompositionTrack to be removed.
--
-- If you retain a reference to the removed track, note that its "composition" key will have the value nil, and the values of its other properties are undefined.
--
-- ObjC selector: @- removeTrack:@
removeTrack :: (IsAVMutableComposition avMutableComposition, IsAVCompositionTrack track) => avMutableComposition -> track -> IO ()
removeTrack avMutableComposition  track =
withObjCPtr track $ \raw_track ->
    sendMsg avMutableComposition (mkSelector "removeTrack:") retVoid [argPtr (castPtr raw_track :: Ptr ())]

-- | mutableTrackCompatibleWithTrack:
--
-- Provides a reference to a track of a mutable composition into which any timeRange of an AVAssetTrack can be inserted (via -[AVMutableCompositionTrack insertTimeRange:ofTrack:atTime:error:]).
--
-- @track@ — A reference to the AVAssetTrack from which a timeRange may be inserted.
--
-- Returns: An AVMutableCompositionTrack that can accommodate the insertion, or, if no such track is available, nil.
--
-- If a compatible track is desired but the result of this method is nil, a new track of the same mediaType as the AVAssetTrack can be created via -addMutableTrackWithMediaType:preferredTrackID:, and this new track will be compatible.
--
-- For best performance, the number of tracks of a composition should be kept to a minimum, corresponding to the number for which media data must be presented in parallel. If media data of the same type is to be presented serially, even from multiple assets, a single track of that media type should be used. This method, -mutableTrackCompatibleWithTrack:, can help the client to identify an existing target track for an insertion.
--
-- Similar to -[AVAsset compatibleTrackForCompositionTrack:].
--
-- ObjC selector: @- mutableTrackCompatibleWithTrack:@
mutableTrackCompatibleWithTrack :: (IsAVMutableComposition avMutableComposition, IsAVAssetTrack track) => avMutableComposition -> track -> IO (Id AVMutableCompositionTrack)
mutableTrackCompatibleWithTrack avMutableComposition  track =
withObjCPtr track $ \raw_track ->
    sendMsg avMutableComposition (mkSelector "mutableTrackCompatibleWithTrack:") (retPtr retVoid) [argPtr (castPtr raw_track :: Ptr ())] >>= retainedObject . castPtr

-- | tracks
--
-- Provides the array of AVMutableCompositionTracks contained by the composition.
--
-- ObjC selector: @- tracks@
tracks :: IsAVMutableComposition avMutableComposition => avMutableComposition -> IO (Id NSArray)
tracks avMutableComposition  =
  sendMsg avMutableComposition (mkSelector "tracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @composition@
compositionSelector :: Selector
compositionSelector = mkSelector "composition"

-- | @Selector@ for @compositionWithURLAssetInitializationOptions:@
compositionWithURLAssetInitializationOptionsSelector :: Selector
compositionWithURLAssetInitializationOptionsSelector = mkSelector "compositionWithURLAssetInitializationOptions:"

-- | @Selector@ for @trackWithTrackID:@
trackWithTrackIDSelector :: Selector
trackWithTrackIDSelector = mkSelector "trackWithTrackID:"

-- | @Selector@ for @loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandlerSelector :: Selector
loadTrackWithTrackID_completionHandlerSelector = mkSelector "loadTrackWithTrackID:completionHandler:"

-- | @Selector@ for @tracksWithMediaType:@
tracksWithMediaTypeSelector :: Selector
tracksWithMediaTypeSelector = mkSelector "tracksWithMediaType:"

-- | @Selector@ for @tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristicSelector :: Selector
tracksWithMediaCharacteristicSelector = mkSelector "tracksWithMediaCharacteristic:"

-- | @Selector@ for @addMutableTrackWithMediaType:preferredTrackID:@
addMutableTrackWithMediaType_preferredTrackIDSelector :: Selector
addMutableTrackWithMediaType_preferredTrackIDSelector = mkSelector "addMutableTrackWithMediaType:preferredTrackID:"

-- | @Selector@ for @removeTrack:@
removeTrackSelector :: Selector
removeTrackSelector = mkSelector "removeTrack:"

-- | @Selector@ for @mutableTrackCompatibleWithTrack:@
mutableTrackCompatibleWithTrackSelector :: Selector
mutableTrackCompatibleWithTrackSelector = mkSelector "mutableTrackCompatibleWithTrack:"

-- | @Selector@ for @tracks@
tracksSelector :: Selector
tracksSelector = mkSelector "tracks"

