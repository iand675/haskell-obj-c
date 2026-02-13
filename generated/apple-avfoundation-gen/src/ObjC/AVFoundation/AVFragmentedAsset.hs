{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A subclass of AVURLAsset that represents media resources that can be extended in total duration without modifying previously existing data structures.
--
-- Such media resources include QuickTime movie files and MPEG-4 files that indicate, via an 'mvex' box in their 'moov' box, that they accommodate additional fragments. Media resources of other types may also be supported. To check whether a given instance of AVFragmentedAsset can be used to monitor the addition of fragments, check the value of the AVURLAsset property canContainFragments.
--
-- An AVFragmentedAsset is capable of changing the values of certain of its properties and those of its tracks, while an operation that appends fragments to the underlying media resource in in progress, if the AVFragmentedAsset is associated with an instance of AVFragmentedAssetMinder.
--
-- While associated with an AVFragmentedAssetMinder, AVFragmentedAsset posts AVAssetDurationDidChangeNotification whenever new fragments are detected, as appropriate. It may also post AVAssetContainsFragmentsDidChangeNotification and AVAssetWasDefragmentedNotification, as discussed in documentation of those notifications. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVFragmentedAsset@.
module ObjC.AVFoundation.AVFragmentedAsset
  ( AVFragmentedAsset
  , IsAVFragmentedAsset(..)
  , fragmentedAssetWithURL_options
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , tracks
  , fragmentedAssetWithURL_optionsSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , trackWithTrackIDSelector
  , tracksSelector
  , tracksWithMediaCharacteristicSelector
  , tracksWithMediaTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns an instance of AVFragmentedAsset for inspection of a fragmented media resource.
--
-- - Parameter URL: An instance of NSURL that references a media resource. - Parameter options: An instance of NSDictionary that contains keys for specifying options for the initialization of the AVFragmentedAsset. See AVURLAssetPreferPreciseDurationAndTimingKey and AVURLAssetReferenceRestrictionsKey above.
--
-- - Returns: An instance of AVFragmentedAsset.
--
-- ObjC selector: @+ fragmentedAssetWithURL:options:@
fragmentedAssetWithURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id AVFragmentedAsset)
fragmentedAssetWithURL_options url options =
  do
    cls' <- getRequiredClass "AVFragmentedAsset"
    sendClassMessage cls' fragmentedAssetWithURL_optionsSelector (toNSURL url) (toNSDictionary options)

-- | Provides an instance of AVFragmentedAssetTrack that represents the track of the specified trackID.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- - Parameter trackID: The trackID of the requested AVFragmentedAssetTrack.
--
-- - Returns: An instance of AVFragmentedAssetTrack; may be nil if no track of the specified trackID is available.
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVFragmentedAsset avFragmentedAsset => avFragmentedAsset -> CInt -> IO (Id AVFragmentedAssetTrack)
trackWithTrackID avFragmentedAsset trackID =
  sendMessage avFragmentedAsset trackWithTrackIDSelector trackID

-- | Loads an instance of AVFragmentedAssetTrack that represents the track of the specified trackID.
--
-- - Parameter trackID: The trackID of the requested AVFragmentedAssetTrack. - Parameter completionHandler: A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVFragmentedAsset avFragmentedAsset => avFragmentedAsset -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avFragmentedAsset trackID completionHandler =
  sendMessage avFragmentedAsset loadTrackWithTrackID_completionHandlerSelector trackID completionHandler

-- | Provides an array of AVFragmentedAssetTracks of the asset that present media of the specified media type.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- - Parameter mediaType: The media type according to which the receiver filters its AVFragmentedAssetTracks. (Media types are defined in AVMediaFormat.h)
--
-- - Returns: An NSArray of AVFragmentedAssetTracks; may be empty if no tracks of the specified media type are available.
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVFragmentedAsset avFragmentedAsset, IsNSString mediaType) => avFragmentedAsset -> mediaType -> IO (Id NSArray)
tracksWithMediaType avFragmentedAsset mediaType =
  sendMessage avFragmentedAsset tracksWithMediaTypeSelector (toNSString mediaType)

-- | Provides an array of AVFragmentedAssetTracks of the asset that present media with the specified characteristic.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- - Parameter mediaCharacteristic: The media characteristic according to which the receiver filters its AVFragmentedAssetTracks. (Media characteristics are defined in AVMediaFormat.h)
--
-- - Returns: An NSArray of AVFragmentedAssetTracks; may be empty if no tracks with the specified characteristic are available.
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVFragmentedAsset avFragmentedAsset, IsNSString mediaCharacteristic) => avFragmentedAsset -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avFragmentedAsset mediaCharacteristic =
  sendMessage avFragmentedAsset tracksWithMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | The tracks in an asset.
--
-- The value of this property is an array of tracks the asset contains; the tracks are of type AVFragmentedAssetTrack.
--
-- ObjC selector: @- tracks@
tracks :: IsAVFragmentedAsset avFragmentedAsset => avFragmentedAsset -> IO (Id NSArray)
tracks avFragmentedAsset =
  sendMessage avFragmentedAsset tracksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentedAssetWithURL:options:@
fragmentedAssetWithURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id AVFragmentedAsset)
fragmentedAssetWithURL_optionsSelector = mkSelector "fragmentedAssetWithURL:options:"

-- | @Selector@ for @trackWithTrackID:@
trackWithTrackIDSelector :: Selector '[CInt] (Id AVFragmentedAssetTrack)
trackWithTrackIDSelector = mkSelector "trackWithTrackID:"

-- | @Selector@ for @loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandlerSelector :: Selector '[CInt, Ptr ()] ()
loadTrackWithTrackID_completionHandlerSelector = mkSelector "loadTrackWithTrackID:completionHandler:"

-- | @Selector@ for @tracksWithMediaType:@
tracksWithMediaTypeSelector :: Selector '[Id NSString] (Id NSArray)
tracksWithMediaTypeSelector = mkSelector "tracksWithMediaType:"

-- | @Selector@ for @tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristicSelector :: Selector '[Id NSString] (Id NSArray)
tracksWithMediaCharacteristicSelector = mkSelector "tracksWithMediaCharacteristic:"

-- | @Selector@ for @tracks@
tracksSelector :: Selector '[] (Id NSArray)
tracksSelector = mkSelector "tracks"

