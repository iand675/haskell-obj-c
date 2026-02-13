{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVFragmentedMovie@.
module ObjC.AVFoundation.AVFragmentedMovie
  ( AVFragmentedMovie
  , IsAVFragmentedMovie(..)
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , tracks
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

-- | trackWithTrackID:
--
-- Provides an instance of AVFragmentedMovieTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVFragmentedMovieTrack.
--
-- Returns: An instance of AVFragmentedMovieTrack; may be nil if no track of the specified trackID is available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVFragmentedMovie avFragmentedMovie => avFragmentedMovie -> CInt -> IO (Id AVFragmentedMovieTrack)
trackWithTrackID avFragmentedMovie trackID =
  sendMessage avFragmentedMovie trackWithTrackIDSelector trackID

-- | loadTrackWithTrackID:completionHandler:
--
-- Loads an instance of AVFragmentedMovieTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVFragmentedMovieTrack.
--
-- @completionHandler@ — A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVFragmentedMovie avFragmentedMovie => avFragmentedMovie -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avFragmentedMovie trackID completionHandler =
  sendMessage avFragmentedMovie loadTrackWithTrackID_completionHandlerSelector trackID completionHandler

-- | tracksWithMediaType:
--
-- Provides an array of AVFragmentedMovieTracks of the asset that present media of the specified media type.
--
-- @mediaType@ — The media type according to which the receiver filters its AVFragmentedMovieTracks. (Media types are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVFragmentedMovieTracks; may be empty if no tracks of the specified media type are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVFragmentedMovie avFragmentedMovie, IsNSString mediaType) => avFragmentedMovie -> mediaType -> IO (Id NSArray)
tracksWithMediaType avFragmentedMovie mediaType =
  sendMessage avFragmentedMovie tracksWithMediaTypeSelector (toNSString mediaType)

-- | tracksWithMediaCharacteristic:
--
-- Provides an array of AVFragmentedMovieTracks of the asset that present media with the specified characteristic.
--
-- @mediaCharacteristic@ — The media characteristic according to which the receiver filters its AVFragmentedMovieTracks. (Media characteristics are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVFragmentedMovieTracks; may be empty if no tracks with the specified characteristic are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVFragmentedMovie avFragmentedMovie, IsNSString mediaCharacteristic) => avFragmentedMovie -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avFragmentedMovie mediaCharacteristic =
  sendMessage avFragmentedMovie tracksWithMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | tracks
--
-- The tracks in a movie.
--
-- The value of this property is an array of tracks the movie contains; the tracks are of type AVFragmentedMovieTrack.
--
-- ObjC selector: @- tracks@
tracks :: IsAVFragmentedMovie avFragmentedMovie => avFragmentedMovie -> IO (Id NSArray)
tracks avFragmentedMovie =
  sendMessage avFragmentedMovie tracksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackWithTrackID:@
trackWithTrackIDSelector :: Selector '[CInt] (Id AVFragmentedMovieTrack)
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

