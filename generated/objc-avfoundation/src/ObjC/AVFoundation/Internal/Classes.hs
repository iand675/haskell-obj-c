{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AVFoundation.Internal.Classes (
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.CoreImage.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.MediaPlayer.Internal.Classes,
    module ObjC.QuartzCore.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.MediaPlayer.Internal.Classes
import ObjC.QuartzCore.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- AVAssetResourceLoadingContentInformationRequestInternal ----------

-- | AVAssetResourceLoadingContentInformationRequest
--
-- An AVAssetResourceLoadingContentInformationRequest represents a query for essential information about a resource referenced by an asset resource loading request.
--
-- When a resource loading delegate accepts responsibility for loading a resource by returning YES from its implementation of resourceLoader:shouldWaitForLoadingOfRequestedResource:, it must check whether the contentInformationRequest property of the AVAssetResourceLoadingRequest is not nil. Whenever the value is not nil, the request includes a query for the information that AVAssetResourceLoadingContentInformationRequest encapsulates. In response to such queries, the resource loading delegate should set the values of the content information request's properties appropriately before invoking the AVAssetResourceLoadingRequest method finishLoading.
--
-- When finishLoading is invoked, the values of the properties of its contentInformationRequest property will, in part, determine how the requested resource is processed. For example, if the requested resource's URL is the URL of an AVURLAsset and contentType is set by the resource loading delegate to a value that the underlying media system doesn't recognize as a supported media file type, operations on the AVURLAsset, such as playback, are likely to fail.
-- 
-- Phantom type for @AVAssetResourceLoadingContentInformationRequestInternal@.
data AVAssetResourceLoadingContentInformationRequestInternal

instance IsObjCObject (Id AVAssetResourceLoadingContentInformationRequestInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingContentInformationRequestInternal"

class IsObjCObject a => IsAVAssetResourceLoadingContentInformationRequestInternal a where
  toAVAssetResourceLoadingContentInformationRequestInternal :: a -> Id AVAssetResourceLoadingContentInformationRequestInternal

instance IsAVAssetResourceLoadingContentInformationRequestInternal (Id AVAssetResourceLoadingContentInformationRequestInternal) where
  toAVAssetResourceLoadingContentInformationRequestInternal = unsafeCastId

-- ---------- AVAssetResourceLoadingDataRequestInternal ----------

-- | AVAssetResourceLoadingDataRequest
--
-- An AVAssetResourceLoadingDataRequest is used to request data from a resource referenced by an AVAssetResourceLoadingRequest.
--
-- The AVAssetResourceLoaderDelegate uses the AVAssetResourceLoadingDataRequest class to do the actual data reading, and its methods will be invoked, as necessary, to acquire data for the AVAssetResourceLoadingRequest instance.
--
-- When a resource loading delegate accepts responsibility for loading a resource by returning YES from its implementation of resourceLoader:shouldWaitForLoadingOfRequestedResource:, it must check whether the dataRequest property of the AVAssetResourceLoadingRequest instance is not nil. If it is not nil, the resource loading delegate is informed of the range of bytes within the resource that are required by the underlying media system. In response, the data is provided by one or more invocations of respondWithData: as needed for provision of the requested data. The data can be provided in increments determined by the resource loading delegate according to convenience or efficiency.
--
-- When the AVAssetResourceLoadingRequest method finishLoading is invoked, the data request is considered fully satisfied. If the entire range of bytes requested has not yet been provided, the underlying media system assumes that the resource's length is limited to the provided content.
-- 
-- Phantom type for @AVAssetResourceLoadingDataRequestInternal@.
data AVAssetResourceLoadingDataRequestInternal

instance IsObjCObject (Id AVAssetResourceLoadingDataRequestInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingDataRequestInternal"

class IsObjCObject a => IsAVAssetResourceLoadingDataRequestInternal a where
  toAVAssetResourceLoadingDataRequestInternal :: a -> Id AVAssetResourceLoadingDataRequestInternal

instance IsAVAssetResourceLoadingDataRequestInternal (Id AVAssetResourceLoadingDataRequestInternal) where
  toAVAssetResourceLoadingDataRequestInternal = unsafeCastId

-- ---------- AVAssetResourceLoadingRequestInternal ----------

-- | AVAssetResourceLoadingRequest
--
-- AVAssetResourceLoadingRequest encapsulates information about a resource request issued by a resource loader.
--
-- When an AVURLAsset needs help loading a resource, it asks its AVAssetResourceLoader object to assist. The resource loader encapsulates the request information by creating an instance of this object, which it then hands to its delegate for processing. The delegate uses the information in this object to perform the request and report on the success or failure of the operation.
-- 
-- Phantom type for @AVAssetResourceLoadingRequestInternal@.
data AVAssetResourceLoadingRequestInternal

instance IsObjCObject (Id AVAssetResourceLoadingRequestInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingRequestInternal"

class IsObjCObject a => IsAVAssetResourceLoadingRequestInternal a where
  toAVAssetResourceLoadingRequestInternal :: a -> Id AVAssetResourceLoadingRequestInternal

instance IsAVAssetResourceLoadingRequestInternal (Id AVAssetResourceLoadingRequestInternal) where
  toAVAssetResourceLoadingRequestInternal = unsafeCastId

-- ---------- AVAudioMixInputParametersInternal ----------

-- | AVAudioMixInputParameters
--
-- Provides time-varying parameters to apply to an input of an audio mix. Audio volume is currently supported as a time-varying parameter.
--
-- Use an instance of AVAudioMixInputParameters to apply audio volume ramps for an input to an audio mix. AVAudioMixInputParameters are associated with audio tracks via the trackID property.
--
-- Notes on audio volume ramps:
--
-- Before the first time at which a volume is set, a volume of 1.0 used; after the last time for which a volume has been set, the last volume is used. Within the timeRange of a volume ramp, the volume is interpolated between the startVolume and endVolume of the ramp. For example, setting the volume to 1.0 at time 0 and also setting a volume ramp from a volume of 0.5 to 0.2 with a timeRange of [4.0, 5.0] results in an audio volume parameters that hold the volume constant at 1.0 from 0.0 sec to 4.0 sec, then cause it to jump to 0.5 and descend to 0.2 from 4.0 sec to 9.0 sec, holding constant at 0.2 thereafter.
-- 
-- Phantom type for @AVAudioMixInputParametersInternal@.
data AVAudioMixInputParametersInternal

instance IsObjCObject (Id AVAudioMixInputParametersInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioMixInputParametersInternal"

class IsObjCObject a => IsAVAudioMixInputParametersInternal a where
  toAVAudioMixInputParametersInternal :: a -> Id AVAudioMixInputParametersInternal

instance IsAVAudioMixInputParametersInternal (Id AVAudioMixInputParametersInternal) where
  toAVAudioMixInputParametersInternal = unsafeCastId

-- ---------- AVAudioMixInternal ----------

-- | AVAudioMix
--
-- Allows custom audio processing to be performed on audio tracks during playback or other operations.
-- 
-- Phantom type for @AVAudioMixInternal@.
data AVAudioMixInternal

instance IsObjCObject (Id AVAudioMixInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioMixInternal"

class IsObjCObject a => IsAVAudioMixInternal a where
  toAVAudioMixInternal :: a -> Id AVAudioMixInternal

instance IsAVAudioMixInternal (Id AVAudioMixInternal) where
  toAVAudioMixInternal = unsafeCastId

-- ---------- AVCompositionTrackInternal ----------

-- | AVCompositionTrack
--
-- AVCompositionTrack offers the low-level representation of tracks of AVCompositions, comprising a media type, a track identifier, and an array of AVCompositionTrackSegments, each comprising a URL, and track identifier, and a time mapping.
-- 
-- Phantom type for @AVCompositionTrackInternal@.
data AVCompositionTrackInternal

instance IsObjCObject (Id AVCompositionTrackInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCompositionTrackInternal"

class IsObjCObject a => IsAVCompositionTrackInternal a where
  toAVCompositionTrackInternal :: a -> Id AVCompositionTrackInternal

instance IsAVCompositionTrackInternal (Id AVCompositionTrackInternal) where
  toAVCompositionTrackInternal = unsafeCastId

-- ---------- AVCompositionTrackSegmentInternal ----------

-- | AVCompositionTrackSegment
--
-- A track segment maps a time from the source media track to the composition track.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCompositionTrackSegmentInternal@.
data AVCompositionTrackSegmentInternal

instance IsObjCObject (Id AVCompositionTrackSegmentInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCompositionTrackSegmentInternal"

class IsObjCObject a => IsAVCompositionTrackSegmentInternal a where
  toAVCompositionTrackSegmentInternal :: a -> Id AVCompositionTrackSegmentInternal

instance IsAVCompositionTrackSegmentInternal (Id AVCompositionTrackSegmentInternal) where
  toAVCompositionTrackSegmentInternal = unsafeCastId

-- ---------- AVMetadataItemFilterInternal ----------

-- | AVMetadataItemFilter
--
-- AVMetadataItemFilter is a tool used to filter AVMetadataItems.
--
-- Instances of AVMetadataItemFilter are used to filter AVMetadataItems.  They are opaque, unmodifiable objects, created via AVMetadataItemFilter class methods.
-- 
-- Phantom type for @AVMetadataItemFilterInternal@.
data AVMetadataItemFilterInternal

instance IsObjCObject (Id AVMetadataItemFilterInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataItemFilterInternal"

class IsObjCObject a => IsAVMetadataItemFilterInternal a where
  toAVMetadataItemFilterInternal :: a -> Id AVMetadataItemFilterInternal

instance IsAVMetadataItemFilterInternal (Id AVMetadataItemFilterInternal) where
  toAVMetadataItemFilterInternal = unsafeCastId

-- ---------- AVMetadataItemInternal ----------

-- | AVMetadataItem
--
-- AVMetadataItem represents an item of metadata associated with an audiovisual asset or with one of its tracks.
--
-- '	AVMetadataItems have keys that accord with the specification of the container format from which they're drawn. Full details of the metadata formats, metadata keys, and metadata keyspaces supported by AVFoundation are available among the defines in AVMetadataFormat.h.
--
-- Note that arrays of AVMetadataItems vended by AVAsset and other classes are "lazy", similar to array-based keys that support key-value observing, meaning that you can obtain objects from those arrays without incurring overhead for items you don't ultimately inspect.
--
-- AVMetadataItem conforms to NSMutableCopying, but for some "lazy" instances of AVMetadataItem, creating a mutable copy can cause properties to load synchronously.  This can cause the calling thread to block while synchronous I/O is performed.  To avoid the possiblity of blocking, which should be avoided on the main thread or when running on one of Swift's concurrency threads, ensure that the @value@ and @extraAttributes@ properties are loaded prior to making a mutable copy.  This can be done using the methods of AVAsynchronousKeyValueLoading, either to synchronously check whether loading has already occurred or to asynchronously load the property values.
--
-- You can filter arrays of AVMetadataItems by locale or by key and keySpace via the category AVMetadataItemArrayFiltering defined below.
-- 
-- Phantom type for @AVMetadataItemInternal@.
data AVMetadataItemInternal

instance IsObjCObject (Id AVMetadataItemInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataItemInternal"

class IsObjCObject a => IsAVMetadataItemInternal a where
  toAVMetadataItemInternal :: a -> Id AVMetadataItemInternal

instance IsAVMetadataItemInternal (Id AVMetadataItemInternal) where
  toAVMetadataItemInternal = unsafeCastId

-- ---------- AVMovieTrackInternal ----------

-- | AVMovieTrack
--
-- AVMovieTrack represents the tracks of audiovisual containers in a file that conforms to the QuickTime movie file format or to one of the related ISO base media file formats (such as MPEG-4).
-- 
-- Phantom type for @AVMovieTrackInternal@.
data AVMovieTrackInternal

instance IsObjCObject (Id AVMovieTrackInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMovieTrackInternal"

class IsObjCObject a => IsAVMovieTrackInternal a where
  toAVMovieTrackInternal :: a -> Id AVMovieTrackInternal

instance IsAVMovieTrackInternal (Id AVMovieTrackInternal) where
  toAVMovieTrackInternal = unsafeCastId

-- ---------- AVMutableCompositionTrackInternal ----------

-- | AVMutableCompositionTrack
--
-- AVMutableCompositionTrack provides a convenient interface for insertions, removals, and scaling of track segments without direct manipulation of their low-level representation.
-- 
-- Phantom type for @AVMutableCompositionTrackInternal@.
data AVMutableCompositionTrackInternal

instance IsObjCObject (Id AVMutableCompositionTrackInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableCompositionTrackInternal"

class IsObjCObject a => IsAVMutableCompositionTrackInternal a where
  toAVMutableCompositionTrackInternal :: a -> Id AVMutableCompositionTrackInternal

instance IsAVMutableCompositionTrackInternal (Id AVMutableCompositionTrackInternal) where
  toAVMutableCompositionTrackInternal = unsafeCastId

-- ---------- AVMutableMetadataItemInternal ----------

-- | AVMutableMetadataItem
--
-- AVMutableMetadataItem provides support for building collections of metadata to be written    				to asset files via AVAssetExportSession, AVAssetWriter or AVAssetWriterInput.
--
-- Can be initialized from an existing AVMetadataItem or with a one or more of the basic properties					of a metadata item: a key, a keySpace, a locale, and a value.
-- 
-- Phantom type for @AVMutableMetadataItemInternal@.
data AVMutableMetadataItemInternal

instance IsObjCObject (Id AVMutableMetadataItemInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableMetadataItemInternal"

class IsObjCObject a => IsAVMutableMetadataItemInternal a where
  toAVMutableMetadataItemInternal :: a -> Id AVMutableMetadataItemInternal

instance IsAVMutableMetadataItemInternal (Id AVMutableMetadataItemInternal) where
  toAVMutableMetadataItemInternal = unsafeCastId

-- ---------- AVMutableMovieTrackInternal ----------

-- | AVMutableMovieTrack
--
-- AVMutableMovieTrack provides the track-level editing interface of an AVMutableMovie. Media can be inserted into a movie track and other editing operations performed via an instance of this class.
-- 
-- Phantom type for @AVMutableMovieTrackInternal@.
data AVMutableMovieTrackInternal

instance IsObjCObject (Id AVMutableMovieTrackInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableMovieTrackInternal"

class IsObjCObject a => IsAVMutableMovieTrackInternal a where
  toAVMutableMovieTrackInternal :: a -> Id AVMutableMovieTrackInternal

instance IsAVMutableMovieTrackInternal (Id AVMutableMovieTrackInternal) where
  toAVMutableMovieTrackInternal = unsafeCastId

-- ---------- AVPlayerItemOutputInternal ----------

-- | AVPlayerItemOutput
--
-- AVPlayerItemOutput is an abstract class encapsulating the common API for all AVPlayerItemOutput subclasses.
--
-- Instances of AVPlayerItemOutput permit the acquisition of individual samples from an AVAsset during playback by an AVPlayer. To provide graceful degradation of service across multiple AVPlayerItemOutput instances for a single source, all AVPlayerItemOutput subclasses only offer the current sample and/or any readily available future samples. All samples earlier than the current sample are automatically discarded by the AVPlayerItemOutput.
--
-- You manage an association of an AVPlayerItemOutput instance with an AVPlayerItem as the source input using the AVPlayerItem methods:
--
-- • addOutput:		• removeOutput:
--
-- When an AVPlayerItemOutput is associated with an AVPlayerItem, samples are provided for a media type in accordance with the rules for mixing, composition, or exclusion that the AVPlayer honors among multiple enabled tracks of that media type for its own rendering purposes. For example, video media will be composed according to the instructions provided via AVPlayerItem.videoComposition, if present.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemOutputInternal@.
data AVPlayerItemOutputInternal

instance IsObjCObject (Id AVPlayerItemOutputInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemOutputInternal"

class IsObjCObject a => IsAVPlayerItemOutputInternal a where
  toAVPlayerItemOutputInternal :: a -> Id AVPlayerItemOutputInternal

instance IsAVPlayerItemOutputInternal (Id AVPlayerItemOutputInternal) where
  toAVPlayerItemOutputInternal = unsafeCastId

-- ---------- AVSampleCursorInternal ----------

-- | AVSampleCursor
--
-- An AVSampleCursor is always positioned at a specific media sample in a sequence of media samples as defined by a higher-level construct, such as an AVAssetTrack. It can be moved to a new position in that sequence either backwards or forwards, either in decode order or in presentation order. Movement can be requested according to a count of samples or according to a delta in time.
--
-- AVSampleCursors can be compared by position within the sample sequence.	  AVSampleCursors can be used synchronously to perform I/O in order to load media data of one or more media samples into memory.	  An AVSampleCursor can provide information about the media sample at its current position, such as its duration, its presentation and decode timestamps, whether it can be decoded independently of other media samples, its offset and length in its storage container, and whether the track signals that the sample is intended to be loaded with other contiguous media samples in a "chunk".	  Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVSampleCursorInternal@.
data AVSampleCursorInternal

instance IsObjCObject (Id AVSampleCursorInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleCursorInternal"

class IsObjCObject a => IsAVSampleCursorInternal a where
  toAVSampleCursorInternal :: a -> Id AVSampleCursorInternal

instance IsAVSampleCursorInternal (Id AVSampleCursorInternal) where
  toAVSampleCursorInternal = unsafeCastId

-- ---------- AVTextStyleRuleInternal ----------

-- | AVTextStyleRule
--
-- AVTextStyleRule represents a set of text styling attributes that can be applied to some or all of the text of legible media, such as subtitles and closed captions.
-- 
-- Phantom type for @AVTextStyleRuleInternal@.
data AVTextStyleRuleInternal

instance IsObjCObject (Id AVTextStyleRuleInternal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVTextStyleRuleInternal"

class IsObjCObject a => IsAVTextStyleRuleInternal a where
  toAVTextStyleRuleInternal :: a -> Id AVTextStyleRuleInternal

instance IsAVTextStyleRuleInternal (Id AVTextStyleRuleInternal) where
  toAVTextStyleRuleInternal = unsafeCastId

-- ---------- AVAsset ----------

-- | An AVAsset is an abstract class that defines AVFoundation's model for timed audiovisual media.
--
-- Each asset contains a collection of tracks that are intended to be presented or processed together, each of a uniform media type, including but not limited to audio, video, text, closed captions, and subtitles.
--
-- AVAssets are often instantiated via its concrete subclass AVURLAsset with NSURLs that refer to audiovisual media resources, such as streams (including HTTP live streams), QuickTime movie files, MP3 files, and files of other types.
--
-- They can also be instantiated using other concrete subclasses that extend the basic model for audiovisual media in useful ways, as AVComposition does for temporal editing.
--
-- Properties of assets as a whole are defined by AVAsset. Additionally, references to instances of AVAssetTracks representing tracks of the collection can be obtained, so that each of these can be examined independently.
--
-- Because of the nature of timed audiovisual media, upon successful initialization of an AVAsset some or all of the values for its keys may not be immediately available. The value of any key can be requested at any time, and AVAsset will always return its value synchronously, although it may have to block the calling thread in order to do so.
--
-- In order to avoid blocking, clients can register their interest in particular keys and to become notified when their values become available. For further details, see AVAsynchronousKeyValueLoading.h. For clients who want to examine a subset of the tracks, metadata, and other parts of the asset, asynchronous methods like -loadTracksWithMediaType:completionHandler: can be used to load this information without blocking. When using these asynchronous methods, it is not necessary to load the associated property beforehand. Swift clients can also use the load(:) method to load properties in a type safe manner.
--
-- On platforms other than macOS, it is particularly important to avoid blocking. To preserve responsiveness, a synchronous request that blocks for too long (eg, a property request on an asset on a slow HTTP server) may lead to media services being reset.
--
-- To play an instance of AVAsset, initialize an instance of AVPlayerItem with it, use the AVPlayerItem to set up its presentation state (such as whether only a limited timeRange of the asset should be played, etc.), and provide the AVPlayerItem to an AVPlayer according to whether the items is to be played by itself or together with a collection of other items. Full details available in AVPlayerItem.h and AVPlayer.h.
--
-- AVAssets can also be inserted into AVMutableCompositions in order to assemble audiovisual constructs from one or more source assets.
-- 
-- Phantom type for @AVAsset@.
data AVAsset

instance IsObjCObject (Id AVAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAsset"

class IsNSObject a => IsAVAsset a where
  toAVAsset :: a -> Id AVAsset

instance IsAVAsset (Id AVAsset) where
  toAVAsset = unsafeCastId

instance IsNSObject (Id AVAsset) where
  toNSObject = unsafeCastId

-- ---------- AVAssetCache ----------

-- | AVAssetCache is a class vended by an AVAsset used for the inspection of locally available media data.
--
-- AVAssetCaches are vended by AVURLAsset's assetCache property.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetCache@.
data AVAssetCache

instance IsObjCObject (Id AVAssetCache) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetCache"

class IsNSObject a => IsAVAssetCache a where
  toAVAssetCache :: a -> Id AVAssetCache

instance IsAVAssetCache (Id AVAssetCache) where
  toAVAssetCache = unsafeCastId

instance IsNSObject (Id AVAssetCache) where
  toNSObject = unsafeCastId

-- ---------- AVAssetDownloadConfiguration ----------

-- | Configuration parameters for the download task.
--
-- Download configuration consists of primary and auxiliary content configurations. Primary content configuration represents the primary set of renditions essential for offline playback. Auxiliary content configurations represent additional configurations to complement the primary. For example, the primary content configuration may represent stereo audio renditions and auxiliary configuration may represent complementing multichannel audio renditions.
--
-- It is important to configure your download configuration object appropriately before using it to create a download task. Download task makes a copy of the configuration settings you provide and use those settings to configure the task. Once configured, the task object ignores any changes you make to the NSURLSessionConfiguration object. If you need to modify your settings, you must update the download configuration object and use it to create a new download task object.
-- 
-- Phantom type for @AVAssetDownloadConfiguration@.
data AVAssetDownloadConfiguration

instance IsObjCObject (Id AVAssetDownloadConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetDownloadConfiguration"

class IsNSObject a => IsAVAssetDownloadConfiguration a where
  toAVAssetDownloadConfiguration :: a -> Id AVAssetDownloadConfiguration

instance IsAVAssetDownloadConfiguration (Id AVAssetDownloadConfiguration) where
  toAVAssetDownloadConfiguration = unsafeCastId

instance IsNSObject (Id AVAssetDownloadConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AVAssetDownloadContentConfiguration ----------

-- | Represents the configuration consisting of variant and the variant's media options.
-- 
-- Phantom type for @AVAssetDownloadContentConfiguration@.
data AVAssetDownloadContentConfiguration

instance IsObjCObject (Id AVAssetDownloadContentConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetDownloadContentConfiguration"

class IsNSObject a => IsAVAssetDownloadContentConfiguration a where
  toAVAssetDownloadContentConfiguration :: a -> Id AVAssetDownloadContentConfiguration

instance IsAVAssetDownloadContentConfiguration (Id AVAssetDownloadContentConfiguration) where
  toAVAssetDownloadContentConfiguration = unsafeCastId

instance IsNSObject (Id AVAssetDownloadContentConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AVAssetDownloadStorageManagementPolicy ----------

-- | A class to inform the system of a policy for automatic purging of downloaded AVAssets.
--
-- System will put in best-effort to evict all the assets based on expirationDate before evicting based on priority.
-- 
-- Phantom type for @AVAssetDownloadStorageManagementPolicy@.
data AVAssetDownloadStorageManagementPolicy

instance IsObjCObject (Id AVAssetDownloadStorageManagementPolicy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetDownloadStorageManagementPolicy"

class IsNSObject a => IsAVAssetDownloadStorageManagementPolicy a where
  toAVAssetDownloadStorageManagementPolicy :: a -> Id AVAssetDownloadStorageManagementPolicy

instance IsAVAssetDownloadStorageManagementPolicy (Id AVAssetDownloadStorageManagementPolicy) where
  toAVAssetDownloadStorageManagementPolicy = unsafeCastId

instance IsNSObject (Id AVAssetDownloadStorageManagementPolicy) where
  toNSObject = unsafeCastId

-- ---------- AVAssetDownloadStorageManager ----------

-- | An AVAssetDownloadStorageManager manages the policy for automatic purging of downloaded AVAssets. The policy is vended as AVAssetDownloadStorageManagementPolicy object.
--
-- When a storage management policy needs to be set on an asset, sharedDownloadStorageManager singleton needs to be fetched.  The new policy can then be set by using setStorageManagementPolicy and the location of the downloaded asset.
-- 
-- Phantom type for @AVAssetDownloadStorageManager@.
data AVAssetDownloadStorageManager

instance IsObjCObject (Id AVAssetDownloadStorageManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetDownloadStorageManager"

class IsNSObject a => IsAVAssetDownloadStorageManager a where
  toAVAssetDownloadStorageManager :: a -> Id AVAssetDownloadStorageManager

instance IsAVAssetDownloadStorageManager (Id AVAssetDownloadStorageManager) where
  toAVAssetDownloadStorageManager = unsafeCastId

instance IsNSObject (Id AVAssetDownloadStorageManager) where
  toNSObject = unsafeCastId

-- ---------- AVAssetExportSession ----------

-- | AVAssetExportSession
--
-- An AVAssetExportSession creates a new timed media resource from the contents of an				existing AVAsset in the form described by a specified export preset.
--
-- Prior to initializing an instance of AVAssetExportSession, you can invoke				+allExportPresets to obtain the complete list of presets available. Use				+exportPresetsCompatibleWithAsset: to obtain a list of presets that are compatible				with a specific AVAsset.
--
-- To configure an export, initialize an AVAssetExportSession with an AVAsset that contains				the source media, an AVAssetExportPreset, the output file type, (a UTI string from				those defined in AVMediaFormat.h) and the output URL.
--
-- After configuration is complete, invoke exportAsynchronouslyWithCompletionHandler:				to start the export process. This method returns immediately; the export is performed				asynchronously. Invoke the -progress method to check on the progress. Note that in				some cases, depending on the capabilities of the device, when multiple exports are				attempted at the same time some may be queued until others have been completed. When				this happens, the status of a queued export will indicate that it's "waiting".
--
-- Whether the export fails, completes, or is cancelled, the completion handler you				supply to -exportAsynchronouslyWithCompletionHandler: will be called. Upon				completion, the status property indicates whether the export has completed				successfully. If it has failed, the value of the error property supplies additional				information about the reason for the failure.
-- 
-- Phantom type for @AVAssetExportSession@.
data AVAssetExportSession

instance IsObjCObject (Id AVAssetExportSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetExportSession"

class IsNSObject a => IsAVAssetExportSession a where
  toAVAssetExportSession :: a -> Id AVAssetExportSession

instance IsAVAssetExportSession (Id AVAssetExportSession) where
  toAVAssetExportSession = unsafeCastId

instance IsNSObject (Id AVAssetExportSession) where
  toNSObject = unsafeCastId

-- ---------- AVAssetImageGenerator ----------

-- | Phantom type for @AVAssetImageGenerator@.
data AVAssetImageGenerator

instance IsObjCObject (Id AVAssetImageGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetImageGenerator"

class IsNSObject a => IsAVAssetImageGenerator a where
  toAVAssetImageGenerator :: a -> Id AVAssetImageGenerator

instance IsAVAssetImageGenerator (Id AVAssetImageGenerator) where
  toAVAssetImageGenerator = unsafeCastId

instance IsNSObject (Id AVAssetImageGenerator) where
  toNSObject = unsafeCastId

-- ---------- AVAssetPlaybackAssistant ----------

-- | AVAssetPlaybackAssistant provides playback information for an asset.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetPlaybackAssistant@.
data AVAssetPlaybackAssistant

instance IsObjCObject (Id AVAssetPlaybackAssistant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetPlaybackAssistant"

class IsNSObject a => IsAVAssetPlaybackAssistant a where
  toAVAssetPlaybackAssistant :: a -> Id AVAssetPlaybackAssistant

instance IsAVAssetPlaybackAssistant (Id AVAssetPlaybackAssistant) where
  toAVAssetPlaybackAssistant = unsafeCastId

instance IsNSObject (Id AVAssetPlaybackAssistant) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReader ----------

-- | AVAssetReader
--
-- AVAssetReader provides services for obtaining media data from an asset.
--
-- Instances of AVAssetReader read media data from an instance of AVAsset, whether the asset is file-based or represents an assembly of media data from multiple sources, as is the case with AVComposition.
--
-- Clients of AVAssetReader can read data from specific tracks of an asset and in specific formats by adding concrete instances of AVAssetReaderOutput to an AVAssetReader instance.
--
-- AVAssetReaderTrackOutput, a concrete subclass of AVAssetReaderOutput, can either read the track's media samples in the format in which they are stored by the asset or convert the media samples to a different format.
--
-- AVAssetReaderAudioMixOutput mixes multiple audio tracks of the asset after reading them, while AVAssetReaderVideoCompositionOutput composites multiple video tracks after reading them.
-- 
-- Phantom type for @AVAssetReader@.
data AVAssetReader

instance IsObjCObject (Id AVAssetReader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReader"

class IsNSObject a => IsAVAssetReader a where
  toAVAssetReader :: a -> Id AVAssetReader

instance IsAVAssetReader (Id AVAssetReader) where
  toAVAssetReader = unsafeCastId

instance IsNSObject (Id AVAssetReader) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderOutput ----------

-- | AVAssetReaderOutput
--
-- AVAssetReaderOutput is an abstract class that defines an interface for reading a single collection of samples of a common media type from an AVAssetReader.
--
-- Clients can read the media data of an asset by adding one or more concrete instances of AVAssetReaderOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method.
--
-- IMPORTANT PERFORMANCE NOTE: Make sure to set the alwaysCopiesSampleData property to NO if you do not need to modify the sample data in-place, to avoid unnecessary and inefficient copying.
-- 
-- Phantom type for @AVAssetReaderOutput@.
data AVAssetReaderOutput

instance IsObjCObject (Id AVAssetReaderOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderOutput"

class IsNSObject a => IsAVAssetReaderOutput a where
  toAVAssetReaderOutput :: a -> Id AVAssetReaderOutput

instance IsAVAssetReaderOutput (Id AVAssetReaderOutput) where
  toAVAssetReaderOutput = unsafeCastId

instance IsNSObject (Id AVAssetReaderOutput) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderOutputCaptionAdaptor ----------

-- | Phantom type for @AVAssetReaderOutputCaptionAdaptor@.
data AVAssetReaderOutputCaptionAdaptor

instance IsObjCObject (Id AVAssetReaderOutputCaptionAdaptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderOutputCaptionAdaptor"

class IsNSObject a => IsAVAssetReaderOutputCaptionAdaptor a where
  toAVAssetReaderOutputCaptionAdaptor :: a -> Id AVAssetReaderOutputCaptionAdaptor

instance IsAVAssetReaderOutputCaptionAdaptor (Id AVAssetReaderOutputCaptionAdaptor) where
  toAVAssetReaderOutputCaptionAdaptor = unsafeCastId

instance IsNSObject (Id AVAssetReaderOutputCaptionAdaptor) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderOutputMetadataAdaptor ----------

-- | Phantom type for @AVAssetReaderOutputMetadataAdaptor@.
data AVAssetReaderOutputMetadataAdaptor

instance IsObjCObject (Id AVAssetReaderOutputMetadataAdaptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderOutputMetadataAdaptor"

class IsNSObject a => IsAVAssetReaderOutputMetadataAdaptor a where
  toAVAssetReaderOutputMetadataAdaptor :: a -> Id AVAssetReaderOutputMetadataAdaptor

instance IsAVAssetReaderOutputMetadataAdaptor (Id AVAssetReaderOutputMetadataAdaptor) where
  toAVAssetReaderOutputMetadataAdaptor = unsafeCastId

instance IsNSObject (Id AVAssetReaderOutputMetadataAdaptor) where
  toNSObject = unsafeCastId

-- ---------- AVAssetResourceLoader ----------

-- | Phantom type for @AVAssetResourceLoader@.
data AVAssetResourceLoader

instance IsObjCObject (Id AVAssetResourceLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoader"

class IsNSObject a => IsAVAssetResourceLoader a where
  toAVAssetResourceLoader :: a -> Id AVAssetResourceLoader

instance IsAVAssetResourceLoader (Id AVAssetResourceLoader) where
  toAVAssetResourceLoader = unsafeCastId

instance IsNSObject (Id AVAssetResourceLoader) where
  toNSObject = unsafeCastId

-- ---------- AVAssetResourceLoadingContentInformationRequest ----------

-- | Phantom type for @AVAssetResourceLoadingContentInformationRequest@.
data AVAssetResourceLoadingContentInformationRequest

instance IsObjCObject (Id AVAssetResourceLoadingContentInformationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingContentInformationRequest"

class IsNSObject a => IsAVAssetResourceLoadingContentInformationRequest a where
  toAVAssetResourceLoadingContentInformationRequest :: a -> Id AVAssetResourceLoadingContentInformationRequest

instance IsAVAssetResourceLoadingContentInformationRequest (Id AVAssetResourceLoadingContentInformationRequest) where
  toAVAssetResourceLoadingContentInformationRequest = unsafeCastId

instance IsNSObject (Id AVAssetResourceLoadingContentInformationRequest) where
  toNSObject = unsafeCastId

-- ---------- AVAssetResourceLoadingDataRequest ----------

-- | Phantom type for @AVAssetResourceLoadingDataRequest@.
data AVAssetResourceLoadingDataRequest

instance IsObjCObject (Id AVAssetResourceLoadingDataRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingDataRequest"

class IsNSObject a => IsAVAssetResourceLoadingDataRequest a where
  toAVAssetResourceLoadingDataRequest :: a -> Id AVAssetResourceLoadingDataRequest

instance IsAVAssetResourceLoadingDataRequest (Id AVAssetResourceLoadingDataRequest) where
  toAVAssetResourceLoadingDataRequest = unsafeCastId

instance IsNSObject (Id AVAssetResourceLoadingDataRequest) where
  toNSObject = unsafeCastId

-- ---------- AVAssetResourceLoadingRequest ----------

-- | Phantom type for @AVAssetResourceLoadingRequest@.
data AVAssetResourceLoadingRequest

instance IsObjCObject (Id AVAssetResourceLoadingRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingRequest"

class IsNSObject a => IsAVAssetResourceLoadingRequest a where
  toAVAssetResourceLoadingRequest :: a -> Id AVAssetResourceLoadingRequest

instance IsAVAssetResourceLoadingRequest (Id AVAssetResourceLoadingRequest) where
  toAVAssetResourceLoadingRequest = unsafeCastId

instance IsNSObject (Id AVAssetResourceLoadingRequest) where
  toNSObject = unsafeCastId

-- ---------- AVAssetResourceLoadingRequestor ----------

-- | AVAssetResourceLoadingRequestor
--
-- AVAssetResourceLoadingRequestor represents the originator of loading request
--
-- Information about the originator of a loading request, in order to decide whether or how to fulfill the request.
-- 
-- Phantom type for @AVAssetResourceLoadingRequestor@.
data AVAssetResourceLoadingRequestor

instance IsObjCObject (Id AVAssetResourceLoadingRequestor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceLoadingRequestor"

class IsNSObject a => IsAVAssetResourceLoadingRequestor a where
  toAVAssetResourceLoadingRequestor :: a -> Id AVAssetResourceLoadingRequestor

instance IsAVAssetResourceLoadingRequestor (Id AVAssetResourceLoadingRequestor) where
  toAVAssetResourceLoadingRequestor = unsafeCastId

instance IsNSObject (Id AVAssetResourceLoadingRequestor) where
  toNSObject = unsafeCastId

-- ---------- AVAssetSegmentReport ----------

-- | AVAssetSegmentReport
--
-- This class provides information on a segment data.
--
-- Clients may get an instance of AVAssetSegmentReport through the -assetWriter:didOutputSegmentData:segmentType:segmentReport: delegate method, which is defined in AVAssetWriter.h. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetSegmentReport@.
data AVAssetSegmentReport

instance IsObjCObject (Id AVAssetSegmentReport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetSegmentReport"

class IsNSObject a => IsAVAssetSegmentReport a where
  toAVAssetSegmentReport :: a -> Id AVAssetSegmentReport

instance IsAVAssetSegmentReport (Id AVAssetSegmentReport) where
  toAVAssetSegmentReport = unsafeCastId

instance IsNSObject (Id AVAssetSegmentReport) where
  toNSObject = unsafeCastId

-- ---------- AVAssetSegmentReportSampleInformation ----------

-- | AVAssetSegmentReportSampleInformation
--
-- This class is vended by AVAssetSegmentTrackReport. It will provide information on a sample in a track.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetSegmentReportSampleInformation@.
data AVAssetSegmentReportSampleInformation

instance IsObjCObject (Id AVAssetSegmentReportSampleInformation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetSegmentReportSampleInformation"

class IsNSObject a => IsAVAssetSegmentReportSampleInformation a where
  toAVAssetSegmentReportSampleInformation :: a -> Id AVAssetSegmentReportSampleInformation

instance IsAVAssetSegmentReportSampleInformation (Id AVAssetSegmentReportSampleInformation) where
  toAVAssetSegmentReportSampleInformation = unsafeCastId

instance IsNSObject (Id AVAssetSegmentReportSampleInformation) where
  toNSObject = unsafeCastId

-- ---------- AVAssetSegmentTrackReport ----------

-- | AVAssetSegmentTrackReport
--
-- This class is vended by AVAssetSegmentReport. It will provide information on a track in a segment data.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetSegmentTrackReport@.
data AVAssetSegmentTrackReport

instance IsObjCObject (Id AVAssetSegmentTrackReport) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetSegmentTrackReport"

class IsNSObject a => IsAVAssetSegmentTrackReport a where
  toAVAssetSegmentTrackReport :: a -> Id AVAssetSegmentTrackReport

instance IsAVAssetSegmentTrackReport (Id AVAssetSegmentTrackReport) where
  toAVAssetSegmentTrackReport = unsafeCastId

instance IsNSObject (Id AVAssetSegmentTrackReport) where
  toNSObject = unsafeCastId

-- ---------- AVAssetTrack ----------

-- | An AVAssetTrack object provides provides the track-level inspection interface for all assets.
--
-- AVAssetTrack adopts the AVAsynchronousKeyValueLoading protocol. Methods in the protocol should be used to access a track's properties without blocking the current thread. To cancel load requests for all keys of AVAssetTrack one must message the parent AVAsset object (for example, [track.asset cancelLoading]).
--
-- For clients who want to examine a subset of the metadata or other parts of the track, asynchronous methods like -loadMetadataForFormat:completionHandler: can be used to load this information without blocking. When using these asynchronous methods, it is not necessary to load the associated property beforehand. Swift clients can also use the load(:) method to load properties in a type safe manner.
-- 
-- Phantom type for @AVAssetTrack@.
data AVAssetTrack

instance IsObjCObject (Id AVAssetTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetTrack"

class IsNSObject a => IsAVAssetTrack a where
  toAVAssetTrack :: a -> Id AVAssetTrack

instance IsAVAssetTrack (Id AVAssetTrack) where
  toAVAssetTrack = unsafeCastId

instance IsNSObject (Id AVAssetTrack) where
  toNSObject = unsafeCastId

-- ---------- AVAssetTrackGroup ----------

-- | AVAssetTrackGroup
--
-- A class whose instances describe a group of tracks in an asset.
--
-- Instances of AVAssetTrackGroup describe a single group of related tracks in an asset. For example, a track group can	describe a set of alternate tracks, which are tracks containing variations of the same content, such as content	translated into different languages, out of which only one track should be played at a time.
--
-- Clients can inspect the track groups contained in an AVAsset by loading and obtaining the value of its trackGroups property.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetTrackGroup@.
data AVAssetTrackGroup

instance IsObjCObject (Id AVAssetTrackGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetTrackGroup"

class IsNSObject a => IsAVAssetTrackGroup a where
  toAVAssetTrackGroup :: a -> Id AVAssetTrackGroup

instance IsAVAssetTrackGroup (Id AVAssetTrackGroup) where
  toAVAssetTrackGroup = unsafeCastId

instance IsNSObject (Id AVAssetTrackGroup) where
  toNSObject = unsafeCastId

-- ---------- AVAssetTrackSegment ----------

-- | Phantom type for @AVAssetTrackSegment@.
data AVAssetTrackSegment

instance IsObjCObject (Id AVAssetTrackSegment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetTrackSegment"

class IsNSObject a => IsAVAssetTrackSegment a where
  toAVAssetTrackSegment :: a -> Id AVAssetTrackSegment

instance IsAVAssetTrackSegment (Id AVAssetTrackSegment) where
  toAVAssetTrackSegment = unsafeCastId

instance IsNSObject (Id AVAssetTrackSegment) where
  toNSObject = unsafeCastId

-- ---------- AVAssetVariant ----------

-- | An AVAssetVariant represents a bit rate variant. Each asset contains a collection of variants that represent a combination of audio, video, text, closed captions, and subtitles for a particular bit rate. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetVariant@.
data AVAssetVariant

instance IsObjCObject (Id AVAssetVariant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetVariant"

class IsNSObject a => IsAVAssetVariant a where
  toAVAssetVariant :: a -> Id AVAssetVariant

instance IsAVAssetVariant (Id AVAssetVariant) where
  toAVAssetVariant = unsafeCastId

instance IsNSObject (Id AVAssetVariant) where
  toNSObject = unsafeCastId

-- ---------- AVAssetVariantAudioAttributes ----------

-- | Audio attributes for an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetVariantAudioAttributes@.
data AVAssetVariantAudioAttributes

instance IsObjCObject (Id AVAssetVariantAudioAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetVariantAudioAttributes"

class IsNSObject a => IsAVAssetVariantAudioAttributes a where
  toAVAssetVariantAudioAttributes :: a -> Id AVAssetVariantAudioAttributes

instance IsAVAssetVariantAudioAttributes (Id AVAssetVariantAudioAttributes) where
  toAVAssetVariantAudioAttributes = unsafeCastId

instance IsNSObject (Id AVAssetVariantAudioAttributes) where
  toNSObject = unsafeCastId

-- ---------- AVAssetVariantAudioRenditionSpecificAttributes ----------

-- | Audio rendition attributes for an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetVariantAudioRenditionSpecificAttributes@.
data AVAssetVariantAudioRenditionSpecificAttributes

instance IsObjCObject (Id AVAssetVariantAudioRenditionSpecificAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetVariantAudioRenditionSpecificAttributes"

class IsNSObject a => IsAVAssetVariantAudioRenditionSpecificAttributes a where
  toAVAssetVariantAudioRenditionSpecificAttributes :: a -> Id AVAssetVariantAudioRenditionSpecificAttributes

instance IsAVAssetVariantAudioRenditionSpecificAttributes (Id AVAssetVariantAudioRenditionSpecificAttributes) where
  toAVAssetVariantAudioRenditionSpecificAttributes = unsafeCastId

instance IsNSObject (Id AVAssetVariantAudioRenditionSpecificAttributes) where
  toNSObject = unsafeCastId

-- ---------- AVAssetVariantQualifier ----------

-- | The qualifier of an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetVariantQualifier@.
data AVAssetVariantQualifier

instance IsObjCObject (Id AVAssetVariantQualifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetVariantQualifier"

class IsNSObject a => IsAVAssetVariantQualifier a where
  toAVAssetVariantQualifier :: a -> Id AVAssetVariantQualifier

instance IsAVAssetVariantQualifier (Id AVAssetVariantQualifier) where
  toAVAssetVariantQualifier = unsafeCastId

instance IsNSObject (Id AVAssetVariantQualifier) where
  toNSObject = unsafeCastId

-- ---------- AVAssetVariantVideoAttributes ----------

-- | Video attributes for an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetVariantVideoAttributes@.
data AVAssetVariantVideoAttributes

instance IsObjCObject (Id AVAssetVariantVideoAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetVariantVideoAttributes"

class IsNSObject a => IsAVAssetVariantVideoAttributes a where
  toAVAssetVariantVideoAttributes :: a -> Id AVAssetVariantVideoAttributes

instance IsAVAssetVariantVideoAttributes (Id AVAssetVariantVideoAttributes) where
  toAVAssetVariantVideoAttributes = unsafeCastId

instance IsNSObject (Id AVAssetVariantVideoAttributes) where
  toNSObject = unsafeCastId

-- ---------- AVAssetVariantVideoLayoutAttributes ----------

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetVariantVideoLayoutAttributes@.
data AVAssetVariantVideoLayoutAttributes

instance IsObjCObject (Id AVAssetVariantVideoLayoutAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetVariantVideoLayoutAttributes"

class IsNSObject a => IsAVAssetVariantVideoLayoutAttributes a where
  toAVAssetVariantVideoLayoutAttributes :: a -> Id AVAssetVariantVideoLayoutAttributes

instance IsAVAssetVariantVideoLayoutAttributes (Id AVAssetVariantVideoLayoutAttributes) where
  toAVAssetVariantVideoLayoutAttributes = unsafeCastId

instance IsNSObject (Id AVAssetVariantVideoLayoutAttributes) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriter ----------

-- | AVAssetWriter
--
-- AVAssetWriter provides services for writing media data to a new file,
--
-- Instances of AVAssetWriter can write media to new files in formats such as the QuickTime movie file format or the MPEG-4 file format. AVAssetWriter has support for automatic interleaving of media data for multiple concurrent tracks. Source media data can be obtained from instances of AVAssetReader for one or more assets or from other sources outside of AVFoundation.
--
-- Instances of AVAssetWriter can re-encode media samples as they are written. Instances of AVAssetWriter can also optionally write metadata collections to the output file.
--
-- A single instance of AVAssetWriter can be used once to write to a single file. Clients that wish to write to files multiple times must use a new instance of AVAssetWriter each time.
-- 
-- Phantom type for @AVAssetWriter@.
data AVAssetWriter

instance IsObjCObject (Id AVAssetWriter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriter"

class IsNSObject a => IsAVAssetWriter a where
  toAVAssetWriter :: a -> Id AVAssetWriter

instance IsAVAssetWriter (Id AVAssetWriter) where
  toAVAssetWriter = unsafeCastId

instance IsNSObject (Id AVAssetWriter) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInput ----------

-- | AVAssetWriterInput defines an interface for appending either new media samples or references to existing media samples packaged as CMSampleBuffer objects to a single track of the output file of an AVAssetWriter.
--
-- Clients that need to write multiple concurrent tracks of media data should use one AVAssetWriterInput instance per track. In order to write multiple concurrent tracks with ideal interleaving of media data, clients should observe the value returned by the readyForMoreMediaData property of each AVAssetWriterInput instance.
--
-- AVAssetWriterInput also supports writing per-track metadata collections to the output file.
--
-- As of macOS 10.10 and iOS 8.0 AVAssetWriterInput can also be used to create tracks that are not self-contained. Such tracks reference sample data that is located in another file. This is currently supported only for instances of AVAssetWriterInput attached to an instance of AVAssetWriter that writes files of type AVFileTypeQuickTimeMovie.
-- 
-- Phantom type for @AVAssetWriterInput@.
data AVAssetWriterInput

instance IsObjCObject (Id AVAssetWriterInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInput"

class IsNSObject a => IsAVAssetWriterInput a where
  toAVAssetWriterInput :: a -> Id AVAssetWriterInput

instance IsAVAssetWriterInput (Id AVAssetWriterInput) where
  toAVAssetWriterInput = unsafeCastId

instance IsNSObject (Id AVAssetWriterInput) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInputCaptionAdaptor ----------

-- | Phantom type for @AVAssetWriterInputCaptionAdaptor@.
data AVAssetWriterInputCaptionAdaptor

instance IsObjCObject (Id AVAssetWriterInputCaptionAdaptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInputCaptionAdaptor"

class IsNSObject a => IsAVAssetWriterInputCaptionAdaptor a where
  toAVAssetWriterInputCaptionAdaptor :: a -> Id AVAssetWriterInputCaptionAdaptor

instance IsAVAssetWriterInputCaptionAdaptor (Id AVAssetWriterInputCaptionAdaptor) where
  toAVAssetWriterInputCaptionAdaptor = unsafeCastId

instance IsNSObject (Id AVAssetWriterInputCaptionAdaptor) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInputMetadataAdaptor ----------

-- | Phantom type for @AVAssetWriterInputMetadataAdaptor@.
data AVAssetWriterInputMetadataAdaptor

instance IsObjCObject (Id AVAssetWriterInputMetadataAdaptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInputMetadataAdaptor"

class IsNSObject a => IsAVAssetWriterInputMetadataAdaptor a where
  toAVAssetWriterInputMetadataAdaptor :: a -> Id AVAssetWriterInputMetadataAdaptor

instance IsAVAssetWriterInputMetadataAdaptor (Id AVAssetWriterInputMetadataAdaptor) where
  toAVAssetWriterInputMetadataAdaptor = unsafeCastId

instance IsNSObject (Id AVAssetWriterInputMetadataAdaptor) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInputPassDescription ----------

-- | Defines an interface for querying information about the requirements of the current pass, such as the time ranges of media data to append.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAssetWriterInputPassDescription@.
data AVAssetWriterInputPassDescription

instance IsObjCObject (Id AVAssetWriterInputPassDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInputPassDescription"

class IsNSObject a => IsAVAssetWriterInputPassDescription a where
  toAVAssetWriterInputPassDescription :: a -> Id AVAssetWriterInputPassDescription

instance IsAVAssetWriterInputPassDescription (Id AVAssetWriterInputPassDescription) where
  toAVAssetWriterInputPassDescription = unsafeCastId

instance IsNSObject (Id AVAssetWriterInputPassDescription) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInputPixelBufferAdaptor ----------

-- | Phantom type for @AVAssetWriterInputPixelBufferAdaptor@.
data AVAssetWriterInputPixelBufferAdaptor

instance IsObjCObject (Id AVAssetWriterInputPixelBufferAdaptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInputPixelBufferAdaptor"

class IsNSObject a => IsAVAssetWriterInputPixelBufferAdaptor a where
  toAVAssetWriterInputPixelBufferAdaptor :: a -> Id AVAssetWriterInputPixelBufferAdaptor

instance IsAVAssetWriterInputPixelBufferAdaptor (Id AVAssetWriterInputPixelBufferAdaptor) where
  toAVAssetWriterInputPixelBufferAdaptor = unsafeCastId

instance IsNSObject (Id AVAssetWriterInputPixelBufferAdaptor) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInputTaggedPixelBufferGroupAdaptor ----------

-- | Phantom type for @AVAssetWriterInputTaggedPixelBufferGroupAdaptor@.
data AVAssetWriterInputTaggedPixelBufferGroupAdaptor

instance IsObjCObject (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInputTaggedPixelBufferGroupAdaptor"

class IsNSObject a => IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor a where
  toAVAssetWriterInputTaggedPixelBufferGroupAdaptor :: a -> Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor

instance IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor) where
  toAVAssetWriterInputTaggedPixelBufferGroupAdaptor = unsafeCastId

instance IsNSObject (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor) where
  toNSObject = unsafeCastId

-- ---------- AVAsynchronousCIImageFilteringRequest ----------

-- | Phantom type for @AVAsynchronousCIImageFilteringRequest@.
data AVAsynchronousCIImageFilteringRequest

instance IsObjCObject (Id AVAsynchronousCIImageFilteringRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAsynchronousCIImageFilteringRequest"

class IsNSObject a => IsAVAsynchronousCIImageFilteringRequest a where
  toAVAsynchronousCIImageFilteringRequest :: a -> Id AVAsynchronousCIImageFilteringRequest

instance IsAVAsynchronousCIImageFilteringRequest (Id AVAsynchronousCIImageFilteringRequest) where
  toAVAsynchronousCIImageFilteringRequest = unsafeCastId

instance IsNSObject (Id AVAsynchronousCIImageFilteringRequest) where
  toNSObject = unsafeCastId

-- ---------- AVAsynchronousVideoCompositionRequest ----------

-- | An AVAsynchronousVideoCompositionRequest instance contains the information necessary for a video compositor to render an output pixel buffer. The video compositor must implement the AVVideoCompositing protocol.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAsynchronousVideoCompositionRequest@.
data AVAsynchronousVideoCompositionRequest

instance IsObjCObject (Id AVAsynchronousVideoCompositionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAsynchronousVideoCompositionRequest"

class IsNSObject a => IsAVAsynchronousVideoCompositionRequest a where
  toAVAsynchronousVideoCompositionRequest :: a -> Id AVAsynchronousVideoCompositionRequest

instance IsAVAsynchronousVideoCompositionRequest (Id AVAsynchronousVideoCompositionRequest) where
  toAVAsynchronousVideoCompositionRequest = unsafeCastId

instance IsNSObject (Id AVAsynchronousVideoCompositionRequest) where
  toNSObject = unsafeCastId

-- ---------- AVAudioMix ----------

-- | Phantom type for @AVAudioMix@.
data AVAudioMix

instance IsObjCObject (Id AVAudioMix) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioMix"

class IsNSObject a => IsAVAudioMix a where
  toAVAudioMix :: a -> Id AVAudioMix

instance IsAVAudioMix (Id AVAudioMix) where
  toAVAudioMix = unsafeCastId

instance IsNSObject (Id AVAudioMix) where
  toNSObject = unsafeCastId

-- ---------- AVAudioMixInputParameters ----------

-- | Phantom type for @AVAudioMixInputParameters@.
data AVAudioMixInputParameters

instance IsObjCObject (Id AVAudioMixInputParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAudioMixInputParameters"

class IsNSObject a => IsAVAudioMixInputParameters a where
  toAVAudioMixInputParameters :: a -> Id AVAudioMixInputParameters

instance IsAVAudioMixInputParameters (Id AVAudioMixInputParameters) where
  toAVAudioMixInputParameters = unsafeCastId

instance IsNSObject (Id AVAudioMixInputParameters) where
  toNSObject = unsafeCastId

-- ---------- AVCameraCalibrationData ----------

-- | AVCameraCalibrationData
--
-- AVCameraCalibrationData is a model object describing a camera's calibration information.
--
-- When rendering effects to images produced by cameras, or performing computer vision tasks such as correcting images for geometric distortions, it is necessary to characterize the camera's calibration information, such as its pixel focal length, principal point, lens distortion characteristics, etc. AVCameraCalibrationData provides this information.
-- 
-- Phantom type for @AVCameraCalibrationData@.
data AVCameraCalibrationData

instance IsObjCObject (Id AVCameraCalibrationData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCameraCalibrationData"

class IsNSObject a => IsAVCameraCalibrationData a where
  toAVCameraCalibrationData :: a -> Id AVCameraCalibrationData

instance IsAVCameraCalibrationData (Id AVCameraCalibrationData) where
  toAVCameraCalibrationData = unsafeCastId

instance IsNSObject (Id AVCameraCalibrationData) where
  toNSObject = unsafeCastId

-- ---------- AVCaption ----------

-- | AVCaption
--
-- An instance of AVCaption represents a unit of text that is active at a particular time range.
--
-- A caption contains one meaningful sentence, paragraph, or otherwise known as a caption cue. Within the active time range, it may perform animation (e.g. Karaoke), rolling-up, changes the visibility, or any other dynamic styling.
-- 
-- Phantom type for @AVCaption@.
data AVCaption

instance IsObjCObject (Id AVCaption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaption"

class IsNSObject a => IsAVCaption a where
  toAVCaption :: a -> Id AVCaption

instance IsAVCaption (Id AVCaption) where
  toAVCaption = unsafeCastId

instance IsNSObject (Id AVCaption) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionConversionAdjustment ----------

-- | AVCaptionConversionAdjustment
--
-- Describes an adjustment that can be performed in order to correct a problem encountered during the validation of a caption conversion.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCaptionConversionAdjustment@.
data AVCaptionConversionAdjustment

instance IsObjCObject (Id AVCaptionConversionAdjustment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionConversionAdjustment"

class IsNSObject a => IsAVCaptionConversionAdjustment a where
  toAVCaptionConversionAdjustment :: a -> Id AVCaptionConversionAdjustment

instance IsAVCaptionConversionAdjustment (Id AVCaptionConversionAdjustment) where
  toAVCaptionConversionAdjustment = unsafeCastId

instance IsNSObject (Id AVCaptionConversionAdjustment) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionConversionValidator ----------

-- | AVCaptionConversionValidator
--
-- Performs a validation of captions for a conversion operation and warns about problems that are encountered.
-- 
-- Phantom type for @AVCaptionConversionValidator@.
data AVCaptionConversionValidator

instance IsObjCObject (Id AVCaptionConversionValidator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionConversionValidator"

class IsNSObject a => IsAVCaptionConversionValidator a where
  toAVCaptionConversionValidator :: a -> Id AVCaptionConversionValidator

instance IsAVCaptionConversionValidator (Id AVCaptionConversionValidator) where
  toAVCaptionConversionValidator = unsafeCastId

instance IsNSObject (Id AVCaptionConversionValidator) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionConversionWarning ----------

-- | AVCaptionConversionWarning
--
-- Reports a specific problem encountered during the validation of a caption conversion.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCaptionConversionWarning@.
data AVCaptionConversionWarning

instance IsObjCObject (Id AVCaptionConversionWarning) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionConversionWarning"

class IsNSObject a => IsAVCaptionConversionWarning a where
  toAVCaptionConversionWarning :: a -> Id AVCaptionConversionWarning

instance IsAVCaptionConversionWarning (Id AVCaptionConversionWarning) where
  toAVCaptionConversionWarning = unsafeCastId

instance IsNSObject (Id AVCaptionConversionWarning) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionFormatConformer ----------

-- | AVCaptionFormatConformer
--
-- Performs a conversion of canonical caption to conform to a specific format.
-- 
-- Phantom type for @AVCaptionFormatConformer@.
data AVCaptionFormatConformer

instance IsObjCObject (Id AVCaptionFormatConformer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionFormatConformer"

class IsNSObject a => IsAVCaptionFormatConformer a where
  toAVCaptionFormatConformer :: a -> Id AVCaptionFormatConformer

instance IsAVCaptionFormatConformer (Id AVCaptionFormatConformer) where
  toAVCaptionFormatConformer = unsafeCastId

instance IsNSObject (Id AVCaptionFormatConformer) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionGroup ----------

-- | AVCaptionGroup
--
-- An instance of AVCaptionGroup represents zero or more captions that intersect in time.
--
-- The time range of each caption may overlap as there can be more than one active caption at a time. A sequence of AVCaptionGroup objects represents such overlapping caption timeline.
--
-- An instance of AVCaptionGroup has a time range and a list of active captions for the time range. Two successive AVCaptionGroup objects have contiguous and non-overlapping time ranges. A new AVCaptionGroup time range commences whenever any of caption becomes active or inactive. When a caption spans over multiple AVCaptionGroup time ranges, these  AVCaptionGroup objects refer to an equal AVCaption object.
--
-- An empty AVCaptionGroup represents the time range without any active captions.
--
-- The list of captions in the group is ordered according to the document order. For example, suppose a TTML document has two temporally overhapping captions:
--
-- Hello      World
--
-- AVCaptionGroup for time range 1s to 2s has the list of captions: Hello and World in this order despite the fact that "World" is shown earlier than "Hello".
--
-- A client may use AVCaptionGroup to get the list of active captions for the time range. For example, presentation processing may find the AVCaptionGroup object for the current time, get the list of captions, and place them into the destination display region.
-- 
-- Phantom type for @AVCaptionGroup@.
data AVCaptionGroup

instance IsObjCObject (Id AVCaptionGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionGroup"

class IsNSObject a => IsAVCaptionGroup a where
  toAVCaptionGroup :: a -> Id AVCaptionGroup

instance IsAVCaptionGroup (Id AVCaptionGroup) where
  toAVCaptionGroup = unsafeCastId

instance IsNSObject (Id AVCaptionGroup) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionGrouper ----------

-- | Phantom type for @AVCaptionGrouper@.
data AVCaptionGrouper

instance IsObjCObject (Id AVCaptionGrouper) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionGrouper"

class IsNSObject a => IsAVCaptionGrouper a where
  toAVCaptionGrouper :: a -> Id AVCaptionGrouper

instance IsAVCaptionGrouper (Id AVCaptionGrouper) where
  toAVCaptionGrouper = unsafeCastId

instance IsNSObject (Id AVCaptionGrouper) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionRegion ----------

-- | AVCaptionRegion
--
-- An instance of AVCaptionRegion represents a region where a caption is placed.
--
-- Currently, there is just four predefined region instances. The interface doesn't support configuration of region settings.
-- 
-- Phantom type for @AVCaptionRegion@.
data AVCaptionRegion

instance IsObjCObject (Id AVCaptionRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionRegion"

class IsNSObject a => IsAVCaptionRegion a where
  toAVCaptionRegion :: a -> Id AVCaptionRegion

instance IsAVCaptionRegion (Id AVCaptionRegion) where
  toAVCaptionRegion = unsafeCastId

instance IsNSObject (Id AVCaptionRegion) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionRenderer ----------

-- | AVCaptionRenderer
--
-- An instance of AVCaptionRenderer represents a service that can render the captions for a particular time
--
-- An instance of AVCaptionRenderer performs drawing of a caption "scene" from a population of captions given a time. If there are no captions or no captions at the specified time, "emptiness" will still be drawn (e.g., flood filling with zero alpha or a color).
-- 
-- Phantom type for @AVCaptionRenderer@.
data AVCaptionRenderer

instance IsObjCObject (Id AVCaptionRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionRenderer"

class IsNSObject a => IsAVCaptionRenderer a where
  toAVCaptionRenderer :: a -> Id AVCaptionRenderer

instance IsAVCaptionRenderer (Id AVCaptionRenderer) where
  toAVCaptionRenderer = unsafeCastId

instance IsNSObject (Id AVCaptionRenderer) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionRendererScene ----------

-- | AVCaptionRendererScene
--
-- An instance of AVCaptionRendererScene holds a time range and associated state indicating when the AVCaptionRenderer will draw different output.
--
-- In rendering the timeline established by the captions referenced by an AVCaptionRenderer, there are considerations such as temporal overlapping of captions, the existence of captions and other graphical elements like regions, and whether captions may be animated (e.g., scrolling in regions, character reveal in a caption). To communicate to the AVCaptionRenderer client the minimal set of time ranges where there are any visual differences, AVCaptionRendererScenes can be requested from -[AVCaptionRenderer captionSceneChangesInRange:]. A client wanting to optimize drawing performance may use this timing information to draw scenes only once per scene. Alternatively, clients can ignore scenes and repeatedly call renderInContext:atTime: but this may have additional performance impact.
--
-- Other information about the rendering of a caption scene can be communicated through the AVCaptionRendererScene. For example, if captions are animated, an AVCaptionRendererScene with the time range and an indication of the animation occurring will be returned. There should be no inference from the number of scenes to the number of captions. Even a single caption with internal animations in part of its duration could result in multiple AVCaptionRendererScenes being produced.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCaptionRendererScene@.
data AVCaptionRendererScene

instance IsObjCObject (Id AVCaptionRendererScene) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionRendererScene"

class IsNSObject a => IsAVCaptionRendererScene a where
  toAVCaptionRendererScene :: a -> Id AVCaptionRendererScene

instance IsAVCaptionRendererScene (Id AVCaptionRendererScene) where
  toAVCaptionRendererScene = unsafeCastId

instance IsNSObject (Id AVCaptionRendererScene) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionRuby ----------

-- | AVCaptionRuby
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCaptionRuby@.
data AVCaptionRuby

instance IsObjCObject (Id AVCaptionRuby) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionRuby"

class IsNSObject a => IsAVCaptionRuby a where
  toAVCaptionRuby :: a -> Id AVCaptionRuby

instance IsAVCaptionRuby (Id AVCaptionRuby) where
  toAVCaptionRuby = unsafeCastId

instance IsNSObject (Id AVCaptionRuby) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureAudioChannel ----------

-- | AVCaptureAudioChannel
--
-- AVCaptureAudioChannel represents a single channel of audio flowing through an AVCaptureSession.
--
-- An AVCaptureConnection from an input producing audio to an output receiving audio exposes an array of AVCaptureAudioChannel objects, one for each channel of audio available. Iterating through these audio channel objects, a client may poll for audio levels. Instances of AVCaptureAudioChannel cannot be created directly.
-- 
-- Phantom type for @AVCaptureAudioChannel@.
data AVCaptureAudioChannel

instance IsObjCObject (Id AVCaptureAudioChannel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureAudioChannel"

class IsNSObject a => IsAVCaptureAudioChannel a where
  toAVCaptureAudioChannel :: a -> Id AVCaptureAudioChannel

instance IsAVCaptureAudioChannel (Id AVCaptureAudioChannel) where
  toAVCaptureAudioChannel = unsafeCastId

instance IsNSObject (Id AVCaptureAudioChannel) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureBracketedStillImageSettings ----------

-- | AVCaptureBracketedStillImageSettings
--
-- AVCaptureBracketedStillImageSettings is an abstract base class that defines an interface for settings pertaining to a bracketed capture.
--
-- AVCaptureBracketedStillImageSettings may not be instantiated directly.
-- 
-- Phantom type for @AVCaptureBracketedStillImageSettings@.
data AVCaptureBracketedStillImageSettings

instance IsObjCObject (Id AVCaptureBracketedStillImageSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureBracketedStillImageSettings"

class IsNSObject a => IsAVCaptureBracketedStillImageSettings a where
  toAVCaptureBracketedStillImageSettings :: a -> Id AVCaptureBracketedStillImageSettings

instance IsAVCaptureBracketedStillImageSettings (Id AVCaptureBracketedStillImageSettings) where
  toAVCaptureBracketedStillImageSettings = unsafeCastId

instance IsNSObject (Id AVCaptureBracketedStillImageSettings) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureConnection ----------

-- | AVCaptureConnection
--
-- AVCaptureConnection represents a connection between an AVCaptureInputPort or ports, and an AVCaptureOutput or AVCaptureVideoPreviewLayer present in an AVCaptureSession.
--
-- AVCaptureInputs have one or more AVCaptureInputPorts. AVCaptureOutputs can accept data from one or more sources (example - an AVCaptureMovieFileOutput accepts both video and audio data). AVCaptureVideoPreviewLayers can accept data from one AVCaptureInputPort whose mediaType is AVMediaTypeVideo. When an input or output is added to a session, or a video preview layer is associated with a session, the session greedily forms connections between all the compatible AVCaptureInputs' ports and AVCaptureOutputs or AVCaptureVideoPreviewLayers. Iterating through an output's connections or a video preview layer's sole connection, a client may enable or disable the flow of data from a given input to a given output or preview layer.
--
-- Connections involving audio expose an array of AVCaptureAudioChannel objects, which can be used for monitoring levels.
--
-- Connections involving video expose video specific properties, such as videoMirrored and videoRotationAngle.
-- 
-- Phantom type for @AVCaptureConnection@.
data AVCaptureConnection

instance IsObjCObject (Id AVCaptureConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureConnection"

class IsNSObject a => IsAVCaptureConnection a where
  toAVCaptureConnection :: a -> Id AVCaptureConnection

instance IsAVCaptureConnection (Id AVCaptureConnection) where
  toAVCaptureConnection = unsafeCastId

instance IsNSObject (Id AVCaptureConnection) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureControl ----------

-- | AVCaptureControl
--
-- AVCaptureControl is an abstract class that defines an interface for a unique type of control which allows deep integration with the camera system through AVCaptureSession.
--
-- Various concrete subclasses of @AVCaptureControl@ are provided by AVFoundation to allow your application to both leverage common system controls and define unique custom controls.
--
-- Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- For controls that use symbols to represent them, only SF Symbols may be used.
-- 
-- Phantom type for @AVCaptureControl@.
data AVCaptureControl

instance IsObjCObject (Id AVCaptureControl) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureControl"

class IsNSObject a => IsAVCaptureControl a where
  toAVCaptureControl :: a -> Id AVCaptureControl

instance IsAVCaptureControl (Id AVCaptureControl) where
  toAVCaptureControl = unsafeCastId

instance IsNSObject (Id AVCaptureControl) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDataOutputSynchronizer ----------

-- | AVCaptureDataOutputSynchronizer
--
-- AVCaptureDataOutputSynchronizer synchronizes the delivery of data from multiple capture data outputs (AVCaptureVideoDataOutput, AVCaptureDepthDataOutput, AVCaptureMetadataOutput, AVCaptureAudioDataOutput) to a single delegate callback.
--
-- AVCaptureDataOutputSynchronizer is initialized with an array of data outputs (AVCaptureVideoDataOutput, AVCaptureDepthDataOutput, AVCaptureMetadataOutput, or AVCaptureAudioDataOutput) from which you'd like to receive a single, synchronized delegate callback. The first output in the array acts as the primary data output and determines when the synchronized callback is delivered. When data is received for the primary data output, it is held until all other data outputs have received data with an equal or later presentation time stamp, or it has been determined that there is no data for a particular output at the primary data output's pts. Once all other outputs are ready, a single delegate callback is sent with all the data aligned with the primary data output's data. Separate delegate callbacks are sent for any other data received with presentation time stamps earlier than the next primary data output time.
--
-- For instance, if you specify a video data output as your first (primary) output and a metadata output for detected faces as your second output, your data callback will not be called until there is face data ready for a video frame, or it is assured that there is no face metadata for that particular video frame.
--
-- Note that the AVCaptureDataOutputSynchronizer overrides each data output's -setSampleBufferDelegate:queue:, -setDepthDataDelegate:queue:, or -setMetadataObjectsDelegate:queue: method call. -[AVCaptureVideoDataOutput alwaysDiscardsLateVideoFrames] and -[AVCaptureDepthDataOutput alwaysDiscardsLateDepthData] properties are honored.
-- 
-- Phantom type for @AVCaptureDataOutputSynchronizer@.
data AVCaptureDataOutputSynchronizer

instance IsObjCObject (Id AVCaptureDataOutputSynchronizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDataOutputSynchronizer"

class IsNSObject a => IsAVCaptureDataOutputSynchronizer a where
  toAVCaptureDataOutputSynchronizer :: a -> Id AVCaptureDataOutputSynchronizer

instance IsAVCaptureDataOutputSynchronizer (Id AVCaptureDataOutputSynchronizer) where
  toAVCaptureDataOutputSynchronizer = unsafeCastId

instance IsNSObject (Id AVCaptureDataOutputSynchronizer) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeskViewApplication ----------

-- | AVCaptureDeskViewApplication
--
-- Allows a client to programmatically present the Desk View application and be informed when it is done being launched.
--
-- Users can launch the Desk View application through the Video Effects button in Control Center when a Desk View capable Continuity Camera is running. Developers may use this interface as a shortcut to launch the Desk View application directly from their application.
-- 
-- Phantom type for @AVCaptureDeskViewApplication@.
data AVCaptureDeskViewApplication

instance IsObjCObject (Id AVCaptureDeskViewApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeskViewApplication"

class IsNSObject a => IsAVCaptureDeskViewApplication a where
  toAVCaptureDeskViewApplication :: a -> Id AVCaptureDeskViewApplication

instance IsAVCaptureDeskViewApplication (Id AVCaptureDeskViewApplication) where
  toAVCaptureDeskViewApplication = unsafeCastId

instance IsNSObject (Id AVCaptureDeskViewApplication) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeskViewApplicationLaunchConfiguration ----------

-- | AVCaptureDeskViewApplicationLaunchConfiguration
--
-- An object for configuring how the Desk View application is presented.
--
-- Developers may use this interface to customize the presentation of the Desk View application upon launch.
-- 
-- Phantom type for @AVCaptureDeskViewApplicationLaunchConfiguration@.
data AVCaptureDeskViewApplicationLaunchConfiguration

instance IsObjCObject (Id AVCaptureDeskViewApplicationLaunchConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeskViewApplicationLaunchConfiguration"

class IsNSObject a => IsAVCaptureDeskViewApplicationLaunchConfiguration a where
  toAVCaptureDeskViewApplicationLaunchConfiguration :: a -> Id AVCaptureDeskViewApplicationLaunchConfiguration

instance IsAVCaptureDeskViewApplicationLaunchConfiguration (Id AVCaptureDeskViewApplicationLaunchConfiguration) where
  toAVCaptureDeskViewApplicationLaunchConfiguration = unsafeCastId

instance IsNSObject (Id AVCaptureDeskViewApplicationLaunchConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDevice ----------

-- | AVCaptureDevice
--
-- An AVCaptureDevice represents a physical device that provides realtime input media data, such as video and audio.
--
-- Each instance of AVCaptureDevice corresponds to a device, such as a camera or microphone. Instances of AVCaptureDevice cannot be created directly. An array of all currently available devices can also be obtained using the AVCaptureDeviceDiscoverySession. Devices can provide one or more streams of a given media type. Applications can search for devices matching desired criteria by using AVCaptureDeviceDiscoverySession, or may obtain a reference to the default device matching desired criteria by using +[AVCaptureDevice defaultDeviceWithDeviceType:mediaType:position:].
--
-- Instances of AVCaptureDevice can be used to provide media data to an AVCaptureSession by creating an AVCaptureDeviceInput with the device and adding that to the capture session.
-- 
-- Phantom type for @AVCaptureDevice@.
data AVCaptureDevice

instance IsObjCObject (Id AVCaptureDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDevice"

class IsNSObject a => IsAVCaptureDevice a where
  toAVCaptureDevice :: a -> Id AVCaptureDevice

instance IsAVCaptureDevice (Id AVCaptureDevice) where
  toAVCaptureDevice = unsafeCastId

instance IsNSObject (Id AVCaptureDevice) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeviceDiscoverySession ----------

-- | AVCaptureDeviceDiscoverySession
--
-- The AVCaptureDeviceDiscoverySession allows clients to search for devices by certain criteria.
--
-- This class allows clients to discover devices by providing certain search criteria. The objective of this class is to help find devices by device type and optionally by media type or position and allow you to key-value observe changes to the returned devices list.
-- 
-- Phantom type for @AVCaptureDeviceDiscoverySession@.
data AVCaptureDeviceDiscoverySession

instance IsObjCObject (Id AVCaptureDeviceDiscoverySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeviceDiscoverySession"

class IsNSObject a => IsAVCaptureDeviceDiscoverySession a where
  toAVCaptureDeviceDiscoverySession :: a -> Id AVCaptureDeviceDiscoverySession

instance IsAVCaptureDeviceDiscoverySession (Id AVCaptureDeviceDiscoverySession) where
  toAVCaptureDeviceDiscoverySession = unsafeCastId

instance IsNSObject (Id AVCaptureDeviceDiscoverySession) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeviceFormat ----------

-- | AVCaptureDeviceFormat
--
-- An AVCaptureDeviceFormat wraps a CMFormatDescription and other format-related information, such as min and max framerate.
--
-- An AVCaptureDevice exposes an array of formats, and its current activeFormat may be queried. The payload for the formats property is an array of AVCaptureDeviceFormat objects and the activeFormat property payload is an AVCaptureDeviceFormat. AVCaptureDeviceFormat is a thin wrapper around a CMFormatDescription, and can carry associated device format information that doesn't go in a CMFormatDescription, such as min and max frame rate. An AVCaptureDeviceFormat object is immutable. Its values do not change for the life of the object.
-- 
-- Phantom type for @AVCaptureDeviceFormat@.
data AVCaptureDeviceFormat

instance IsObjCObject (Id AVCaptureDeviceFormat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeviceFormat"

class IsNSObject a => IsAVCaptureDeviceFormat a where
  toAVCaptureDeviceFormat :: a -> Id AVCaptureDeviceFormat

instance IsAVCaptureDeviceFormat (Id AVCaptureDeviceFormat) where
  toAVCaptureDeviceFormat = unsafeCastId

instance IsNSObject (Id AVCaptureDeviceFormat) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeviceInputSource ----------

-- | AVCaptureDeviceInputSource
--
-- An AVCaptureDeviceInputSource represents a distinct input source on an AVCaptureDevice object.
--
-- An AVCaptureDevice may optionally present an array of inputSources, representing distinct mutually exclusive inputs to the device, for example, an audio AVCaptureDevice might have ADAT optical and analog input sources. A video AVCaptureDevice might have an HDMI input source, or a component input source.
-- 
-- Phantom type for @AVCaptureDeviceInputSource@.
data AVCaptureDeviceInputSource

instance IsObjCObject (Id AVCaptureDeviceInputSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeviceInputSource"

class IsNSObject a => IsAVCaptureDeviceInputSource a where
  toAVCaptureDeviceInputSource :: a -> Id AVCaptureDeviceInputSource

instance IsAVCaptureDeviceInputSource (Id AVCaptureDeviceInputSource) where
  toAVCaptureDeviceInputSource = unsafeCastId

instance IsNSObject (Id AVCaptureDeviceInputSource) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeviceRotationCoordinator ----------

-- | AVCaptureDeviceRotationCoordinator
--
-- The AVCaptureDeviceRotationCoordinator allows clients to monitor rotations of a given AVCaptureDevice instance and be provided the video rotation angle that should be applied for horizon-level preview and capture relative to gravity.
--
-- Each instance of AVCaptureDeviceRotationCoordinator allows a client to coordinate with changes to the rotation of an AVCaptureDevice to ensure the camera's video preview and captured output are horizon-level. The coordinator delivers key-value updates on the main queue.
-- 
-- Phantom type for @AVCaptureDeviceRotationCoordinator@.
data AVCaptureDeviceRotationCoordinator

instance IsObjCObject (Id AVCaptureDeviceRotationCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeviceRotationCoordinator"

class IsNSObject a => IsAVCaptureDeviceRotationCoordinator a where
  toAVCaptureDeviceRotationCoordinator :: a -> Id AVCaptureDeviceRotationCoordinator

instance IsAVCaptureDeviceRotationCoordinator (Id AVCaptureDeviceRotationCoordinator) where
  toAVCaptureDeviceRotationCoordinator = unsafeCastId

instance IsNSObject (Id AVCaptureDeviceRotationCoordinator) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureExternalDisplayConfiguration ----------

-- | A class you use to specify a configuration to your external display configurator.
--
-- Using an ``AVCaptureExternalDisplayConfiguration``, you direct your ``AVCaptureExternalDisplayConfigurator`` how to configure an external display to match your device's active video format.
-- 
-- Phantom type for @AVCaptureExternalDisplayConfiguration@.
data AVCaptureExternalDisplayConfiguration

instance IsObjCObject (Id AVCaptureExternalDisplayConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureExternalDisplayConfiguration"

class IsNSObject a => IsAVCaptureExternalDisplayConfiguration a where
  toAVCaptureExternalDisplayConfiguration :: a -> Id AVCaptureExternalDisplayConfiguration

instance IsAVCaptureExternalDisplayConfiguration (Id AVCaptureExternalDisplayConfiguration) where
  toAVCaptureExternalDisplayConfiguration = unsafeCastId

instance IsNSObject (Id AVCaptureExternalDisplayConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureExternalDisplayConfigurator ----------

-- | A configurator class allowing you to configure properties of an external display to match the camera's active video format.
--
-- An ``AVCaptureExternalDisplayConfigurator`` allows you to configure a connected external display to output a clean feed using a ``CALayer``. Using the configurator, you can opt into automatic adjustment of the external display’s color space and / or frame rate to match your device’s capture configuration. These adjustments are only applied to the external display, not to the device.
--
-- - Note: Not all displays support the same configuration options as the device’s capture formats. Your adjustments to the external display are applied with utmost effort to accurately represent the capture device. When your capture device's ``AVCaptureDevice/activeFormat`` is unavailable on the external display, the configurator automatically chooses the closest available format.
-- 
-- Phantom type for @AVCaptureExternalDisplayConfigurator@.
data AVCaptureExternalDisplayConfigurator

instance IsObjCObject (Id AVCaptureExternalDisplayConfigurator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureExternalDisplayConfigurator"

class IsNSObject a => IsAVCaptureExternalDisplayConfigurator a where
  toAVCaptureExternalDisplayConfigurator :: a -> Id AVCaptureExternalDisplayConfigurator

instance IsAVCaptureExternalDisplayConfigurator (Id AVCaptureExternalDisplayConfigurator) where
  toAVCaptureExternalDisplayConfigurator = unsafeCastId

instance IsNSObject (Id AVCaptureExternalDisplayConfigurator) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureFraming ----------

-- | A framing, consisting of an aspect ratio and a zoom factor.
--
-- An ``AVCaptureSmartFramingMonitor`` provides framing recommendations using this object.
-- 
-- Phantom type for @AVCaptureFraming@.
data AVCaptureFraming

instance IsObjCObject (Id AVCaptureFraming) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureFraming"

class IsNSObject a => IsAVCaptureFraming a where
  toAVCaptureFraming :: a -> Id AVCaptureFraming

instance IsAVCaptureFraming (Id AVCaptureFraming) where
  toAVCaptureFraming = unsafeCastId

instance IsNSObject (Id AVCaptureFraming) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureInput ----------

-- | AVCaptureInput
--
-- AVCaptureInput is an abstract class that provides an interface for connecting capture input sources to an AVCaptureSession.
--
-- Concrete instances of AVCaptureInput representing input sources such as cameras can be added to instances of AVCaptureSession using the -[AVCaptureSession addInput:] method. An AVCaptureInput vends one or more streams of media data. For example, input devices can provide both audio and video data. Each media stream provided by an input is represented by an AVCaptureInputPort object. Within a capture session, connections are made between AVCaptureInput instances and AVCaptureOutput instances via AVCaptureConnection objects that define the mapping between a set of AVCaptureInputPort objects and a single AVCaptureOutput.
-- 
-- Phantom type for @AVCaptureInput@.
data AVCaptureInput

instance IsObjCObject (Id AVCaptureInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureInput"

class IsNSObject a => IsAVCaptureInput a where
  toAVCaptureInput :: a -> Id AVCaptureInput

instance IsAVCaptureInput (Id AVCaptureInput) where
  toAVCaptureInput = unsafeCastId

instance IsNSObject (Id AVCaptureInput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureInputPort ----------

-- | AVCaptureInputPort
--
-- An AVCaptureInputPort describes a single stream of media data provided by an AVCaptureInput and provides an interface for connecting that stream to AVCaptureOutput instances via AVCaptureConnection.
--
-- Instances of AVCaptureInputPort cannot be created directly. An AVCaptureInput exposes its input ports via its ports property. Input ports provide information about the format of their media data via the mediaType and formatDescription properties, and allow clients to control the flow of data via the enabled property. Input ports are used by an AVCaptureConnection to define the mapping between inputs and outputs in an AVCaptureSession.
-- 
-- Phantom type for @AVCaptureInputPort@.
data AVCaptureInputPort

instance IsObjCObject (Id AVCaptureInputPort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureInputPort"

class IsNSObject a => IsAVCaptureInputPort a where
  toAVCaptureInputPort :: a -> Id AVCaptureInputPort

instance IsAVCaptureInputPort (Id AVCaptureInputPort) where
  toAVCaptureInputPort = unsafeCastId

instance IsNSObject (Id AVCaptureInputPort) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureOutput ----------

-- | AVCaptureOutput
--
-- AVCaptureOutput is an abstract class that defines an interface for an output destination of an AVCaptureSession.
--
-- AVCaptureOutput provides an abstract interface for connecting capture output destinations, such as files and video previews, to an AVCaptureSession.
--
-- An AVCaptureOutput can have multiple connections represented by AVCaptureConnection objects, one for each stream of media that it receives from an AVCaptureInput. An AVCaptureOutput does not have any connections when it is first created. When an output is added to an AVCaptureSession, connections are created that map media data from that session's inputs to its outputs.
--
-- Concrete AVCaptureOutput instances can be added to an AVCaptureSession using the -[AVCaptureSession addOutput:] and -[AVCaptureSession addOutputWithNoConnections:] methods.
-- 
-- Phantom type for @AVCaptureOutput@.
data AVCaptureOutput

instance IsObjCObject (Id AVCaptureOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureOutput"

class IsNSObject a => IsAVCaptureOutput a where
  toAVCaptureOutput :: a -> Id AVCaptureOutput

instance IsAVCaptureOutput (Id AVCaptureOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCapturePhoto ----------

-- | AVCapturePhoto
--
-- An object representing a photo in memory, produced by the -captureOutput:didFinishingProcessingPhoto:error: in the AVCapturePhotoCaptureDelegate protocol method.
--
-- Beginning in iOS 11, AVCapturePhotoOutput's AVCapturePhotoCaptureDelegate supports a simplified callback for delivering image data, namely -captureOutput:didFinishingProcessingPhoto:error:. This callback presents each image result for your capture request as an AVCapturePhoto object, an immutable wrapper from which various properties of the photo capture may be queried, such as the photo's preview pixel buffer, metadata, depth data, camera calibration data, and image bracket specific properties. AVCapturePhoto can wrap file-containerized photo results, such as HEVC encoded image data, containerized in the HEIC file format. CMSampleBufferRef, on the other hand, may only be used to express non file format containerized photo data. For this reason, the AVCapturePhotoCaptureDelegate protocol methods that return CMSampleBuffers have been deprecated in favor of -captureOutput:didFinishingProcessingPhoto:error:. A AVCapturePhoto wraps a single image result. For instance, if you've requested a bracketed capture of 3 images, your callback is called 3 times, each time delivering an AVCapturePhoto.
-- 
-- Phantom type for @AVCapturePhoto@.
data AVCapturePhoto

instance IsObjCObject (Id AVCapturePhoto) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCapturePhoto"

class IsNSObject a => IsAVCapturePhoto a where
  toAVCapturePhoto :: a -> Id AVCapturePhoto

instance IsAVCapturePhoto (Id AVCapturePhoto) where
  toAVCapturePhoto = unsafeCastId

instance IsNSObject (Id AVCapturePhoto) where
  toNSObject = unsafeCastId

-- ---------- AVCapturePhotoOutputReadinessCoordinator ----------

-- | AVCapturePhotoOutputReadinessCoordinator
--
-- AVCapturePhotoOutputReadinessCoordinator notifies its delegate of changes in an AVCapturePhotoOutput's captureReadiness property and can be used to coordinate UI updates on the main queue with use of AVCapturePhotoOutput on a background queue.
--
-- AVCapturePhotoOutputReadinessCoordinator tracks its output's captureReadiness and incorporates additional requests registered via -startTrackingCaptureRequestUsingPhotoSettings:. This allows clients to synchronously update shutter button availability and appearance and on the main thread while calling -[AVCapturePhotoOutput capturePhotoWithSettings:delegate:] asynchronously on a background queue.
-- 
-- Phantom type for @AVCapturePhotoOutputReadinessCoordinator@.
data AVCapturePhotoOutputReadinessCoordinator

instance IsObjCObject (Id AVCapturePhotoOutputReadinessCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCapturePhotoOutputReadinessCoordinator"

class IsNSObject a => IsAVCapturePhotoOutputReadinessCoordinator a where
  toAVCapturePhotoOutputReadinessCoordinator :: a -> Id AVCapturePhotoOutputReadinessCoordinator

instance IsAVCapturePhotoOutputReadinessCoordinator (Id AVCapturePhotoOutputReadinessCoordinator) where
  toAVCapturePhotoOutputReadinessCoordinator = unsafeCastId

instance IsNSObject (Id AVCapturePhotoOutputReadinessCoordinator) where
  toNSObject = unsafeCastId

-- ---------- AVCapturePhotoSettings ----------

-- | AVCapturePhotoSettings
--
-- A mutable settings object encapsulating all the desired properties of a photo capture.
--
-- To take a picture, a client instantiates and configures an AVCapturePhotoSettings object, then calls AVCapturePhotoOutput's -capturePhotoWithSettings:delegate:, passing the settings and a delegate to be informed when events relating to the photo capture occur. Since AVCapturePhotoSettings has no reference to the AVCapturePhotoOutput instance with which it will be used, minimal validation occurs while you configure an AVCapturePhotoSettings instance. The bulk of the validation is executed when you call AVCapturePhotoOutput's -capturePhotoWithSettings:delegate:.
-- 
-- Phantom type for @AVCapturePhotoSettings@.
data AVCapturePhotoSettings

instance IsObjCObject (Id AVCapturePhotoSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCapturePhotoSettings"

class IsNSObject a => IsAVCapturePhotoSettings a where
  toAVCapturePhotoSettings :: a -> Id AVCapturePhotoSettings

instance IsAVCapturePhotoSettings (Id AVCapturePhotoSettings) where
  toAVCapturePhotoSettings = unsafeCastId

instance IsNSObject (Id AVCapturePhotoSettings) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureReactionEffectState ----------

-- | AVCaptureReactionEffectState
--
-- Reports the state of a reaction performed on an AVCaptureDevice.
--
-- AVCaptureReactionEffectState may be obtained by calling -[AVCaptureDevice reactionEffectsInProgress].  When -[AVCaptureDevice canPerformReactionEffects] returns YES, new entries are added either by calling -[AVCaptureDevice performEffectForReaction:], or by gesture detection in the capture stream when AVCaptureDevice.reactionEffectGesturesEnabled.  The effect rendering is done before frames are given to the capture client, and these status objects let you know when these effects are performed.
-- 
-- Phantom type for @AVCaptureReactionEffectState@.
data AVCaptureReactionEffectState

instance IsObjCObject (Id AVCaptureReactionEffectState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureReactionEffectState"

class IsNSObject a => IsAVCaptureReactionEffectState a where
  toAVCaptureReactionEffectState :: a -> Id AVCaptureReactionEffectState

instance IsAVCaptureReactionEffectState (Id AVCaptureReactionEffectState) where
  toAVCaptureReactionEffectState = unsafeCastId

instance IsNSObject (Id AVCaptureReactionEffectState) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureResolvedPhotoSettings ----------

-- | AVCaptureResolvedPhotoSettings
--
-- An immutable object produced by callbacks in each and every AVCapturePhotoCaptureDelegate protocol method.
--
-- When you initiate a photo capture request using -capturePhotoWithSettings:delegate:, some of your settings are not yet certain. For instance, auto flash and auto still image stabilization allow the AVCapturePhotoOutput to decide just in time whether to employ flash or still image stabilization, depending on the current scene. Once the request is issued, AVCapturePhotoOutput begins the capture, resolves the uncertain settings, and in its first callback informs you of its choices through an AVCaptureResolvedPhotoSettings object. This same object is presented to all the callbacks fired for a particular photo capture request. Its uniqueID property matches that of the AVCapturePhotoSettings instance you used to initiate the photo request.
-- 
-- Phantom type for @AVCaptureResolvedPhotoSettings@.
data AVCaptureResolvedPhotoSettings

instance IsObjCObject (Id AVCaptureResolvedPhotoSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureResolvedPhotoSettings"

class IsNSObject a => IsAVCaptureResolvedPhotoSettings a where
  toAVCaptureResolvedPhotoSettings :: a -> Id AVCaptureResolvedPhotoSettings

instance IsAVCaptureResolvedPhotoSettings (Id AVCaptureResolvedPhotoSettings) where
  toAVCaptureResolvedPhotoSettings = unsafeCastId

instance IsNSObject (Id AVCaptureResolvedPhotoSettings) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSession ----------

-- | AVCaptureSession
--
-- AVCaptureSession is the central hub of the AVFoundation capture classes.
--
-- To perform a real-time capture, a client may instantiate AVCaptureSession and add appropriate AVCaptureInputs, such as AVCaptureDeviceInput, and outputs, such as AVCaptureMovieFileOutput. [AVCaptureSession startRunning] starts the flow of data from the inputs to the outputs, and [AVCaptureSession stopRunning] stops the flow. A client may set the sessionPreset property to customize the quality level or bitrate of the output.
-- 
-- Phantom type for @AVCaptureSession@.
data AVCaptureSession

instance IsObjCObject (Id AVCaptureSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSession"

class IsNSObject a => IsAVCaptureSession a where
  toAVCaptureSession :: a -> Id AVCaptureSession

instance IsAVCaptureSession (Id AVCaptureSession) where
  toAVCaptureSession = unsafeCastId

instance IsNSObject (Id AVCaptureSession) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSmartFramingMonitor ----------

-- | An object associated with a capture device that monitors the scene and suggests an optimal framing.
--
-- A smart framing monitor observes its associated device for objects of interest entering and exiting the camera's field of view and recommends an optimal framing for good photographic composition. This framing recommendation consists of an aspect ratio and zoom factor. You may respond to the device's framing recommendation by calling ``AVCaptureDevice/setDynamicAspectRatio:completionHandler:`` and setting ``AVCaptureDevice/videoZoomFactor`` on the associated device in whatever order best matches your animation between old and new framings.
-- 
-- Phantom type for @AVCaptureSmartFramingMonitor@.
data AVCaptureSmartFramingMonitor

instance IsObjCObject (Id AVCaptureSmartFramingMonitor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSmartFramingMonitor"

class IsNSObject a => IsAVCaptureSmartFramingMonitor a where
  toAVCaptureSmartFramingMonitor :: a -> Id AVCaptureSmartFramingMonitor

instance IsAVCaptureSmartFramingMonitor (Id AVCaptureSmartFramingMonitor) where
  toAVCaptureSmartFramingMonitor = unsafeCastId

instance IsNSObject (Id AVCaptureSmartFramingMonitor) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSpatialAudioMetadataSampleGenerator ----------

-- | An interface for generating a spatial audio timed metadata sample.
-- 
-- Phantom type for @AVCaptureSpatialAudioMetadataSampleGenerator@.
data AVCaptureSpatialAudioMetadataSampleGenerator

instance IsObjCObject (Id AVCaptureSpatialAudioMetadataSampleGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSpatialAudioMetadataSampleGenerator"

class IsNSObject a => IsAVCaptureSpatialAudioMetadataSampleGenerator a where
  toAVCaptureSpatialAudioMetadataSampleGenerator :: a -> Id AVCaptureSpatialAudioMetadataSampleGenerator

instance IsAVCaptureSpatialAudioMetadataSampleGenerator (Id AVCaptureSpatialAudioMetadataSampleGenerator) where
  toAVCaptureSpatialAudioMetadataSampleGenerator = unsafeCastId

instance IsNSObject (Id AVCaptureSpatialAudioMetadataSampleGenerator) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSynchronizedData ----------

-- | AVCaptureSynchronizedData
--
-- An abstract base class representing the data delivered by a data output through the AVCaptureDataOutputSynchronizer interface.
--
-- AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedData: delegate callback delivers a dictionary of key/value pairs, with the keys being the AVCaptureOutput instances returning data, and the values being concrete subclasses of AVCaptureSynchronizedData.
-- 
-- Phantom type for @AVCaptureSynchronizedData@.
data AVCaptureSynchronizedData

instance IsObjCObject (Id AVCaptureSynchronizedData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSynchronizedData"

class IsNSObject a => IsAVCaptureSynchronizedData a where
  toAVCaptureSynchronizedData :: a -> Id AVCaptureSynchronizedData

instance IsAVCaptureSynchronizedData (Id AVCaptureSynchronizedData) where
  toAVCaptureSynchronizedData = unsafeCastId

instance IsNSObject (Id AVCaptureSynchronizedData) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSynchronizedDataCollection ----------

-- | AVCaptureSynchronizedDataCollection
--
-- A collection of AVCaptureSynchronizedData objects.
--
-- AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedDataCollection: delegate method delivers a collection of AVCaptureSynchronizedData objects which can be iterated by AVCaptureOutput. AVCaptureSynchronizedDataCollection supports object subscripting and fast enumeration of the data outputs as keys.
-- 
-- Phantom type for @AVCaptureSynchronizedDataCollection@.
data AVCaptureSynchronizedDataCollection

instance IsObjCObject (Id AVCaptureSynchronizedDataCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSynchronizedDataCollection"

class IsNSObject a => IsAVCaptureSynchronizedDataCollection a where
  toAVCaptureSynchronizedDataCollection :: a -> Id AVCaptureSynchronizedDataCollection

instance IsAVCaptureSynchronizedDataCollection (Id AVCaptureSynchronizedDataCollection) where
  toAVCaptureSynchronizedDataCollection = unsafeCastId

instance IsNSObject (Id AVCaptureSynchronizedDataCollection) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSystemPressureState ----------

-- | AVCaptureSystemPressureState
--
-- A model object describing a system pressure level and contributing factors to the pressured state.
--
-- Beginning in iOS 11.1, AVCaptureDevice can report its current system pressure state. System pressure refers to a state in which capture quality is degraded or capture hardware availability is limited due to factors such as overall system temperature, insufficient battery charge for current peak power requirements, or camera module temperature.
-- 
-- Phantom type for @AVCaptureSystemPressureState@.
data AVCaptureSystemPressureState

instance IsObjCObject (Id AVCaptureSystemPressureState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSystemPressureState"

class IsNSObject a => IsAVCaptureSystemPressureState a where
  toAVCaptureSystemPressureState :: a -> Id AVCaptureSystemPressureState

instance IsAVCaptureSystemPressureState (Id AVCaptureSystemPressureState) where
  toAVCaptureSystemPressureState = unsafeCastId

instance IsNSObject (Id AVCaptureSystemPressureState) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureTimecodeGenerator ----------

-- | Generates and synchronizes timecode data from various sources for precise video and audio synchronization.
--
-- The ``AVCaptureTimecodeGenerator`` class supports multiple timecode sources, including frame counting, system clock synchronization, and MIDI timecode input (MTC). Suitable for playback, recording, or other time-sensitive operations where precise timecode metadata is required.
--
-- Use the ``startSynchronizationWithTimecodeSource:`` method to set up the desired timecode source.
-- 
-- Phantom type for @AVCaptureTimecodeGenerator@.
data AVCaptureTimecodeGenerator

instance IsObjCObject (Id AVCaptureTimecodeGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureTimecodeGenerator"

class IsNSObject a => IsAVCaptureTimecodeGenerator a where
  toAVCaptureTimecodeGenerator :: a -> Id AVCaptureTimecodeGenerator

instance IsAVCaptureTimecodeGenerator (Id AVCaptureTimecodeGenerator) where
  toAVCaptureTimecodeGenerator = unsafeCastId

instance IsNSObject (Id AVCaptureTimecodeGenerator) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureTimecodeSource ----------

-- | Describes a timecode source that a timecode generator can synchronize to.
--
-- @AVCaptureTimecodeSource@ provides information about a specific timecode source available for synchronization in @AVCaptureTimecodeGenerator@. It includes metadata such as the source’s name, type, and unique identifier.
-- 
-- Phantom type for @AVCaptureTimecodeSource@.
data AVCaptureTimecodeSource

instance IsObjCObject (Id AVCaptureTimecodeSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureTimecodeSource"

class IsNSObject a => IsAVCaptureTimecodeSource a where
  toAVCaptureTimecodeSource :: a -> Id AVCaptureTimecodeSource

instance IsAVCaptureTimecodeSource (Id AVCaptureTimecodeSource) where
  toAVCaptureTimecodeSource = unsafeCastId

instance IsNSObject (Id AVCaptureTimecodeSource) where
  toNSObject = unsafeCastId

-- ---------- AVCompositionTrackFormatDescriptionReplacement ----------

-- | AVCompositionTrackFormatDescriptionReplacement
--
-- A format description and its replacement.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCompositionTrackFormatDescriptionReplacement@.
data AVCompositionTrackFormatDescriptionReplacement

instance IsObjCObject (Id AVCompositionTrackFormatDescriptionReplacement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCompositionTrackFormatDescriptionReplacement"

class IsNSObject a => IsAVCompositionTrackFormatDescriptionReplacement a where
  toAVCompositionTrackFormatDescriptionReplacement :: a -> Id AVCompositionTrackFormatDescriptionReplacement

instance IsAVCompositionTrackFormatDescriptionReplacement (Id AVCompositionTrackFormatDescriptionReplacement) where
  toAVCompositionTrackFormatDescriptionReplacement = unsafeCastId

instance IsNSObject (Id AVCompositionTrackFormatDescriptionReplacement) where
  toNSObject = unsafeCastId

-- ---------- AVContentKey ----------

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVContentKey@.
data AVContentKey

instance IsObjCObject (Id AVContentKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVContentKey"

class IsNSObject a => IsAVContentKey a where
  toAVContentKey :: a -> Id AVContentKey

instance IsAVContentKey (Id AVContentKey) where
  toAVContentKey = unsafeCastId

instance IsNSObject (Id AVContentKey) where
  toNSObject = unsafeCastId

-- ---------- AVContentKeyRequest ----------

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVContentKeyRequest@.
data AVContentKeyRequest

instance IsObjCObject (Id AVContentKeyRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVContentKeyRequest"

class IsNSObject a => IsAVContentKeyRequest a where
  toAVContentKeyRequest :: a -> Id AVContentKeyRequest

instance IsAVContentKeyRequest (Id AVContentKeyRequest) where
  toAVContentKeyRequest = unsafeCastId

instance IsNSObject (Id AVContentKeyRequest) where
  toNSObject = unsafeCastId

-- ---------- AVContentKeyResponse ----------

-- | AVContentKeyResponse is used to represent the data returned from the key server when requesting a key for decrypting content.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVContentKeyResponse@.
data AVContentKeyResponse

instance IsObjCObject (Id AVContentKeyResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVContentKeyResponse"

class IsNSObject a => IsAVContentKeyResponse a where
  toAVContentKeyResponse :: a -> Id AVContentKeyResponse

instance IsAVContentKeyResponse (Id AVContentKeyResponse) where
  toAVContentKeyResponse = unsafeCastId

instance IsNSObject (Id AVContentKeyResponse) where
  toNSObject = unsafeCastId

-- ---------- AVContentKeySession ----------

-- | An AVContentKeySession is used to create and track decryption keys for media data. Objects conforming to the AVContentKeyRecipient protocol, such as AVURLAssets, can be added to an AVContentKeySession to employ the services of the AVContentKeySession in handling new key requests and to obtain access to the session's already existing keys.
--
-- Its secondary purpose is to provide a report of expired sessions to assist a controlling entity that wishes to track the set of sessions that are still active. If initialized with a location at which to store them, AVContentKeySession maintains a global collection of pending "expired session reports", each associated with an identifier for the app that created the session. The contents of this identifier are specified by the controlling entity that provides media data or that grants permission for its use.
--
-- Expired sessions are tracked as follows: a stream processing session is considered to be started after an instance of AVContentKeySession is created and the first object conforming to the AVContentKeyRecipient protocol is added to it. If an instance of AVContentKeySession that has reached this state does not receive an expire message before it's deallocated or the process in which it's running is terminated, an "expired session report" will subsequently be added to the pending list of expired session reports that indicates that the session expired abnormally. In contrast, for AVContentKeySessions that reach the state of having at least one object conforming to the AVContentKeyRecipient protocol added to them and later receive an expire message, "expired session reports" will be generated that indicate that the session expired normally.
--
-- To obtain the collection of pending expired session reports in order to provide them to the controlling entity associated with a specific app identifier, use +pendingExpiredSessionReportsWithAppIdentifier:.
--
-- After pending expired session reports have been sent to the controlling entity and their receipt has been acknowledged, they can be removed from the collection of pending expired session reports maintained by AVContentKeySession by using +removePendingExpiredSessionReports:withAppIdentifier:.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVContentKeySession@.
data AVContentKeySession

instance IsObjCObject (Id AVContentKeySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVContentKeySession"

class IsNSObject a => IsAVContentKeySession a where
  toAVContentKeySession :: a -> Id AVContentKeySession

instance IsAVContentKeySession (Id AVContentKeySession) where
  toAVContentKeySession = unsafeCastId

instance IsNSObject (Id AVContentKeySession) where
  toNSObject = unsafeCastId

-- ---------- AVContentKeySpecifier ----------

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVContentKeySpecifier@.
data AVContentKeySpecifier

instance IsObjCObject (Id AVContentKeySpecifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVContentKeySpecifier"

class IsNSObject a => IsAVContentKeySpecifier a where
  toAVContentKeySpecifier :: a -> Id AVContentKeySpecifier

instance IsAVContentKeySpecifier (Id AVContentKeySpecifier) where
  toAVContentKeySpecifier = unsafeCastId

instance IsNSObject (Id AVContentKeySpecifier) where
  toNSObject = unsafeCastId

-- ---------- AVContinuityDevice ----------

-- | AVContinuityDevice
--
-- An AVContinuityDevice represents a physical iOS device that provides capture devices and audio session ports.
--
-- Each instance of AVContinuityDevice corresponds to a continuity device, such as an iPhone or iPad. Instances of AVContinuityDevice cannot be created directly.
-- 
-- Phantom type for @AVContinuityDevice@.
data AVContinuityDevice

instance IsObjCObject (Id AVContinuityDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVContinuityDevice"

class IsNSObject a => IsAVContinuityDevice a where
  toAVContinuityDevice :: a -> Id AVContinuityDevice

instance IsAVContinuityDevice (Id AVContinuityDevice) where
  toAVContinuityDevice = unsafeCastId

instance IsNSObject (Id AVContinuityDevice) where
  toNSObject = unsafeCastId

-- ---------- AVCoordinatedPlaybackParticipant ----------

-- | A participant in a coordinated playback group connected through AVPlaybackCoordinator.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCoordinatedPlaybackParticipant@.
data AVCoordinatedPlaybackParticipant

instance IsObjCObject (Id AVCoordinatedPlaybackParticipant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCoordinatedPlaybackParticipant"

class IsNSObject a => IsAVCoordinatedPlaybackParticipant a where
  toAVCoordinatedPlaybackParticipant :: a -> Id AVCoordinatedPlaybackParticipant

instance IsAVCoordinatedPlaybackParticipant (Id AVCoordinatedPlaybackParticipant) where
  toAVCoordinatedPlaybackParticipant = unsafeCastId

instance IsNSObject (Id AVCoordinatedPlaybackParticipant) where
  toNSObject = unsafeCastId

-- ---------- AVCoordinatedPlaybackSuspension ----------

-- | A representation of a temporary break in participation.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- - NOTE: See AVPlaybackCoordinator's beginSuspensionForReason: method for details on use.
-- 
-- Phantom type for @AVCoordinatedPlaybackSuspension@.
data AVCoordinatedPlaybackSuspension

instance IsObjCObject (Id AVCoordinatedPlaybackSuspension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCoordinatedPlaybackSuspension"

class IsNSObject a => IsAVCoordinatedPlaybackSuspension a where
  toAVCoordinatedPlaybackSuspension :: a -> Id AVCoordinatedPlaybackSuspension

instance IsAVCoordinatedPlaybackSuspension (Id AVCoordinatedPlaybackSuspension) where
  toAVCoordinatedPlaybackSuspension = unsafeCastId

instance IsNSObject (Id AVCoordinatedPlaybackSuspension) where
  toNSObject = unsafeCastId

-- ---------- AVCustomMediaSelectionScheme ----------

-- | For content that has been authored with the express intent of offering an alternative selection interface for AVMediaSelectionOptions, AVCustomMediaSelectionScheme provides a collection of custom settings for controlling the presentation of the media.
--
-- Each selectable setting is associated with a media characteristic that one or more of the AVMediaSelectionOptions in the AVMediaSelectionGroup possesses. By selecting a setting in a user interface based on an AVCustomMediaSelectionScheme, users are essentially indicating a preference for the media characteristic of the selected setting. Selection of a specific AVMediaSelectionOption in the AVMediaSelectionGroup is then derived from the user's indicated preferences. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCustomMediaSelectionScheme@.
data AVCustomMediaSelectionScheme

instance IsObjCObject (Id AVCustomMediaSelectionScheme) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCustomMediaSelectionScheme"

class IsNSObject a => IsAVCustomMediaSelectionScheme a where
  toAVCustomMediaSelectionScheme :: a -> Id AVCustomMediaSelectionScheme

instance IsAVCustomMediaSelectionScheme (Id AVCustomMediaSelectionScheme) where
  toAVCustomMediaSelectionScheme = unsafeCastId

instance IsNSObject (Id AVCustomMediaSelectionScheme) where
  toNSObject = unsafeCastId

-- ---------- AVDelegatingPlaybackCoordinatorPlaybackControlCommand ----------

-- | Abstract superclass for playback commands
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVDelegatingPlaybackCoordinatorPlaybackControlCommand@.
data AVDelegatingPlaybackCoordinatorPlaybackControlCommand

instance IsObjCObject (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDelegatingPlaybackCoordinatorPlaybackControlCommand"

class IsNSObject a => IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand a where
  toAVDelegatingPlaybackCoordinatorPlaybackControlCommand :: a -> Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand

instance IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand) where
  toAVDelegatingPlaybackCoordinatorPlaybackControlCommand = unsafeCastId

instance IsNSObject (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand) where
  toNSObject = unsafeCastId

-- ---------- AVDepthData ----------

-- | AVDepthData
--
-- An object wrapping a map of disparity or depth pixel data, plus metadata.
--
-- "Depth Data" is a generic term for a map of pixel data containing depth-related information. AVDepthData wraps a disparity or depth map and provides conversion methods, focus information, and camera calibration data to aid in using the map for rendering or computer vision tasks. CoreVideo supports the following four depth data pixel formats:          kCVPixelFormatType_DisparityFloat16    = 'hdis'          kCVPixelFormatType_DisparityFloat32    = 'fdis'          kCVPixelFormatType_DepthFloat16        = 'hdep'          kCVPixelFormatType_DepthFloat32        = 'fdep'
--
-- The disparity formats describe normalized shift values when comparing two images. Units are 1/meters: ( pixelShift / (pixelFocalLength * baselineInMeters) ).     The depth formats describe the distance to an object in meters.
--
-- Disparity / depth maps are generated from camera images containing non-rectilinear data. Camera lenses have small imperfections that cause small distortions in their resultant images compared to a pinhole camera. AVDepthData maps contain non-rectilinear (non-distortion-corrected) data as well. Their values are warped to match the lens distortion characteristics present in their accompanying YUV image. Therefore an AVDepthData map can be used as a proxy for depth when rendering effects to its accompanying image, but not to correlate points in 3D space. In order to use AVDepthData for computer vision tasks, you should use its accompanying camera calibration data to rectify the depth data (see AVCameraCalibrationData).
--
-- When capturing depth data from a camera using AVCaptureDepthDataOutput, AVDepthData objects are delivered to your AVCaptureDepthDataOutputDelegate in a streaming fashion. When capturing depth data along with photos using AVCapturePhotoOutput, depth data is delivered to your AVCapturePhotoCaptureDelegate as a property of an AVCapturePhoto (see -[AVCapturePhotoCaptureDelegate captureOutput:didFinishProcessingPhoto:error:]). When working with image files containing depth information, AVDepthData may be instantiated using information obtained from ImageIO. When editing images containing depth information, derivative AVDepthData objects may be instantiated reflecting the edits that have been performed.
-- 
-- Phantom type for @AVDepthData@.
data AVDepthData

instance IsObjCObject (Id AVDepthData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDepthData"

class IsNSObject a => IsAVDepthData a where
  toAVDepthData :: a -> Id AVDepthData

instance IsAVDepthData (Id AVDepthData) where
  toAVDepthData = unsafeCastId

instance IsNSObject (Id AVDepthData) where
  toNSObject = unsafeCastId

-- ---------- AVExposureBiasRange ----------

-- | AVExposureBiasRange
--
-- An AVExposureBiasRange expresses an inclusive range of supported exposure bias values, in EV units.
--
-- This is used by AVCaptureSystemExposureBiasSlider for the range the slider uses.
-- 
-- Phantom type for @AVExposureBiasRange@.
data AVExposureBiasRange

instance IsObjCObject (Id AVExposureBiasRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExposureBiasRange"

class IsNSObject a => IsAVExposureBiasRange a where
  toAVExposureBiasRange :: a -> Id AVExposureBiasRange

instance IsAVExposureBiasRange (Id AVExposureBiasRange) where
  toAVExposureBiasRange = unsafeCastId

instance IsNSObject (Id AVExposureBiasRange) where
  toNSObject = unsafeCastId

-- ---------- AVExternalStorageDevice ----------

-- | AVExternalStorageDevice
--
-- An AVExternalStorageDevice represents a physical external storage device connected to the device that can be used to store captured media assets.
--
-- Each instance of AVExternalStorageDevice corresponds to a physical external storage device where captured media assets can be stored. Instances of AVExternalStorageDevice cannot be created directly. An array of all currently available external storage devices can be obtained using AVExternalStorageDeviceDiscoverySession.
--
-- Instances of AVExternalStorageDevice can be used with AVCaptureFileOutput subclasses for writing media files.
-- 
-- Phantom type for @AVExternalStorageDevice@.
data AVExternalStorageDevice

instance IsObjCObject (Id AVExternalStorageDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExternalStorageDevice"

class IsNSObject a => IsAVExternalStorageDevice a where
  toAVExternalStorageDevice :: a -> Id AVExternalStorageDevice

instance IsAVExternalStorageDevice (Id AVExternalStorageDevice) where
  toAVExternalStorageDevice = unsafeCastId

instance IsNSObject (Id AVExternalStorageDevice) where
  toNSObject = unsafeCastId

-- ---------- AVExternalStorageDeviceDiscoverySession ----------

-- | AVExternalStorageDeviceDiscoverySession
--
-- AVExternalStorageDeviceDiscoverySession is used to monitor connection / disconnection of external storage devices to the device.
--
-- AVExternalStorageDeviceDiscoverySession is a singleton that lists the external storage devices connected to this device. The client is expected to key-value observe the externalStorageDevices property for changes to the external storage devices list.
-- 
-- Phantom type for @AVExternalStorageDeviceDiscoverySession@.
data AVExternalStorageDeviceDiscoverySession

instance IsObjCObject (Id AVExternalStorageDeviceDiscoverySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExternalStorageDeviceDiscoverySession"

class IsNSObject a => IsAVExternalStorageDeviceDiscoverySession a where
  toAVExternalStorageDeviceDiscoverySession :: a -> Id AVExternalStorageDeviceDiscoverySession

instance IsAVExternalStorageDeviceDiscoverySession (Id AVExternalStorageDeviceDiscoverySession) where
  toAVExternalStorageDeviceDiscoverySession = unsafeCastId

instance IsNSObject (Id AVExternalStorageDeviceDiscoverySession) where
  toNSObject = unsafeCastId

-- ---------- AVExternalSyncDevice ----------

-- | An external sync device connected to a host device that can be used to drive the timing of an internal component, such as a camera sensor.
--
-- Each instance of ``AVExternalSyncDevice`` corresponds to a physical external device that can drive an internal component, like a camera readout. You cannot create instances of ``AVExternalSyncDevice``. Instead, you obtain an array of all currently available external sync devices using ``AVExternalSyncDeviceDiscoverySession``.
-- 
-- Phantom type for @AVExternalSyncDevice@.
data AVExternalSyncDevice

instance IsObjCObject (Id AVExternalSyncDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExternalSyncDevice"

class IsNSObject a => IsAVExternalSyncDevice a where
  toAVExternalSyncDevice :: a -> Id AVExternalSyncDevice

instance IsAVExternalSyncDevice (Id AVExternalSyncDevice) where
  toAVExternalSyncDevice = unsafeCastId

instance IsNSObject (Id AVExternalSyncDevice) where
  toNSObject = unsafeCastId

-- ---------- AVExternalSyncDeviceDiscoverySession ----------

-- | A means of discovering and monitoring connection / disconnection of external sync devices to the host.
--
-- ``AVExternalSyncDeviceDiscoverySession`` is a singleton that lists the external sync devices connected to the host. The client is expected to key-value observe the ``AVExternalSyncDeviceDiscoverySession/devices`` property for changes to the external sync devices list.
-- 
-- Phantom type for @AVExternalSyncDeviceDiscoverySession@.
data AVExternalSyncDeviceDiscoverySession

instance IsObjCObject (Id AVExternalSyncDeviceDiscoverySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVExternalSyncDeviceDiscoverySession"

class IsNSObject a => IsAVExternalSyncDeviceDiscoverySession a where
  toAVExternalSyncDeviceDiscoverySession :: a -> Id AVExternalSyncDeviceDiscoverySession

instance IsAVExternalSyncDeviceDiscoverySession (Id AVExternalSyncDeviceDiscoverySession) where
  toAVExternalSyncDeviceDiscoverySession = unsafeCastId

instance IsNSObject (Id AVExternalSyncDeviceDiscoverySession) where
  toNSObject = unsafeCastId

-- ---------- AVFragmentedAssetMinder ----------

-- | A class that periodically checks whether additional fragments have been appended to fragmented assets.
-- 
-- Phantom type for @AVFragmentedAssetMinder@.
data AVFragmentedAssetMinder

instance IsObjCObject (Id AVFragmentedAssetMinder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFragmentedAssetMinder"

class IsNSObject a => IsAVFragmentedAssetMinder a where
  toAVFragmentedAssetMinder :: a -> Id AVFragmentedAssetMinder

instance IsAVFragmentedAssetMinder (Id AVFragmentedAssetMinder) where
  toAVFragmentedAssetMinder = unsafeCastId

instance IsNSObject (Id AVFragmentedAssetMinder) where
  toNSObject = unsafeCastId

-- ---------- AVFrameRateRange ----------

-- | AVFrameRateRange
--
-- An AVFrameRateRange expresses a range of valid frame rates as min and max rate and min and max duration.
--
-- An AVCaptureDevice exposes an array of formats, and its current activeFormat may be queried. The payload for the formats property is an array of AVCaptureDeviceFormat objects and the activeFormat property payload is an AVCaptureDeviceFormat. AVCaptureDeviceFormat wraps a CMFormatDescription and expresses a range of valid video frame rates as an NSArray of AVFrameRateRange objects. AVFrameRateRange expresses min and max frame rate as a rate in frames per second and duration (CMTime). An AVFrameRateRange object is immutable. Its values do not change for the life of the object.
-- 
-- Phantom type for @AVFrameRateRange@.
data AVFrameRateRange

instance IsObjCObject (Id AVFrameRateRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFrameRateRange"

class IsNSObject a => IsAVFrameRateRange a where
  toAVFrameRateRange :: a -> Id AVFrameRateRange

instance IsAVFrameRateRange (Id AVFrameRateRange) where
  toAVFrameRateRange = unsafeCastId

instance IsNSObject (Id AVFrameRateRange) where
  toNSObject = unsafeCastId

-- ---------- AVMediaDataStorage ----------

-- | AVMediaDataStorage
--
-- Media sample data storage file.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMediaDataStorage@.
data AVMediaDataStorage

instance IsObjCObject (Id AVMediaDataStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaDataStorage"

class IsNSObject a => IsAVMediaDataStorage a where
  toAVMediaDataStorage :: a -> Id AVMediaDataStorage

instance IsAVMediaDataStorage (Id AVMediaDataStorage) where
  toAVMediaDataStorage = unsafeCastId

instance IsNSObject (Id AVMediaDataStorage) where
  toNSObject = unsafeCastId

-- ---------- AVMediaExtensionProperties ----------

-- | A class incorporating properties for a MediaExtension
--
-- AVMediaExtensionProperties objects are returned from property queries on AVAsset, AVPlayerItemTrack, AVSampleBufferDisplayLayer, or AVSampleBufferVideoRenderer. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMediaExtensionProperties@.
data AVMediaExtensionProperties

instance IsObjCObject (Id AVMediaExtensionProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaExtensionProperties"

class IsNSObject a => IsAVMediaExtensionProperties a where
  toAVMediaExtensionProperties :: a -> Id AVMediaExtensionProperties

instance IsAVMediaExtensionProperties (Id AVMediaExtensionProperties) where
  toAVMediaExtensionProperties = unsafeCastId

instance IsNSObject (Id AVMediaExtensionProperties) where
  toNSObject = unsafeCastId

-- ---------- AVMediaPresentationSelector ----------

-- | For content that has been authored with the express intent of offering an alternative selection interface for AVMediaSelectionOptions, AVMediaPresentationSelector represents a collection of mutually exclusive settings.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMediaPresentationSelector@.
data AVMediaPresentationSelector

instance IsObjCObject (Id AVMediaPresentationSelector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaPresentationSelector"

class IsNSObject a => IsAVMediaPresentationSelector a where
  toAVMediaPresentationSelector :: a -> Id AVMediaPresentationSelector

instance IsAVMediaPresentationSelector (Id AVMediaPresentationSelector) where
  toAVMediaPresentationSelector = unsafeCastId

instance IsNSObject (Id AVMediaPresentationSelector) where
  toNSObject = unsafeCastId

-- ---------- AVMediaPresentationSetting ----------

-- | For content that has been authored with the express intent of offering an alternative selection interface for AVMediaSelectionOptions, AVMediaPresentationSetting represents a selectable setting for controlling the presentation of the media.
--
-- Each selectable setting is associated with a media characteristic that one or more of the AVMediaSelectionOptions in the AVMediaSelectionGroup possesses. By selecting a setting in a user interface that offers AVMediaPresentationSettings, users are essentially indicating a preference for the media characteristic of the selected setting. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMediaPresentationSetting@.
data AVMediaPresentationSetting

instance IsObjCObject (Id AVMediaPresentationSetting) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaPresentationSetting"

class IsNSObject a => IsAVMediaPresentationSetting a where
  toAVMediaPresentationSetting :: a -> Id AVMediaPresentationSetting

instance IsAVMediaPresentationSetting (Id AVMediaPresentationSetting) where
  toAVMediaPresentationSetting = unsafeCastId

instance IsNSObject (Id AVMediaPresentationSetting) where
  toNSObject = unsafeCastId

-- ---------- AVMediaSelection ----------

-- | Phantom type for @AVMediaSelection@.
data AVMediaSelection

instance IsObjCObject (Id AVMediaSelection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaSelection"

class IsNSObject a => IsAVMediaSelection a where
  toAVMediaSelection :: a -> Id AVMediaSelection

instance IsAVMediaSelection (Id AVMediaSelection) where
  toAVMediaSelection = unsafeCastId

instance IsNSObject (Id AVMediaSelection) where
  toNSObject = unsafeCastId

-- ---------- AVMediaSelectionGroup ----------

-- | AVMediaSelectionGroup provides a collection of mutually exclusive options for the presentation of media within an asset.
-- 
-- Phantom type for @AVMediaSelectionGroup@.
data AVMediaSelectionGroup

instance IsObjCObject (Id AVMediaSelectionGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaSelectionGroup"

class IsNSObject a => IsAVMediaSelectionGroup a where
  toAVMediaSelectionGroup :: a -> Id AVMediaSelectionGroup

instance IsAVMediaSelectionGroup (Id AVMediaSelectionGroup) where
  toAVMediaSelectionGroup = unsafeCastId

instance IsNSObject (Id AVMediaSelectionGroup) where
  toNSObject = unsafeCastId

-- ---------- AVMediaSelectionOption ----------

-- | AVMediaSelectionOption represents a specific option for the presentation of media within a group of options.
-- 
-- Phantom type for @AVMediaSelectionOption@.
data AVMediaSelectionOption

instance IsObjCObject (Id AVMediaSelectionOption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMediaSelectionOption"

class IsNSObject a => IsAVMediaSelectionOption a where
  toAVMediaSelectionOption :: a -> Id AVMediaSelectionOption

instance IsAVMediaSelectionOption (Id AVMediaSelectionOption) where
  toAVMediaSelectionOption = unsafeCastId

instance IsNSObject (Id AVMediaSelectionOption) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataGroup ----------

-- | AVMetadataGroup
--
-- AVMetadataGroup is the common superclass for AVTimedMetadataGroup and AVDateRangeMetadataGroup; each represents a collection of metadata items associated with a segment of a timeline. AVTimedMetadataGroup is typically used with content that defines an independent timeline, while AVDateRangeMetadataGroup is typically used with content that's associated with a specific range of dates.
-- 
-- Phantom type for @AVMetadataGroup@.
data AVMetadataGroup

instance IsObjCObject (Id AVMetadataGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataGroup"

class IsNSObject a => IsAVMetadataGroup a where
  toAVMetadataGroup :: a -> Id AVMetadataGroup

instance IsAVMetadataGroup (Id AVMetadataGroup) where
  toAVMetadataGroup = unsafeCastId

instance IsNSObject (Id AVMetadataGroup) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataItem ----------

-- | Phantom type for @AVMetadataItem@.
data AVMetadataItem

instance IsObjCObject (Id AVMetadataItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataItem"

class IsNSObject a => IsAVMetadataItem a where
  toAVMetadataItem :: a -> Id AVMetadataItem

instance IsAVMetadataItem (Id AVMetadataItem) where
  toAVMetadataItem = unsafeCastId

instance IsNSObject (Id AVMetadataItem) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataItemFilter ----------

-- | AVMetadataItemFilter
--
-- Filters selected information from a metadata item.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetadataItemFilter@.
data AVMetadataItemFilter

instance IsObjCObject (Id AVMetadataItemFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataItemFilter"

class IsNSObject a => IsAVMetadataItemFilter a where
  toAVMetadataItemFilter :: a -> Id AVMetadataItemFilter

instance IsAVMetadataItemFilter (Id AVMetadataItemFilter) where
  toAVMetadataItemFilter = unsafeCastId

instance IsNSObject (Id AVMetadataItemFilter) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataItemValueRequest ----------

-- | Phantom type for @AVMetadataItemValueRequest@.
data AVMetadataItemValueRequest

instance IsObjCObject (Id AVMetadataItemValueRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataItemValueRequest"

class IsNSObject a => IsAVMetadataItemValueRequest a where
  toAVMetadataItemValueRequest :: a -> Id AVMetadataItemValueRequest

instance IsAVMetadataItemValueRequest (Id AVMetadataItemValueRequest) where
  toAVMetadataItemValueRequest = unsafeCastId

instance IsNSObject (Id AVMetadataItemValueRequest) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataObject ----------

-- | AVMetadataObject
--
-- AVMetadataObject is an abstract class that defines an interface for a metadata object used by AVFoundation.
--
-- AVMetadataObject provides an abstract interface for metadata associated with a piece of media. One example is face metadata that might be detected in a picture. All metadata objects have a time, duration, bounds, and type.
--
-- The concrete AVMetadataFaceObject is used by AVCaptureMetadataOutput for face detection.
-- 
-- Phantom type for @AVMetadataObject@.
data AVMetadataObject

instance IsObjCObject (Id AVMetadataObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataObject"

class IsNSObject a => IsAVMetadataObject a where
  toAVMetadataObject :: a -> Id AVMetadataObject

instance IsAVMetadataObject (Id AVMetadataObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetricEvent ----------

-- | An abstract base class representing metric events.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricEvent@.
data AVMetricEvent

instance IsObjCObject (Id AVMetricEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricEvent"

class IsNSObject a => IsAVMetricEvent a where
  toAVMetricEvent :: a -> Id AVMetricEvent

instance IsAVMetricEvent (Id AVMetricEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricEventStream ----------

-- | AVMetricEventStream allows clients to add publishers and then subscribe to specific metric event classes from those publishers. Publishers are AVFoundation instances implementing AVMetricEventStreamPublisher. The interface allows clients to receive metric events via a subscriber delegate which implements AVMetricEventStreamSubscriber.
-- 
-- Phantom type for @AVMetricEventStream@.
data AVMetricEventStream

instance IsObjCObject (Id AVMetricEventStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricEventStream"

class IsNSObject a => IsAVMetricEventStream a where
  toAVMetricEventStream :: a -> Id AVMetricEventStream

instance IsAVMetricEventStream (Id AVMetricEventStream) where
  toAVMetricEventStream = unsafeCastId

instance IsNSObject (Id AVMetricEventStream) where
  toNSObject = unsafeCastId

-- ---------- AVMetricMediaRendition ----------

-- | Phantom type for @AVMetricMediaRendition@.
data AVMetricMediaRendition

instance IsObjCObject (Id AVMetricMediaRendition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricMediaRendition"

class IsNSObject a => IsAVMetricMediaRendition a where
  toAVMetricMediaRendition :: a -> Id AVMetricMediaRendition

instance IsAVMetricMediaRendition (Id AVMetricMediaRendition) where
  toAVMetricMediaRendition = unsafeCastId

instance IsNSObject (Id AVMetricMediaRendition) where
  toNSObject = unsafeCastId

-- ---------- AVOutputSettingsAssistant ----------

-- | AVOutputSettingsAssistant
--
-- A class, each instance of which specifies a set of parameters for configuring objects that use output settings dictionaries, for example AVAssetWriter & AVAssetWriterInput, so that the resulting media file conforms to some specific criteria
--
-- Instances of AVOutputSettingsAssistant are typically created using a string constant representing a specific preset configuration, such as AVOutputSettingsPreset1280x720.  Once you have an instance, its properties can be used as a guide for creating and configuring an AVAssetWriter object and one or more AVAssetWriterInput objects.  If all the suggested properties are respected, the resulting media file will conform to the criteria implied by the preset.  Alternatively, the properties of an instance can be used as a "base" configuration which can be customized to suit your individual needs.
--
-- The recommendations made by an instance get better as you tell it more about the format of your source data.  For example, if you set the sourceVideoFormat property, the recommendation made by the videoSettings property will ensure that your video frames are not scaled up from a smaller size.
-- 
-- Phantom type for @AVOutputSettingsAssistant@.
data AVOutputSettingsAssistant

instance IsObjCObject (Id AVOutputSettingsAssistant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVOutputSettingsAssistant"

class IsNSObject a => IsAVOutputSettingsAssistant a where
  toAVOutputSettingsAssistant :: a -> Id AVOutputSettingsAssistant

instance IsAVOutputSettingsAssistant (Id AVOutputSettingsAssistant) where
  toAVOutputSettingsAssistant = unsafeCastId

instance IsNSObject (Id AVOutputSettingsAssistant) where
  toNSObject = unsafeCastId

-- ---------- AVPlaybackCoordinationMedium ----------

-- | Phantom type for @AVPlaybackCoordinationMedium@.
data AVPlaybackCoordinationMedium

instance IsObjCObject (Id AVPlaybackCoordinationMedium) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlaybackCoordinationMedium"

class IsNSObject a => IsAVPlaybackCoordinationMedium a where
  toAVPlaybackCoordinationMedium :: a -> Id AVPlaybackCoordinationMedium

instance IsAVPlaybackCoordinationMedium (Id AVPlaybackCoordinationMedium) where
  toAVPlaybackCoordinationMedium = unsafeCastId

instance IsNSObject (Id AVPlaybackCoordinationMedium) where
  toNSObject = unsafeCastId

-- ---------- AVPlaybackCoordinator ----------

-- | The playback coordinator negotiates playback state between a player, such as AVPlayer or a custom playback object represented by an implementation of the AVPlaybackCoordinatorPlaybackControlDelegate protocol, and a group of other connected players.
--
-- AVPlaybackCoordinator will match rate and time of all connected players. This means that a local rate change or seek will be reflected in all connected players. Equally, a rate change or seek in any of the connected players will be reflected locally. AVPlaybackCoordinator does not manage the items in the play queue of the connected players, so it is up to player's owner to share and match the play queue across participants. The coordinator does, however, keep track of the identity of items enqueued in each player. This means that for one player's current time and rate to be applied on another player, both players must be playing the same item. If two players are playing different items, they each have independent playback states. When one of the two players transitions to the other's item later, it will match the time and rate of that other player.
-- 
-- Phantom type for @AVPlaybackCoordinator@.
data AVPlaybackCoordinator

instance IsObjCObject (Id AVPlaybackCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlaybackCoordinator"

class IsNSObject a => IsAVPlaybackCoordinator a where
  toAVPlaybackCoordinator :: a -> Id AVPlaybackCoordinator

instance IsAVPlaybackCoordinator (Id AVPlaybackCoordinator) where
  toAVPlaybackCoordinator = unsafeCastId

instance IsNSObject (Id AVPlaybackCoordinator) where
  toNSObject = unsafeCastId

-- ---------- AVPlayer ----------

-- | AVPlayer offers a playback interface for single-item playback that's sufficient for the implementation of playback controllers and playback user interfaces.
--
-- AVPlayer works equally well with local and remote media files, providing clients with appropriate information about readiness to play or about the need to await additional data before continuing.
--
-- Visual content of items played by an instance of AVPlayer can be displayed in a CoreAnimation layer of class AVPlayerLayer.
--
-- To allow clients to add and remove their objects as key-value observers safely, AVPlayer serializes notifications of changes that occur dynamically during playback on a dispatch queue. By default, this queue is the main queue. See dispatch_get_main_queue().
-- 
-- Phantom type for @AVPlayer@.
data AVPlayer

instance IsObjCObject (Id AVPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayer"

class IsNSObject a => IsAVPlayer a where
  toAVPlayer :: a -> Id AVPlayer

instance IsAVPlayer (Id AVPlayer) where
  toAVPlayer = unsafeCastId

instance IsNSObject (Id AVPlayer) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerInterstitialEvent ----------

-- | An AVPlayerInterstitialEvent provides instructions for temporarily suspending the playback of primary content in order to play alternative interstitial content instead, resuming playback of the primary content when playback of the interstitial content is complete or is canceled.
--
-- The primary content is specified as an instance of AVPlayerItem, designated as the primary item of the interstitial event.
--
-- The timing of interstitial playback is specified as a date within the date range of the primary item. Interstitial events are currently possible only for items with an intrinsic mapping from their timeline to real-time dates.
--
-- The alternative interstitial content is specified as an array of one or more AVPlayerItems that will be used as templates for the creation of items for interstitial playback. In other words, these template items are not the actual items that will be played during interstitial playback; instead they are used to generate the items that are to be played, with property values that match the configuration of your template items.
--
-- If you wish to observe the scheduling and progress of interstitial events, use an AVPlayerInterstitialEventMonitor. If you wish to specify your own schedule of interstitial events, use an AVPlayerInterstitialEventController.
--
-- Note that while previously AVPlayerInterstitialEvent was an immutable object, it is now mutable. This allows it to be created and customized before being set on an AVPlayerInterstitialEventController.
-- 
-- Phantom type for @AVPlayerInterstitialEvent@.
data AVPlayerInterstitialEvent

instance IsObjCObject (Id AVPlayerInterstitialEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerInterstitialEvent"

class IsNSObject a => IsAVPlayerInterstitialEvent a where
  toAVPlayerInterstitialEvent :: a -> Id AVPlayerInterstitialEvent

instance IsAVPlayerInterstitialEvent (Id AVPlayerInterstitialEvent) where
  toAVPlayerInterstitialEvent = unsafeCastId

instance IsNSObject (Id AVPlayerInterstitialEvent) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerInterstitialEventMonitor ----------

-- | An AVPlayerInterstitialEventMonitor allows you to observe the scheduling and progress of interstitial events, specified either intrinsically within the content of primary items, such as via use of directives carried by HLS media playlists, or via use of an AVPlayerInterstitialEventController.
--
-- The schedule of interstitial events is provided as an array of AVPlayerInterstitialEvents. For each AVPlayerInterstitialEvent, when the primary player's current item is the primary item of the interstitial event and its currentDate reaches the date of the event, playback of the primary item by the primary player is temporarily suspended, i.e. its timeControlStatus changes to AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate and its reasonForWaitingToPlay will change to AVPlayerWaitingDuringInterstitialEventReason. During this suspension, playback of items that replicate the interstitial template items of the event are played by the interstitial player, which temporarily assumes the output configuration of the primary player; for example, its visual content will be routed to AVPlayerLayers that reference the primary player. Once the interstitial player has advanced through playback of the interstitial items specified by the event or its current item otherwise becomes nil, playback of the primary content will resume, at an offset from the time at which it was suspended as specified by the event.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerInterstitialEventMonitor@.
data AVPlayerInterstitialEventMonitor

instance IsObjCObject (Id AVPlayerInterstitialEventMonitor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerInterstitialEventMonitor"

class IsNSObject a => IsAVPlayerInterstitialEventMonitor a where
  toAVPlayerInterstitialEventMonitor :: a -> Id AVPlayerInterstitialEventMonitor

instance IsAVPlayerInterstitialEventMonitor (Id AVPlayerInterstitialEventMonitor) where
  toAVPlayerInterstitialEventMonitor = unsafeCastId

instance IsNSObject (Id AVPlayerInterstitialEventMonitor) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItem ----------

-- | An AVPlayerItem carries a reference to an AVAsset as well as presentation settings for that asset.
--
-- Note that inspection of media assets is provided by AVAsset. This class is intended to represent presentation state for an asset that's played by an AVPlayer and to permit observation of that state.
--
-- It is important to avoid key-value observation with a key path containing the asset's property. Observe the AVPlayerItem's property instead. For example, use the "duration" key path instead of the "asset.duration" key path.
--
-- To allow clients to add and remove their objects as key-value observers safely, AVPlayerItem serializes notifications of changes that occur dynamically during playback on the same dispatch queue on which notifications of playback state changes are serialized by its associated AVPlayer. By default, this queue is the main queue. See dispatch_get_main_queue().
-- 
-- Phantom type for @AVPlayerItem@.
data AVPlayerItem

instance IsObjCObject (Id AVPlayerItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItem"

class IsNSObject a => IsAVPlayerItem a where
  toAVPlayerItem :: a -> Id AVPlayerItem

instance IsAVPlayerItem (Id AVPlayerItem) where
  toAVPlayerItem = unsafeCastId

instance IsNSObject (Id AVPlayerItem) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemAccessLog ----------

-- | An AVPlayerItemAccessLog provides methods to retrieve the access log in a format suitable for serialization.
--
-- An AVPlayerItemAccessLog acculumulates key metrics about network playback and presents them as a collection  of AVPlayerItemAccessLogEvent instances. Each AVPlayerItemAccessLogEvent instance collates the data  that relates to each uninterrupted period of playback.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemAccessLog@.
data AVPlayerItemAccessLog

instance IsObjCObject (Id AVPlayerItemAccessLog) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemAccessLog"

class IsNSObject a => IsAVPlayerItemAccessLog a where
  toAVPlayerItemAccessLog :: a -> Id AVPlayerItemAccessLog

instance IsAVPlayerItemAccessLog (Id AVPlayerItemAccessLog) where
  toAVPlayerItemAccessLog = unsafeCastId

instance IsNSObject (Id AVPlayerItemAccessLog) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemAccessLogEvent ----------

-- | An AVPlayerItemAccessLogEvent represents a single log entry.
--
-- An AVPlayerItemAccessLogEvent provides named properties for accessing the data fields of each log event. None of the properties of this class are observable.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemAccessLogEvent@.
data AVPlayerItemAccessLogEvent

instance IsObjCObject (Id AVPlayerItemAccessLogEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemAccessLogEvent"

class IsNSObject a => IsAVPlayerItemAccessLogEvent a where
  toAVPlayerItemAccessLogEvent :: a -> Id AVPlayerItemAccessLogEvent

instance IsAVPlayerItemAccessLogEvent (Id AVPlayerItemAccessLogEvent) where
  toAVPlayerItemAccessLogEvent = unsafeCastId

instance IsNSObject (Id AVPlayerItemAccessLogEvent) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemErrorLog ----------

-- | An AVPlayerItemErrorLog provides methods to retrieve the error log in a format suitable for serialization.
--
-- An AVPlayerItemErrorLog provides data to identify if, and when, network resource playback failures occured.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemErrorLog@.
data AVPlayerItemErrorLog

instance IsObjCObject (Id AVPlayerItemErrorLog) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemErrorLog"

class IsNSObject a => IsAVPlayerItemErrorLog a where
  toAVPlayerItemErrorLog :: a -> Id AVPlayerItemErrorLog

instance IsAVPlayerItemErrorLog (Id AVPlayerItemErrorLog) where
  toAVPlayerItemErrorLog = unsafeCastId

instance IsNSObject (Id AVPlayerItemErrorLog) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemErrorLogEvent ----------

-- | An AVPlayerItemErrorLogEvent represents a single log entry.
--
-- An AVPlayerItemErrorLogEvent provides named properties for accessing the data fields of each log event. None of the properties of this class are observable.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemErrorLogEvent@.
data AVPlayerItemErrorLogEvent

instance IsObjCObject (Id AVPlayerItemErrorLogEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemErrorLogEvent"

class IsNSObject a => IsAVPlayerItemErrorLogEvent a where
  toAVPlayerItemErrorLogEvent :: a -> Id AVPlayerItemErrorLogEvent

instance IsAVPlayerItemErrorLogEvent (Id AVPlayerItemErrorLogEvent) where
  toAVPlayerItemErrorLogEvent = unsafeCastId

instance IsNSObject (Id AVPlayerItemErrorLogEvent) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemIntegratedTimeline ----------

-- | AVPlayerItemIntegratedTimeline
--
-- An AVPlayerItemIntegratedTimeline provides detailed timing information and control for the sequence of playback of a primary AVPlayerItem and scheduled AVPlayerInterstitialEvents.
--
-- An object that models the timeline and sequence of playback of primary AVPlayerItem and scheduled AVPlayerInterstitialEvents. The timeline models all regions expected to be traversed during playback. Notably portions of the primary item may not be presented when exiting an interstitial event with a positive resumption offset.	Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemIntegratedTimeline@.
data AVPlayerItemIntegratedTimeline

instance IsObjCObject (Id AVPlayerItemIntegratedTimeline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemIntegratedTimeline"

class IsNSObject a => IsAVPlayerItemIntegratedTimeline a where
  toAVPlayerItemIntegratedTimeline :: a -> Id AVPlayerItemIntegratedTimeline

instance IsAVPlayerItemIntegratedTimeline (Id AVPlayerItemIntegratedTimeline) where
  toAVPlayerItemIntegratedTimeline = unsafeCastId

instance IsNSObject (Id AVPlayerItemIntegratedTimeline) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemIntegratedTimelineSnapshot ----------

-- | AVPlayerItemIntegratedTimelineSnapshot
--
-- AVPlayerItemIntegratedTimelineSnapshot provides an immutable representation of inspectable details from an AVPlayerItemIntegratedTimeline.
--
-- An instance of AVPlayerItemIntegratedTimelineSnapshot is an immutable snapshot representation of inspectable details from an AVPlayerItemIntegratedTimeline. As playback progresses,AVPlayerItemIntegratedTimelineSnapshot will not reflect the new timeline state. One can request a new snapshot instance from an AVPlayerItemIntegratedTimeline to reflect the latest timeline state.	Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemIntegratedTimelineSnapshot@.
data AVPlayerItemIntegratedTimelineSnapshot

instance IsObjCObject (Id AVPlayerItemIntegratedTimelineSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemIntegratedTimelineSnapshot"

class IsNSObject a => IsAVPlayerItemIntegratedTimelineSnapshot a where
  toAVPlayerItemIntegratedTimelineSnapshot :: a -> Id AVPlayerItemIntegratedTimelineSnapshot

instance IsAVPlayerItemIntegratedTimelineSnapshot (Id AVPlayerItemIntegratedTimelineSnapshot) where
  toAVPlayerItemIntegratedTimelineSnapshot = unsafeCastId

instance IsNSObject (Id AVPlayerItemIntegratedTimelineSnapshot) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemMediaDataCollector ----------

-- | AVPlayerItemMediaDataCollector
--
-- AVPlayerItemMediaDataCollector is an abstract class encapsulating the common API for all AVPlayerItemMediaDataCollector subclasses.
--
-- Instances of AVPlayerItemMediaDataCollector permit the collection of media data from an AVAsset during playback by an AVPlayer. As opposed to AVPlayerItemOutputs, AVPlayerItemMediaDataCollectors collect all media data across an AVPlayerItem's timebase, relevant to the specific collector being used. Attaching an AVPlayerItemMediaDataCollector may incur additional I/O accordingly.
--
-- You manage an association of an AVPlayerItemMediaDataCollector instance with an AVPlayerItem as the source input using the AVPlayerItem methods:
--
-- • addMediaDataCollector:		• removeMediaDataCollector:
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemMediaDataCollector@.
data AVPlayerItemMediaDataCollector

instance IsObjCObject (Id AVPlayerItemMediaDataCollector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemMediaDataCollector"

class IsNSObject a => IsAVPlayerItemMediaDataCollector a where
  toAVPlayerItemMediaDataCollector :: a -> Id AVPlayerItemMediaDataCollector

instance IsAVPlayerItemMediaDataCollector (Id AVPlayerItemMediaDataCollector) where
  toAVPlayerItemMediaDataCollector = unsafeCastId

instance IsNSObject (Id AVPlayerItemMediaDataCollector) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemOutput ----------

-- | Phantom type for @AVPlayerItemOutput@.
data AVPlayerItemOutput

instance IsObjCObject (Id AVPlayerItemOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemOutput"

class IsNSObject a => IsAVPlayerItemOutput a where
  toAVPlayerItemOutput :: a -> Id AVPlayerItemOutput

instance IsAVPlayerItemOutput (Id AVPlayerItemOutput) where
  toAVPlayerItemOutput = unsafeCastId

instance IsNSObject (Id AVPlayerItemOutput) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemSegment ----------

-- | AVPlayerItemSegment
--
-- Representing a segment of time on the integrated timeline. Segments are immutable objects.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemSegment@.
data AVPlayerItemSegment

instance IsObjCObject (Id AVPlayerItemSegment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemSegment"

class IsNSObject a => IsAVPlayerItemSegment a where
  toAVPlayerItemSegment :: a -> Id AVPlayerItemSegment

instance IsAVPlayerItemSegment (Id AVPlayerItemSegment) where
  toAVPlayerItemSegment = unsafeCastId

instance IsNSObject (Id AVPlayerItemSegment) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemTrack ----------

-- | AVPlayerItemTrack
--
-- An AVPlayerItemTrack carries a reference to an AVAssetTrack as well as presentation settings for that track.
--
-- Note that inspection of assets tracks is provided by AVAssetTrack.		This class is intended to represent presentation state for a track of an asset that's played by an AVPlayer and AVPlayerItem.
-- 
-- Phantom type for @AVPlayerItemTrack@.
data AVPlayerItemTrack

instance IsObjCObject (Id AVPlayerItemTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemTrack"

class IsNSObject a => IsAVPlayerItemTrack a where
  toAVPlayerItemTrack :: a -> Id AVPlayerItemTrack

instance IsAVPlayerItemTrack (Id AVPlayerItemTrack) where
  toAVPlayerItemTrack = unsafeCastId

instance IsNSObject (Id AVPlayerItemTrack) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerLooper ----------

-- | Phantom type for @AVPlayerLooper@.
data AVPlayerLooper

instance IsObjCObject (Id AVPlayerLooper) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerLooper"

class IsNSObject a => IsAVPlayerLooper a where
  toAVPlayerLooper :: a -> Id AVPlayerLooper

instance IsAVPlayerLooper (Id AVPlayerLooper) where
  toAVPlayerLooper = unsafeCastId

instance IsNSObject (Id AVPlayerLooper) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerMediaSelectionCriteria ----------

-- | AVPlayerMediaSelectionCriteria
--
-- The preferred languages and media characteristics for a player.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerMediaSelectionCriteria@.
data AVPlayerMediaSelectionCriteria

instance IsObjCObject (Id AVPlayerMediaSelectionCriteria) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerMediaSelectionCriteria"

class IsNSObject a => IsAVPlayerMediaSelectionCriteria a where
  toAVPlayerMediaSelectionCriteria :: a -> Id AVPlayerMediaSelectionCriteria

instance IsAVPlayerMediaSelectionCriteria (Id AVPlayerMediaSelectionCriteria) where
  toAVPlayerMediaSelectionCriteria = unsafeCastId

instance IsNSObject (Id AVPlayerMediaSelectionCriteria) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerVideoOutput ----------

-- | AVPlayerVideoOutput
--
-- AVPlayerVideoOutput offers a way to attach to an AVPlayer and receive video frames and video-related data vended through CMTaggedBufferGroups.
--
-- AVPlayerVideoOutput can be attached to an AVPlayer using AVPlayer's method addVideoOutput:				Note:  An AVPlayerVideoOutput can only be attached to a single player at a time, attempting to attach to multiple player will result in an exception being thrown.				Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerVideoOutput@.
data AVPlayerVideoOutput

instance IsObjCObject (Id AVPlayerVideoOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerVideoOutput"

class IsNSObject a => IsAVPlayerVideoOutput a where
  toAVPlayerVideoOutput :: a -> Id AVPlayerVideoOutput

instance IsAVPlayerVideoOutput (Id AVPlayerVideoOutput) where
  toAVPlayerVideoOutput = unsafeCastId

instance IsNSObject (Id AVPlayerVideoOutput) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerVideoOutputConfiguration ----------

-- | AVPlayerVideoOutputConfiguration
--
-- An AVPlayerVideoOutputConfiguration carries an identifier for the AVPlayerItem the configuration is associated with as well as presentation settings for that item.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerVideoOutputConfiguration@.
data AVPlayerVideoOutputConfiguration

instance IsObjCObject (Id AVPlayerVideoOutputConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerVideoOutputConfiguration"

class IsNSObject a => IsAVPlayerVideoOutputConfiguration a where
  toAVPlayerVideoOutputConfiguration :: a -> Id AVPlayerVideoOutputConfiguration

instance IsAVPlayerVideoOutputConfiguration (Id AVPlayerVideoOutputConfiguration) where
  toAVPlayerVideoOutputConfiguration = unsafeCastId

instance IsNSObject (Id AVPlayerVideoOutputConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AVPortraitEffectsMatte ----------

-- | AVPortraitEffectsMatte
--
-- An object wrapping a matting image used for high quality rendering of portrait style effects onto an image (i.e. shallow depth of field, stage lighting, etc).
--
-- The pixel data in the matting image is represented in CVPixelBuffers as kCVPixelFormatType_OneComponent8 ('L008'). It's stored in image files as an auxiliary image, accessible using CGImageSourceCopyAuxiliaryDataInfoAtIndex with the data type kCGImageAuxiliaryDataTypePortraitEffectsMatte (see <ImageIO/CGImageProperties.h>).
-- 
-- Phantom type for @AVPortraitEffectsMatte@.
data AVPortraitEffectsMatte

instance IsObjCObject (Id AVPortraitEffectsMatte) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPortraitEffectsMatte"

class IsNSObject a => IsAVPortraitEffectsMatte a where
  toAVPortraitEffectsMatte :: a -> Id AVPortraitEffectsMatte

instance IsAVPortraitEffectsMatte (Id AVPortraitEffectsMatte) where
  toAVPortraitEffectsMatte = unsafeCastId

instance IsNSObject (Id AVPortraitEffectsMatte) where
  toNSObject = unsafeCastId

-- ---------- AVRenderedCaptionImage ----------

-- | AVRenderedCaptionImage
--
-- AVRenderedCaptionImage is a wrapper class vended out to the client for reading a rendered caption image (CVPixelBuffer) and its associated position (in pixels). The position is relative to the videoDisplaySize (in pixels) provided by the client during the initialization of AVPlayerItemRenderedLegibleOutput, and accordinging to the upper-left-origin coordinate system (ULO). The CVPixelBuffer will be backed by an IOSurface enabling it to be converted to MTLTexture using CVMetalTextureCache.
--
-- Display scale is a property of the screen on which the client UI elements are displayed. This value defines the mapping between the logical coordinate space (measured in points) and the physical coordinate space (measured in pixels). Higher scale factors indicate that each point is represented by more than one pixel at render time. For example, if the display scale factor is 2.0 and the bounds of caption rectangle are 50 x 50 points, the size of the CVPixelBufferRef for the caption is 100 x 100 pixels. The client shall provide videoDisplaySize value in pixels only and the position value of the caption image shall also be returned in pixels only.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVRenderedCaptionImage@.
data AVRenderedCaptionImage

instance IsObjCObject (Id AVRenderedCaptionImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVRenderedCaptionImage"

class IsNSObject a => IsAVRenderedCaptionImage a where
  toAVRenderedCaptionImage :: a -> Id AVRenderedCaptionImage

instance IsAVRenderedCaptionImage (Id AVRenderedCaptionImage) where
  toAVRenderedCaptionImage = unsafeCastId

instance IsNSObject (Id AVRenderedCaptionImage) where
  toNSObject = unsafeCastId

-- ---------- AVRouteDetector ----------

-- | AVRouteDetector
--
-- AVRouteDetector detects the presence of media playback routes.
--
-- If route detection is enabled (it is disabled by default), AVRouteDetector reports whether or not multiple playback routes have been detected. If this is the case, AVKit's AVRoutePickerView can be used to allow users to pick from the set of available routes.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVRouteDetector@.
data AVRouteDetector

instance IsObjCObject (Id AVRouteDetector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVRouteDetector"

class IsNSObject a => IsAVRouteDetector a where
  toAVRouteDetector :: a -> Id AVRouteDetector

instance IsAVRouteDetector (Id AVRouteDetector) where
  toAVRouteDetector = unsafeCastId

instance IsNSObject (Id AVRouteDetector) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferAudioRenderer ----------

-- | Phantom type for @AVSampleBufferAudioRenderer@.
data AVSampleBufferAudioRenderer

instance IsObjCObject (Id AVSampleBufferAudioRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferAudioRenderer"

class IsNSObject a => IsAVSampleBufferAudioRenderer a where
  toAVSampleBufferAudioRenderer :: a -> Id AVSampleBufferAudioRenderer

instance IsAVSampleBufferAudioRenderer (Id AVSampleBufferAudioRenderer) where
  toAVSampleBufferAudioRenderer = unsafeCastId

instance IsNSObject (Id AVSampleBufferAudioRenderer) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferGenerator ----------

-- | Phantom type for @AVSampleBufferGenerator@.
data AVSampleBufferGenerator

instance IsObjCObject (Id AVSampleBufferGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferGenerator"

class IsNSObject a => IsAVSampleBufferGenerator a where
  toAVSampleBufferGenerator :: a -> Id AVSampleBufferGenerator

instance IsAVSampleBufferGenerator (Id AVSampleBufferGenerator) where
  toAVSampleBufferGenerator = unsafeCastId

instance IsNSObject (Id AVSampleBufferGenerator) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferGeneratorBatch ----------

-- | AVSampleBufferGeneratorBatch
--
-- An AVSampleBufferGeneratorBatch provides an optimized way to load sample data asynchronously for multiple CMSampleBuffers in an asset.
--
-- The AVSampleBufferGeneratorBatch loads sample data asynchronously, by aggregating adjacent I/O requests and overlapping them when possible for all CMSampleBuffers within a batch.		An AVSampleBufferGeneratorBatch is associated with an AVSampleBufferGenerator. See -[AVSampleBufferGenerator makeBatch] to create an AVSampleBufferGeneratorBatch.		See -[AVSampleBufferGeneratorBatch createSampleBufferForRequest: addingToBatch: error:] to create a CMSampleBuffer, defer I/O for its data, and build up a batch.		Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVSampleBufferGeneratorBatch@.
data AVSampleBufferGeneratorBatch

instance IsObjCObject (Id AVSampleBufferGeneratorBatch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferGeneratorBatch"

class IsNSObject a => IsAVSampleBufferGeneratorBatch a where
  toAVSampleBufferGeneratorBatch :: a -> Id AVSampleBufferGeneratorBatch

instance IsAVSampleBufferGeneratorBatch (Id AVSampleBufferGeneratorBatch) where
  toAVSampleBufferGeneratorBatch = unsafeCastId

instance IsNSObject (Id AVSampleBufferGeneratorBatch) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferRenderSynchronizer ----------

-- | AVSampleBufferRenderSynchronizer can synchronize multiple objects conforming to AVQueuedSampleBufferRendering to a single timebase.
-- 
-- Phantom type for @AVSampleBufferRenderSynchronizer@.
data AVSampleBufferRenderSynchronizer

instance IsObjCObject (Id AVSampleBufferRenderSynchronizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferRenderSynchronizer"

class IsNSObject a => IsAVSampleBufferRenderSynchronizer a where
  toAVSampleBufferRenderSynchronizer :: a -> Id AVSampleBufferRenderSynchronizer

instance IsAVSampleBufferRenderSynchronizer (Id AVSampleBufferRenderSynchronizer) where
  toAVSampleBufferRenderSynchronizer = unsafeCastId

instance IsNSObject (Id AVSampleBufferRenderSynchronizer) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferRequest ----------

-- | AVSampleBufferRequest
--
-- An AVSampleBufferRequest describes a CMSampleBuffer creation request.
-- 
-- Phantom type for @AVSampleBufferRequest@.
data AVSampleBufferRequest

instance IsObjCObject (Id AVSampleBufferRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferRequest"

class IsNSObject a => IsAVSampleBufferRequest a where
  toAVSampleBufferRequest :: a -> Id AVSampleBufferRequest

instance IsAVSampleBufferRequest (Id AVSampleBufferRequest) where
  toAVSampleBufferRequest = unsafeCastId

instance IsNSObject (Id AVSampleBufferRequest) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferVideoRenderer ----------

-- | Phantom type for @AVSampleBufferVideoRenderer@.
data AVSampleBufferVideoRenderer

instance IsObjCObject (Id AVSampleBufferVideoRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferVideoRenderer"

class IsNSObject a => IsAVSampleBufferVideoRenderer a where
  toAVSampleBufferVideoRenderer :: a -> Id AVSampleBufferVideoRenderer

instance IsAVSampleBufferVideoRenderer (Id AVSampleBufferVideoRenderer) where
  toAVSampleBufferVideoRenderer = unsafeCastId

instance IsNSObject (Id AVSampleBufferVideoRenderer) where
  toNSObject = unsafeCastId

-- ---------- AVSampleCursor ----------

-- | Phantom type for @AVSampleCursor@.
data AVSampleCursor

instance IsObjCObject (Id AVSampleCursor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleCursor"

class IsNSObject a => IsAVSampleCursor a where
  toAVSampleCursor :: a -> Id AVSampleCursor

instance IsAVSampleCursor (Id AVSampleCursor) where
  toAVSampleCursor = unsafeCastId

instance IsNSObject (Id AVSampleCursor) where
  toNSObject = unsafeCastId

-- ---------- AVSemanticSegmentationMatte ----------

-- | AVSemanticSegmentationMatte
--
-- An object wrapping a matting image for a particular semantic segmentation.
--
-- The pixel data in the matting image is represented in CVPixelBuffers as kCVPixelFormatType_OneComponent8 ('L008'). It is stored in image files as an auxiliary image, accessible using CGImageSourceCopyAuxiliaryDataInfoAtIndex using data types defined in <ImageIO/CGImageProperties.h>.
-- 
-- Phantom type for @AVSemanticSegmentationMatte@.
data AVSemanticSegmentationMatte

instance IsObjCObject (Id AVSemanticSegmentationMatte) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSemanticSegmentationMatte"

class IsNSObject a => IsAVSemanticSegmentationMatte a where
  toAVSemanticSegmentationMatte :: a -> Id AVSemanticSegmentationMatte

instance IsAVSemanticSegmentationMatte (Id AVSemanticSegmentationMatte) where
  toAVSemanticSegmentationMatte = unsafeCastId

instance IsNSObject (Id AVSemanticSegmentationMatte) where
  toNSObject = unsafeCastId

-- ---------- AVSpatialVideoConfiguration ----------

-- | An AVSpatialVideoConfiguration specifies spatial video properties.
-- 
-- Phantom type for @AVSpatialVideoConfiguration@.
data AVSpatialVideoConfiguration

instance IsObjCObject (Id AVSpatialVideoConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSpatialVideoConfiguration"

class IsNSObject a => IsAVSpatialVideoConfiguration a where
  toAVSpatialVideoConfiguration :: a -> Id AVSpatialVideoConfiguration

instance IsAVSpatialVideoConfiguration (Id AVSpatialVideoConfiguration) where
  toAVSpatialVideoConfiguration = unsafeCastId

instance IsNSObject (Id AVSpatialVideoConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AVTextStyleRule ----------

-- | Phantom type for @AVTextStyleRule@.
data AVTextStyleRule

instance IsObjCObject (Id AVTextStyleRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVTextStyleRule"

class IsNSObject a => IsAVTextStyleRule a where
  toAVTextStyleRule :: a -> Id AVTextStyleRule

instance IsAVTextStyleRule (Id AVTextStyleRule) where
  toAVTextStyleRule = unsafeCastId

instance IsNSObject (Id AVTextStyleRule) where
  toNSObject = unsafeCastId

-- ---------- AVVideoComposition ----------

-- | An AVVideoComposition object represents an immutable video composition.
--
-- A video composition describes, for any time in the aggregate time range of its instructions, the number and IDs of video tracks that are to be used in order to produce a composed video frame corresponding to that time. When AVFoundation's built-in video compositor is used, the instructions an AVVideoComposition contain can specify a spatial transformation, an opacity value, and a cropping rectangle for each video source, and these can vary over time via simple linear ramping functions.
--
-- A client can implement their own custom video compositor by implementing the AVVideoCompositing protocol; a custom video compositor is provided with pixel buffers for each of its video sources during playback and other operations and can perform arbitrary graphical operations on them in order to produce visual output.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoComposition@.
data AVVideoComposition

instance IsObjCObject (Id AVVideoComposition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoComposition"

class IsNSObject a => IsAVVideoComposition a where
  toAVVideoComposition :: a -> Id AVVideoComposition

instance IsAVVideoComposition (Id AVVideoComposition) where
  toAVVideoComposition = unsafeCastId

instance IsNSObject (Id AVVideoComposition) where
  toNSObject = unsafeCastId

-- ---------- AVVideoCompositionCoreAnimationTool ----------

-- | A tool for using Core Animation in a video composition.
--
-- Instances of AVVideoCompositionCoreAnimationTool are for use with offline rendering (AVAssetExportSession and AVAssetReader), not with AVPlayer. To synchronize real-time playback with other CoreAnimation layers, use AVSynchronizedLayer.
--
-- Any animations will be interpreted on the video's timeline, not real-time, so (a) set animation beginTimes to small positive value such as AVCoreAnimationBeginTimeAtZero rather than 0, because CoreAnimation will replace a value of 0 with CACurrentMediaTime();  (b) set removedOnCompletion to NO on animations so they are not automatically removed; (c) do not use layers associated with UIViews.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoCompositionCoreAnimationTool@.
data AVVideoCompositionCoreAnimationTool

instance IsObjCObject (Id AVVideoCompositionCoreAnimationTool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoCompositionCoreAnimationTool"

class IsNSObject a => IsAVVideoCompositionCoreAnimationTool a where
  toAVVideoCompositionCoreAnimationTool :: a -> Id AVVideoCompositionCoreAnimationTool

instance IsAVVideoCompositionCoreAnimationTool (Id AVVideoCompositionCoreAnimationTool) where
  toAVVideoCompositionCoreAnimationTool = unsafeCastId

instance IsNSObject (Id AVVideoCompositionCoreAnimationTool) where
  toNSObject = unsafeCastId

-- ---------- AVVideoCompositionInstruction ----------

-- | An AVVideoCompositionInstruction object represents an operation to be performed by a compositor.
--
-- An AVVideoComposition object maintains an array of instructions to perform its composition. This class is not intended to be subclassed; instead, conform to AVVideoCompositionInstructionProtocol ("AVVideoCompositionInstruction" in Objective-C). Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoCompositionInstruction@.
data AVVideoCompositionInstruction

instance IsObjCObject (Id AVVideoCompositionInstruction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoCompositionInstruction"

class IsNSObject a => IsAVVideoCompositionInstruction a where
  toAVVideoCompositionInstruction :: a -> Id AVVideoCompositionInstruction

instance IsAVVideoCompositionInstruction (Id AVVideoCompositionInstruction) where
  toAVVideoCompositionInstruction = unsafeCastId

instance IsNSObject (Id AVVideoCompositionInstruction) where
  toNSObject = unsafeCastId

-- ---------- AVVideoCompositionLayerInstruction ----------

-- | An AVVideoCompositionLayerInstruction object represents the transform, opacity, and cropping ramps to apply to a given track. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoCompositionLayerInstruction@.
data AVVideoCompositionLayerInstruction

instance IsObjCObject (Id AVVideoCompositionLayerInstruction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoCompositionLayerInstruction"

class IsNSObject a => IsAVVideoCompositionLayerInstruction a where
  toAVVideoCompositionLayerInstruction :: a -> Id AVVideoCompositionLayerInstruction

instance IsAVVideoCompositionLayerInstruction (Id AVVideoCompositionLayerInstruction) where
  toAVVideoCompositionLayerInstruction = unsafeCastId

instance IsNSObject (Id AVVideoCompositionLayerInstruction) where
  toNSObject = unsafeCastId

-- ---------- AVVideoCompositionRenderContext ----------

-- | The AVVideoCompositionRenderContext class defines the context within which custom compositors render new output pixels buffers.
--
-- An instance of AVVideoCompositionRenderContext provides size and scaling information and offers a service for efficiently providing pixel buffers from a managed pool of buffers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoCompositionRenderContext@.
data AVVideoCompositionRenderContext

instance IsObjCObject (Id AVVideoCompositionRenderContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoCompositionRenderContext"

class IsNSObject a => IsAVVideoCompositionRenderContext a where
  toAVVideoCompositionRenderContext :: a -> Id AVVideoCompositionRenderContext

instance IsAVVideoCompositionRenderContext (Id AVVideoCompositionRenderContext) where
  toAVVideoCompositionRenderContext = unsafeCastId

instance IsNSObject (Id AVVideoCompositionRenderContext) where
  toNSObject = unsafeCastId

-- ---------- AVVideoCompositionRenderHint ----------

-- | An AVVideoCompositionRenderHint instance contains the information necessary for announcing upcoming rendering request time ranges.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoCompositionRenderHint@.
data AVVideoCompositionRenderHint

instance IsObjCObject (Id AVVideoCompositionRenderHint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoCompositionRenderHint"

class IsNSObject a => IsAVVideoCompositionRenderHint a where
  toAVVideoCompositionRenderHint :: a -> Id AVVideoCompositionRenderHint

instance IsAVVideoCompositionRenderHint (Id AVVideoCompositionRenderHint) where
  toAVVideoCompositionRenderHint = unsafeCastId

instance IsNSObject (Id AVVideoCompositionRenderHint) where
  toNSObject = unsafeCastId

-- ---------- AVVideoOutputSpecification ----------

-- | AVVideoOutputSpecification
--
-- AVVideoOutputSpecification offers a way to package CMTagCollections together with output settings. Allowing for direct association between output settings and specific tag collections, as well as default output settings which can be associated with all tag collections which do not have a specified mapping.
--
-- For more information about working with CMTagCollections and CMTags first look at <CoreMedia/CMTagCollection.h>
-- 
-- Phantom type for @AVVideoOutputSpecification@.
data AVVideoOutputSpecification

instance IsObjCObject (Id AVVideoOutputSpecification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoOutputSpecification"

class IsNSObject a => IsAVVideoOutputSpecification a where
  toAVVideoOutputSpecification :: a -> Id AVVideoOutputSpecification

instance IsAVVideoOutputSpecification (Id AVVideoOutputSpecification) where
  toAVVideoOutputSpecification = unsafeCastId

instance IsNSObject (Id AVVideoOutputSpecification) where
  toNSObject = unsafeCastId

-- ---------- AVVideoPerformanceMetrics ----------

-- | AVVideoPerformanceMetrics
--
-- [SPI] An instance of AVVideoPerformanceMetrics provides current performance metrics.
--
-- An instance of AVVideoPerformanceMetrics provides named properties for accessing the video playback quality metrics.				Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVVideoPerformanceMetrics@.
data AVVideoPerformanceMetrics

instance IsObjCObject (Id AVVideoPerformanceMetrics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVVideoPerformanceMetrics"

class IsNSObject a => IsAVVideoPerformanceMetrics a where
  toAVVideoPerformanceMetrics :: a -> Id AVVideoPerformanceMetrics

instance IsAVVideoPerformanceMetrics (Id AVVideoPerformanceMetrics) where
  toAVVideoPerformanceMetrics = unsafeCastId

instance IsNSObject (Id AVVideoPerformanceMetrics) where
  toNSObject = unsafeCastId

-- ---------- AVZoomRange ----------

-- | AVZoomRange
--
-- An AVZoomRange expresses an inclusive range of supported zoom factors.
--
-- This is used by features that have requirements on zoom factors falling within certain ranges.
-- 
-- Phantom type for @AVZoomRange@.
data AVZoomRange

instance IsObjCObject (Id AVZoomRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVZoomRange"

class IsNSObject a => IsAVZoomRange a where
  toAVZoomRange :: a -> Id AVZoomRange

instance IsAVZoomRange (Id AVZoomRange) where
  toAVZoomRange = unsafeCastId

instance IsNSObject (Id AVZoomRange) where
  toNSObject = unsafeCastId

-- ---------- AVComposition ----------

-- | Phantom type for @AVComposition@.
data AVComposition

instance IsObjCObject (Id AVComposition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVComposition"

class IsAVAsset a => IsAVComposition a where
  toAVComposition :: a -> Id AVComposition

instance IsAVComposition (Id AVComposition) where
  toAVComposition = unsafeCastId

instance IsAVAsset (Id AVComposition) where
  toAVAsset = unsafeCastId

instance IsNSObject (Id AVComposition) where
  toNSObject = unsafeCastId

-- ---------- AVMovie ----------

-- | Phantom type for @AVMovie@.
data AVMovie

instance IsObjCObject (Id AVMovie) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMovie"

class IsAVAsset a => IsAVMovie a where
  toAVMovie :: a -> Id AVMovie

instance IsAVMovie (Id AVMovie) where
  toAVMovie = unsafeCastId

instance IsAVAsset (Id AVMovie) where
  toAVAsset = unsafeCastId

instance IsNSObject (Id AVMovie) where
  toNSObject = unsafeCastId

-- ---------- AVURLAsset ----------

-- | AVURLAsset provides access to the AVAsset model for timed audiovisual media referenced by URL.
--
-- Note that although instances of AVURLAsset are immutable, values for its keys may not be immediately available without blocking. See the discussion of the class AVAsset above regarding the availability of values for keys and the use of AVAsynchronousKeyValueLoading.
--
-- Once an AVURLAsset's value for a key is available, it will not change. AVPlayerItem provides access to information that can change dynamically during playback; see AVPlayerItem.duration and AVPlayerItem.tracks.
--
-- AVURLAssets can be initialized with NSURLs that refer to audiovisual media resources, such as streams (including HTTP live streams), QuickTime movie files, MP3 files, and files of other types.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVURLAsset@.
data AVURLAsset

instance IsObjCObject (Id AVURLAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVURLAsset"

class IsAVAsset a => IsAVURLAsset a where
  toAVURLAsset :: a -> Id AVURLAsset

instance IsAVURLAsset (Id AVURLAsset) where
  toAVURLAsset = unsafeCastId

instance IsAVAsset (Id AVURLAsset) where
  toAVAsset = unsafeCastId

instance IsNSObject (Id AVURLAsset) where
  toNSObject = unsafeCastId

-- ---------- AVMutableAssetDownloadStorageManagementPolicy ----------

-- | A mutable subclass of AVAssetDownloadStorageManagementPolicy.
--
-- System will put in best-effort to evict all the assets based on expirationDate before evicting based on priority.
-- 
-- Phantom type for @AVMutableAssetDownloadStorageManagementPolicy@.
data AVMutableAssetDownloadStorageManagementPolicy

instance IsObjCObject (Id AVMutableAssetDownloadStorageManagementPolicy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableAssetDownloadStorageManagementPolicy"

class IsAVAssetDownloadStorageManagementPolicy a => IsAVMutableAssetDownloadStorageManagementPolicy a where
  toAVMutableAssetDownloadStorageManagementPolicy :: a -> Id AVMutableAssetDownloadStorageManagementPolicy

instance IsAVMutableAssetDownloadStorageManagementPolicy (Id AVMutableAssetDownloadStorageManagementPolicy) where
  toAVMutableAssetDownloadStorageManagementPolicy = unsafeCastId

instance IsAVAssetDownloadStorageManagementPolicy (Id AVMutableAssetDownloadStorageManagementPolicy) where
  toAVAssetDownloadStorageManagementPolicy = unsafeCastId

instance IsNSObject (Id AVMutableAssetDownloadStorageManagementPolicy) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderAudioMixOutput ----------

-- | AVAssetReaderAudioMixOutput
--
-- AVAssetReaderAudioMixOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading audio samples that result from mixing the audio from one or more AVAssetTracks of an AVAssetReader's AVAsset.
--
-- Clients can read the audio data mixed from one or more asset tracks by adding an instance of AVAssetReaderAudioMixOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method.
-- 
-- Phantom type for @AVAssetReaderAudioMixOutput@.
data AVAssetReaderAudioMixOutput

instance IsObjCObject (Id AVAssetReaderAudioMixOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderAudioMixOutput"

class IsAVAssetReaderOutput a => IsAVAssetReaderAudioMixOutput a where
  toAVAssetReaderAudioMixOutput :: a -> Id AVAssetReaderAudioMixOutput

instance IsAVAssetReaderAudioMixOutput (Id AVAssetReaderAudioMixOutput) where
  toAVAssetReaderAudioMixOutput = unsafeCastId

instance IsAVAssetReaderOutput (Id AVAssetReaderAudioMixOutput) where
  toAVAssetReaderOutput = unsafeCastId

instance IsNSObject (Id AVAssetReaderAudioMixOutput) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderSampleReferenceOutput ----------

-- | AVAssetReaderSampleReferenceOutput
--
-- AVAssetReaderSampleReferenceOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading sample references from a single AVAssetTrack of an AVAssetReader's AVAsset.
--
-- Clients can extract information about the location (file URL and offset) of samples in a track by adding an instance of AVAssetReaderSampleReferenceOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method. No actual sample data can be extracted using this class. The location of the sample data is described by the kCMSampleBufferAttachmentKey_SampleReferenceURL and kCMSampleBufferAttachmentKey_SampleReferenceByteOffset attachments on the extracted sample buffers. More information about sample buffers describing sample references can be found in the CMSampleBuffer documentation.
--
-- Sample buffers extracted using this class can also be appended to an AVAssetWriterInput to create movie tracks that are not self-contained and reference data in the original file instead.  Currently, only instances of AVAssetWriter configured to write files of type AVFileTypeQuickTimeMovie can be used to write tracks that are not self-contained.
--
-- Since no sample data is ever returned by instances of AVAssetReaderSampleReferenceOutput, the value of the alwaysCopiesSampleData property is ignored.
-- 
-- Phantom type for @AVAssetReaderSampleReferenceOutput@.
data AVAssetReaderSampleReferenceOutput

instance IsObjCObject (Id AVAssetReaderSampleReferenceOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderSampleReferenceOutput"

class IsAVAssetReaderOutput a => IsAVAssetReaderSampleReferenceOutput a where
  toAVAssetReaderSampleReferenceOutput :: a -> Id AVAssetReaderSampleReferenceOutput

instance IsAVAssetReaderSampleReferenceOutput (Id AVAssetReaderSampleReferenceOutput) where
  toAVAssetReaderSampleReferenceOutput = unsafeCastId

instance IsAVAssetReaderOutput (Id AVAssetReaderSampleReferenceOutput) where
  toAVAssetReaderOutput = unsafeCastId

instance IsNSObject (Id AVAssetReaderSampleReferenceOutput) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderTrackOutput ----------

-- | AVAssetReaderTrackOutput
--
-- AVAssetReaderTrackOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading media data from a single AVAssetTrack of an AVAssetReader's AVAsset.
--
-- Clients can read the media data of an asset track by adding an instance of AVAssetReaderTrackOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method. The track's media samples can either be read in the format in which they are stored in the asset, or they can be converted to a different format.
-- 
-- Phantom type for @AVAssetReaderTrackOutput@.
data AVAssetReaderTrackOutput

instance IsObjCObject (Id AVAssetReaderTrackOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderTrackOutput"

class IsAVAssetReaderOutput a => IsAVAssetReaderTrackOutput a where
  toAVAssetReaderTrackOutput :: a -> Id AVAssetReaderTrackOutput

instance IsAVAssetReaderTrackOutput (Id AVAssetReaderTrackOutput) where
  toAVAssetReaderTrackOutput = unsafeCastId

instance IsAVAssetReaderOutput (Id AVAssetReaderTrackOutput) where
  toAVAssetReaderOutput = unsafeCastId

instance IsNSObject (Id AVAssetReaderTrackOutput) where
  toNSObject = unsafeCastId

-- ---------- AVAssetReaderVideoCompositionOutput ----------

-- | AVAssetReaderVideoCompositionOutput
--
-- AVAssetReaderVideoCompositionOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading video frames that have been composited together from the frames in one or more AVAssetTracks of an AVAssetReader's AVAsset.
--
-- Clients can read the video frames composited from one or more asset tracks by adding an instance of AVAssetReaderVideoCompositionOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method.
-- 
-- Phantom type for @AVAssetReaderVideoCompositionOutput@.
data AVAssetReaderVideoCompositionOutput

instance IsObjCObject (Id AVAssetReaderVideoCompositionOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetReaderVideoCompositionOutput"

class IsAVAssetReaderOutput a => IsAVAssetReaderVideoCompositionOutput a where
  toAVAssetReaderVideoCompositionOutput :: a -> Id AVAssetReaderVideoCompositionOutput

instance IsAVAssetReaderVideoCompositionOutput (Id AVAssetReaderVideoCompositionOutput) where
  toAVAssetReaderVideoCompositionOutput = unsafeCastId

instance IsAVAssetReaderOutput (Id AVAssetReaderVideoCompositionOutput) where
  toAVAssetReaderOutput = unsafeCastId

instance IsNSObject (Id AVAssetReaderVideoCompositionOutput) where
  toNSObject = unsafeCastId

-- ---------- AVAssetResourceRenewalRequest ----------

-- | AVAssetResourceRenewalRequest
--
-- AVAssetResourceRenewalRequest encapsulates information about a resource request issued by a resource loader for the purpose of renewing a request previously issued.
--
-- When an AVURLAsset needs to renew a resource (because contentInformationRequest.renewalDate has been set on a previous loading request), it asks its AVAssetResourceLoader object to assist. The resource loader encapsulates the request information by creating an instance of this object, which it then hands to its delegate for processing. The delegate uses the information in this object to perform the request and report on the success or failure of the operation.
-- 
-- Phantom type for @AVAssetResourceRenewalRequest@.
data AVAssetResourceRenewalRequest

instance IsObjCObject (Id AVAssetResourceRenewalRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetResourceRenewalRequest"

class IsAVAssetResourceLoadingRequest a => IsAVAssetResourceRenewalRequest a where
  toAVAssetResourceRenewalRequest :: a -> Id AVAssetResourceRenewalRequest

instance IsAVAssetResourceRenewalRequest (Id AVAssetResourceRenewalRequest) where
  toAVAssetResourceRenewalRequest = unsafeCastId

instance IsAVAssetResourceLoadingRequest (Id AVAssetResourceRenewalRequest) where
  toAVAssetResourceLoadingRequest = unsafeCastId

instance IsNSObject (Id AVAssetResourceRenewalRequest) where
  toNSObject = unsafeCastId

-- ---------- AVCompositionTrack ----------

-- | Phantom type for @AVCompositionTrack@.
data AVCompositionTrack

instance IsObjCObject (Id AVCompositionTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCompositionTrack"

class IsAVAssetTrack a => IsAVCompositionTrack a where
  toAVCompositionTrack :: a -> Id AVCompositionTrack

instance IsAVCompositionTrack (Id AVCompositionTrack) where
  toAVCompositionTrack = unsafeCastId

instance IsAVAssetTrack (Id AVCompositionTrack) where
  toAVAssetTrack = unsafeCastId

instance IsNSObject (Id AVCompositionTrack) where
  toNSObject = unsafeCastId

-- ---------- AVFragmentedAssetTrack ----------

-- | A subclass of AVAssetTrack for handling tracks of fragmented assets. An AVFragmentedAssetTrack is capable of changing the values of certain of its properties, if its parent asset is associated with an instance of AVFragmentedAssetMinder when one or more fragments are appended to the underlying media resource.
--
-- While its parent asset is associated with an AVFragmentedAssetMinder, AVFragmentedAssetTrack posts AVAssetTrackTimeRangeDidChangeNotification and AVAssetTrackSegmentsDidChangeNotification whenever new fragments are detected, as appropriate. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVFragmentedAssetTrack@.
data AVFragmentedAssetTrack

instance IsObjCObject (Id AVFragmentedAssetTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFragmentedAssetTrack"

class IsAVAssetTrack a => IsAVFragmentedAssetTrack a where
  toAVFragmentedAssetTrack :: a -> Id AVFragmentedAssetTrack

instance IsAVFragmentedAssetTrack (Id AVFragmentedAssetTrack) where
  toAVFragmentedAssetTrack = unsafeCastId

instance IsAVAssetTrack (Id AVFragmentedAssetTrack) where
  toAVAssetTrack = unsafeCastId

instance IsNSObject (Id AVFragmentedAssetTrack) where
  toNSObject = unsafeCastId

-- ---------- AVMovieTrack ----------

-- | Phantom type for @AVMovieTrack@.
data AVMovieTrack

instance IsObjCObject (Id AVMovieTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMovieTrack"

class IsAVAssetTrack a => IsAVMovieTrack a where
  toAVMovieTrack :: a -> Id AVMovieTrack

instance IsAVMovieTrack (Id AVMovieTrack) where
  toAVMovieTrack = unsafeCastId

instance IsAVAssetTrack (Id AVMovieTrack) where
  toAVAssetTrack = unsafeCastId

instance IsNSObject (Id AVMovieTrack) where
  toNSObject = unsafeCastId

-- ---------- AVCompositionTrackSegment ----------

-- | Phantom type for @AVCompositionTrackSegment@.
data AVCompositionTrackSegment

instance IsObjCObject (Id AVCompositionTrackSegment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCompositionTrackSegment"

class IsAVAssetTrackSegment a => IsAVCompositionTrackSegment a where
  toAVCompositionTrackSegment :: a -> Id AVCompositionTrackSegment

instance IsAVCompositionTrackSegment (Id AVCompositionTrackSegment) where
  toAVCompositionTrackSegment = unsafeCastId

instance IsAVAssetTrackSegment (Id AVCompositionTrackSegment) where
  toAVAssetTrackSegment = unsafeCastId

instance IsNSObject (Id AVCompositionTrackSegment) where
  toNSObject = unsafeCastId

-- ---------- AVMutableAudioMix ----------

-- | Phantom type for @AVMutableAudioMix@.
data AVMutableAudioMix

instance IsObjCObject (Id AVMutableAudioMix) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableAudioMix"

class IsAVAudioMix a => IsAVMutableAudioMix a where
  toAVMutableAudioMix :: a -> Id AVMutableAudioMix

instance IsAVMutableAudioMix (Id AVMutableAudioMix) where
  toAVMutableAudioMix = unsafeCastId

instance IsAVAudioMix (Id AVMutableAudioMix) where
  toAVAudioMix = unsafeCastId

instance IsNSObject (Id AVMutableAudioMix) where
  toNSObject = unsafeCastId

-- ---------- AVMutableAudioMixInputParameters ----------

-- | Phantom type for @AVMutableAudioMixInputParameters@.
data AVMutableAudioMixInputParameters

instance IsObjCObject (Id AVMutableAudioMixInputParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableAudioMixInputParameters"

class IsAVAudioMixInputParameters a => IsAVMutableAudioMixInputParameters a where
  toAVMutableAudioMixInputParameters :: a -> Id AVMutableAudioMixInputParameters

instance IsAVMutableAudioMixInputParameters (Id AVMutableAudioMixInputParameters) where
  toAVMutableAudioMixInputParameters = unsafeCastId

instance IsAVAudioMixInputParameters (Id AVMutableAudioMixInputParameters) where
  toAVAudioMixInputParameters = unsafeCastId

instance IsNSObject (Id AVMutableAudioMixInputParameters) where
  toNSObject = unsafeCastId

-- ---------- AVMutableCaption ----------

-- | AVMutableCaption
--
-- Mutable subclass of AVCaption.
-- 
-- Phantom type for @AVMutableCaption@.
data AVMutableCaption

instance IsObjCObject (Id AVMutableCaption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableCaption"

class IsAVCaption a => IsAVMutableCaption a where
  toAVMutableCaption :: a -> Id AVMutableCaption

instance IsAVMutableCaption (Id AVMutableCaption) where
  toAVMutableCaption = unsafeCastId

instance IsAVCaption (Id AVMutableCaption) where
  toAVCaption = unsafeCastId

instance IsNSObject (Id AVMutableCaption) where
  toNSObject = unsafeCastId

-- ---------- AVCaptionConversionTimeRangeAdjustment ----------

-- | AVCaptionConversionTimeRangeAdjustment
--
-- Describes an adjustment to the timeRange of one or more captions.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVCaptionConversionTimeRangeAdjustment@.
data AVCaptionConversionTimeRangeAdjustment

instance IsObjCObject (Id AVCaptionConversionTimeRangeAdjustment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptionConversionTimeRangeAdjustment"

class IsAVCaptionConversionAdjustment a => IsAVCaptionConversionTimeRangeAdjustment a where
  toAVCaptionConversionTimeRangeAdjustment :: a -> Id AVCaptionConversionTimeRangeAdjustment

instance IsAVCaptionConversionTimeRangeAdjustment (Id AVCaptionConversionTimeRangeAdjustment) where
  toAVCaptionConversionTimeRangeAdjustment = unsafeCastId

instance IsAVCaptionConversionAdjustment (Id AVCaptionConversionTimeRangeAdjustment) where
  toAVCaptionConversionAdjustment = unsafeCastId

instance IsNSObject (Id AVCaptionConversionTimeRangeAdjustment) where
  toNSObject = unsafeCastId

-- ---------- AVMutableCaptionRegion ----------

-- | AVMutableCaptionRegion
--
-- Mutable subclass of AVCaptionRegion.
-- 
-- Phantom type for @AVMutableCaptionRegion@.
data AVMutableCaptionRegion

instance IsObjCObject (Id AVMutableCaptionRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableCaptionRegion"

class IsAVCaptionRegion a => IsAVMutableCaptionRegion a where
  toAVMutableCaptionRegion :: a -> Id AVMutableCaptionRegion

instance IsAVMutableCaptionRegion (Id AVMutableCaptionRegion) where
  toAVMutableCaptionRegion = unsafeCastId

instance IsAVCaptionRegion (Id AVMutableCaptionRegion) where
  toAVCaptionRegion = unsafeCastId

instance IsNSObject (Id AVMutableCaptionRegion) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureAutoExposureBracketedStillImageSettings ----------

-- | AVCaptureAutoExposureBracketedStillImageSettings
--
-- AVCaptureAutoExposureBracketedStillImageSettings is a concrete subclass of AVCaptureBracketedStillImageSettings to be used when bracketing exposure target bias.
--
-- An AVCaptureAutoExposureBracketedStillImageSettings instance defines the exposure target bias setting that should be applied to one image in a bracket. An array of settings objects is passed to -[AVCaptureStillImageOutput captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:]. Min and max exposure target bias are queryable properties of the AVCaptureDevice supplying data to an AVCaptureStillImageOutput instance. If you wish to leave exposureTargetBias unchanged for this bracketed still image, you may pass the special value AVCaptureExposureTargetBiasCurrent (see AVCaptureDevice.h).
-- 
-- Phantom type for @AVCaptureAutoExposureBracketedStillImageSettings@.
data AVCaptureAutoExposureBracketedStillImageSettings

instance IsObjCObject (Id AVCaptureAutoExposureBracketedStillImageSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureAutoExposureBracketedStillImageSettings"

class IsAVCaptureBracketedStillImageSettings a => IsAVCaptureAutoExposureBracketedStillImageSettings a where
  toAVCaptureAutoExposureBracketedStillImageSettings :: a -> Id AVCaptureAutoExposureBracketedStillImageSettings

instance IsAVCaptureAutoExposureBracketedStillImageSettings (Id AVCaptureAutoExposureBracketedStillImageSettings) where
  toAVCaptureAutoExposureBracketedStillImageSettings = unsafeCastId

instance IsAVCaptureBracketedStillImageSettings (Id AVCaptureAutoExposureBracketedStillImageSettings) where
  toAVCaptureBracketedStillImageSettings = unsafeCastId

instance IsNSObject (Id AVCaptureAutoExposureBracketedStillImageSettings) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureManualExposureBracketedStillImageSettings ----------

-- | AVCaptureManualExposureBracketedStillImageSettings
--
-- AVCaptureManualExposureBracketedStillImageSettings is a concrete subclass of AVCaptureBracketedStillImageSettings to be used when bracketing exposure duration and ISO.
--
-- An AVCaptureManualExposureBracketedStillImageSettings instance defines the exposure duration and ISO settings that should be applied to one image in a bracket. An array of settings objects is passed to -[AVCaptureStillImageOutput captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:]. Min and max duration and ISO values are queryable properties of the AVCaptureDevice supplying data to an AVCaptureStillImageOutput instance. If you wish to leave exposureDuration unchanged for this bracketed still image, you may pass the special value AVCaptureExposureDurationCurrent. To keep ISO unchanged, you may pass AVCaptureISOCurrent (see AVCaptureDevice.h).
-- 
-- Phantom type for @AVCaptureManualExposureBracketedStillImageSettings@.
data AVCaptureManualExposureBracketedStillImageSettings

instance IsObjCObject (Id AVCaptureManualExposureBracketedStillImageSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureManualExposureBracketedStillImageSettings"

class IsAVCaptureBracketedStillImageSettings a => IsAVCaptureManualExposureBracketedStillImageSettings a where
  toAVCaptureManualExposureBracketedStillImageSettings :: a -> Id AVCaptureManualExposureBracketedStillImageSettings

instance IsAVCaptureManualExposureBracketedStillImageSettings (Id AVCaptureManualExposureBracketedStillImageSettings) where
  toAVCaptureManualExposureBracketedStillImageSettings = unsafeCastId

instance IsAVCaptureBracketedStillImageSettings (Id AVCaptureManualExposureBracketedStillImageSettings) where
  toAVCaptureBracketedStillImageSettings = unsafeCastId

instance IsNSObject (Id AVCaptureManualExposureBracketedStillImageSettings) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureIndexPicker ----------

-- | AVCaptureIndexPicker
--
-- An @AVCaptureControl@ for selecting from a set of mutually exclusive values by index.
--
-- @AVCaptureIndexPicker@ is ideal when the set of values is provided by an indexed container like @NSArray@, @Array@, or @Sequence@. Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- @AVCaptureIndexPicker@ uses zero-based indexing.
-- 
-- Phantom type for @AVCaptureIndexPicker@.
data AVCaptureIndexPicker

instance IsObjCObject (Id AVCaptureIndexPicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureIndexPicker"

class IsAVCaptureControl a => IsAVCaptureIndexPicker a where
  toAVCaptureIndexPicker :: a -> Id AVCaptureIndexPicker

instance IsAVCaptureIndexPicker (Id AVCaptureIndexPicker) where
  toAVCaptureIndexPicker = unsafeCastId

instance IsAVCaptureControl (Id AVCaptureIndexPicker) where
  toAVCaptureControl = unsafeCastId

instance IsNSObject (Id AVCaptureIndexPicker) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSlider ----------

-- | AVCaptureSlider
--
-- An @AVCaptureControl@ for selecting a value from a bounded range of values.
--
-- @AVCaptureSlider@ is ideal when your control only needs a single float value. Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
-- 
-- Phantom type for @AVCaptureSlider@.
data AVCaptureSlider

instance IsObjCObject (Id AVCaptureSlider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSlider"

class IsAVCaptureControl a => IsAVCaptureSlider a where
  toAVCaptureSlider :: a -> Id AVCaptureSlider

instance IsAVCaptureSlider (Id AVCaptureSlider) where
  toAVCaptureSlider = unsafeCastId

instance IsAVCaptureControl (Id AVCaptureSlider) where
  toAVCaptureControl = unsafeCastId

instance IsNSObject (Id AVCaptureSlider) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSystemExposureBiasSlider ----------

-- | AVCaptureSystemExposureBiasSlider
--
-- The system's recommended continuous exposure bias control for @-[AVCaptureDevice exposureTargetBias]@.
--
-- @AVCaptureSystemExposureBiasSlider@ uses the range specified by @systemRecommendedExposureBiasRange@ on the @activeFormat@ from the @AVCaptureDevice@ specified during initialization. As the device's @activeFormat@ changes, the slider updates its range with the new format's @systemRecommendedExposureBiasRange@.
--
-- Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
-- 
-- Phantom type for @AVCaptureSystemExposureBiasSlider@.
data AVCaptureSystemExposureBiasSlider

instance IsObjCObject (Id AVCaptureSystemExposureBiasSlider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSystemExposureBiasSlider"

class IsAVCaptureControl a => IsAVCaptureSystemExposureBiasSlider a where
  toAVCaptureSystemExposureBiasSlider :: a -> Id AVCaptureSystemExposureBiasSlider

instance IsAVCaptureSystemExposureBiasSlider (Id AVCaptureSystemExposureBiasSlider) where
  toAVCaptureSystemExposureBiasSlider = unsafeCastId

instance IsAVCaptureControl (Id AVCaptureSystemExposureBiasSlider) where
  toAVCaptureControl = unsafeCastId

instance IsNSObject (Id AVCaptureSystemExposureBiasSlider) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSystemZoomSlider ----------

-- | AVCaptureSystemZoomSlider
--
-- The system's recommended continuous zoom control for @-[AVCaptureDevice videoZoomFactor]@.
--
-- @AVCaptureSystemZoomSlider@ uses the range specified by the @systemRecommendedVideoZoomRange@ on the @activeFormat@ from the @AVCaptureDevice@ specified during initialization. As the device's @activeFormat@ changes, the slider updates its range with the new format's @systemRecommendedVideoZoomRange@.
--
-- Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
-- 
-- Phantom type for @AVCaptureSystemZoomSlider@.
data AVCaptureSystemZoomSlider

instance IsObjCObject (Id AVCaptureSystemZoomSlider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSystemZoomSlider"

class IsAVCaptureControl a => IsAVCaptureSystemZoomSlider a where
  toAVCaptureSystemZoomSlider :: a -> Id AVCaptureSystemZoomSlider

instance IsAVCaptureSystemZoomSlider (Id AVCaptureSystemZoomSlider) where
  toAVCaptureSystemZoomSlider = unsafeCastId

instance IsAVCaptureControl (Id AVCaptureSystemZoomSlider) where
  toAVCaptureControl = unsafeCastId

instance IsNSObject (Id AVCaptureSystemZoomSlider) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeviceInput ----------

-- | AVCaptureDeviceInput
--
-- AVCaptureDeviceInput is a concrete subclass of AVCaptureInput that provides an interface for capturing media from an AVCaptureDevice.
--
-- Instances of AVCaptureDeviceInput are input sources for AVCaptureSession that provide media data from devices connected to the system, represented by instances of AVCaptureDevice.
-- 
-- Phantom type for @AVCaptureDeviceInput@.
data AVCaptureDeviceInput

instance IsObjCObject (Id AVCaptureDeviceInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeviceInput"

class IsAVCaptureInput a => IsAVCaptureDeviceInput a where
  toAVCaptureDeviceInput :: a -> Id AVCaptureDeviceInput

instance IsAVCaptureDeviceInput (Id AVCaptureDeviceInput) where
  toAVCaptureDeviceInput = unsafeCastId

instance IsAVCaptureInput (Id AVCaptureDeviceInput) where
  toAVCaptureInput = unsafeCastId

instance IsNSObject (Id AVCaptureDeviceInput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureMetadataInput ----------

-- | AVCaptureMetadataInput
--
-- AVCaptureMetadataInput is a concrete subclass of AVCaptureInput that provides a way for clients to supply AVMetadataItems to an AVCaptureSession.
--
-- Instances of AVCaptureMetadataInput are input sources for AVCaptureSession that provide AVMetadataItems to an AVCaptureSession. AVCaptureMetadataInputs present one and only one AVCaptureInputPort, which currently may only be connected to an AVCaptureMovieFileOutput. The metadata supplied over the input port is provided by the client, and must conform to a client-supplied CMFormatDescription. The AVMetadataItems are supplied in an AVTimedMetadataGroup.
-- 
-- Phantom type for @AVCaptureMetadataInput@.
data AVCaptureMetadataInput

instance IsObjCObject (Id AVCaptureMetadataInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureMetadataInput"

class IsAVCaptureInput a => IsAVCaptureMetadataInput a where
  toAVCaptureMetadataInput :: a -> Id AVCaptureMetadataInput

instance IsAVCaptureMetadataInput (Id AVCaptureMetadataInput) where
  toAVCaptureMetadataInput = unsafeCastId

instance IsAVCaptureInput (Id AVCaptureMetadataInput) where
  toAVCaptureInput = unsafeCastId

instance IsNSObject (Id AVCaptureMetadataInput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureScreenInput ----------

-- | AVCaptureScreenInput
--
-- AVCaptureScreenInput is a concrete subclass of AVCaptureInput that provides an interface for capturing media from a screen or portion thereof.
--
-- Instances of AVCaptureScreenInput are input sources for AVCaptureSession that provide media data from one of the screens connected to the system, represented by CGDirectDisplayIDs.
-- 
-- Phantom type for @AVCaptureScreenInput@.
data AVCaptureScreenInput

instance IsObjCObject (Id AVCaptureScreenInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureScreenInput"

class IsAVCaptureInput a => IsAVCaptureScreenInput a where
  toAVCaptureScreenInput :: a -> Id AVCaptureScreenInput

instance IsAVCaptureScreenInput (Id AVCaptureScreenInput) where
  toAVCaptureScreenInput = unsafeCastId

instance IsAVCaptureInput (Id AVCaptureScreenInput) where
  toAVCaptureInput = unsafeCastId

instance IsNSObject (Id AVCaptureScreenInput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureAudioDataOutput ----------

-- | AVCaptureAudioDataOutput
--
-- AVCaptureAudioDataOutput is a concrete subclass of AVCaptureOutput that can be used to process uncompressed or compressed samples from the audio being captured.
--
-- Instances of AVCaptureAudioDataOutput produce audio sample buffers suitable for processing using other media APIs. Applications can access the sample buffers with the captureOutput:didOutputSampleBuffer:fromConnection: delegate method.
-- 
-- Phantom type for @AVCaptureAudioDataOutput@.
data AVCaptureAudioDataOutput

instance IsObjCObject (Id AVCaptureAudioDataOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureAudioDataOutput"

class IsAVCaptureOutput a => IsAVCaptureAudioDataOutput a where
  toAVCaptureAudioDataOutput :: a -> Id AVCaptureAudioDataOutput

instance IsAVCaptureAudioDataOutput (Id AVCaptureAudioDataOutput) where
  toAVCaptureAudioDataOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureAudioDataOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureAudioDataOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureAudioPreviewOutput ----------

-- | AVCaptureAudioPreviewOutput
--
-- AVCaptureAudioPreviewOutput is a concrete subclass of AVCaptureOutput that can be used to preview the audio being captured.
--
-- Instances of AVCaptureAudioPreviewOutput have an associated Core Audio output device that can be used to play audio being captured by the capture session. The unique ID of a Core Audio device can be obtained from its kAudioDevicePropertyDeviceUID property.
-- 
-- Phantom type for @AVCaptureAudioPreviewOutput@.
data AVCaptureAudioPreviewOutput

instance IsObjCObject (Id AVCaptureAudioPreviewOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureAudioPreviewOutput"

class IsAVCaptureOutput a => IsAVCaptureAudioPreviewOutput a where
  toAVCaptureAudioPreviewOutput :: a -> Id AVCaptureAudioPreviewOutput

instance IsAVCaptureAudioPreviewOutput (Id AVCaptureAudioPreviewOutput) where
  toAVCaptureAudioPreviewOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureAudioPreviewOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureAudioPreviewOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDepthDataOutput ----------

-- | AVCaptureDepthDataOutput
--
-- AVCaptureDepthDataOutput is a concrete subclass of AVCaptureOutput that can be used to process depth data in a streaming fashion.
--
-- Instances of AVCaptureDepthDataOutput capture AVDepthData objects expressing disparity/depth. Applications can access the frames with the depthDataOutput:didOutputDepthData:fromConnection: delegate method.
--
-- AVCaptureDepthDataOutput always provides depth data in the format expressed by its source's -[AVCaptureDevice activeDepthDataFormat] property. If you wish to receive depth data in another format, you may choose from the -[AVCaptureDevice activeFormat]'s -[AVCaptureDeviceFormat supportedDepthDataFormats], and set it using -[AVCaptureDevice setActiveDepthDataFormat:].
-- 
-- Phantom type for @AVCaptureDepthDataOutput@.
data AVCaptureDepthDataOutput

instance IsObjCObject (Id AVCaptureDepthDataOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDepthDataOutput"

class IsAVCaptureOutput a => IsAVCaptureDepthDataOutput a where
  toAVCaptureDepthDataOutput :: a -> Id AVCaptureDepthDataOutput

instance IsAVCaptureDepthDataOutput (Id AVCaptureDepthDataOutput) where
  toAVCaptureDepthDataOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureDepthDataOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureDepthDataOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureFileOutput ----------

-- | AVCaptureFileOutput
--
-- AVCaptureFileOutput is an abstract subclass of AVCaptureOutput that provides an interface for writing captured media to files.
--
-- This abstract superclass defines the interface for outputs that record media samples to files. File outputs can start recording to a new file using the startRecordingToOutputFileURL:recordingDelegate: method. On successive invocations of this method on macOS, the output file can by changed dynamically without losing media samples. A file output can stop recording using the stopRecording method. Because files are recorded in the background, applications will need to specify a delegate for each new file so that they can be notified when recorded files are finished.
--
-- On macOS, clients can also set a delegate on the file output itself that can be used to control recording along exact media sample boundaries using the captureOutput:didOutputSampleBuffer:fromConnection: method.
--
-- The concrete subclasses of AVCaptureFileOutput are AVCaptureMovieFileOutput, which records media to a QuickTime movie file, and AVCaptureAudioFileOutput, which writes audio media to a variety of audio file formats.
-- 
-- Phantom type for @AVCaptureFileOutput@.
data AVCaptureFileOutput

instance IsObjCObject (Id AVCaptureFileOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureFileOutput"

class IsAVCaptureOutput a => IsAVCaptureFileOutput a where
  toAVCaptureFileOutput :: a -> Id AVCaptureFileOutput

instance IsAVCaptureFileOutput (Id AVCaptureFileOutput) where
  toAVCaptureFileOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureFileOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureFileOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureMetadataOutput ----------

-- | AVCaptureMetadataOutput
--
-- AVCaptureMetadataOutput is a concrete subclass of AVCaptureOutput that can be used to process metadata objects from an attached connection.
--
-- Instances of AVCaptureMetadataOutput emit arrays of AVMetadataObject instances (see AVMetadataObject.h), such as detected faces. Applications can access the metadata objects with the captureOutput:didOutputMetadataObjects:fromConnection: delegate method.
-- 
-- Phantom type for @AVCaptureMetadataOutput@.
data AVCaptureMetadataOutput

instance IsObjCObject (Id AVCaptureMetadataOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureMetadataOutput"

class IsAVCaptureOutput a => IsAVCaptureMetadataOutput a where
  toAVCaptureMetadataOutput :: a -> Id AVCaptureMetadataOutput

instance IsAVCaptureMetadataOutput (Id AVCaptureMetadataOutput) where
  toAVCaptureMetadataOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureMetadataOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureMetadataOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCapturePhotoOutput ----------

-- | AVCapturePhotoOutput
--
-- AVCapturePhotoOutput is a concrete subclass of AVCaptureOutput that supersedes AVCaptureStillImageOutput as the preferred interface for capturing photos. In addition to capturing all flavors of still image supported by AVCaptureStillImageOutput, it supports Live Photo capture, preview-sized image delivery, wide color, RAW, RAW+JPG and RAW+DNG formats.
--
-- Taking a photo is multi-step process. Clients wishing to build a responsive UI need to know about the progress of a photo capture request as it advances from capture to processing to finished delivery. AVCapturePhotoOutput informs clients of photo capture progress through a delegate protocol. To take a picture, a client instantiates and configures an AVCapturePhotoSettings object, then calls AVCapturePhotoOutput's -capturePhotoWithSettings:delegate:, passing a delegate to be informed when events relating to the photo capture occur (e.g., the photo is about to be captured, the photo has been captured but not processed yet, the Live Photo movie is ready, etc.).
--
-- Some AVCapturePhotoSettings properties can be set to "Auto", such as flashMode. When set to AVCaptureFlashModeAuto, the photo output decides at capture time whether the current scene and lighting conditions require use of the flash. Thus the client doesn't know with certainty which features will be enabled when making the capture request. With the first and each subsequent delegate callback, the client is provided an AVCaptureResolvedPhotoSettings instance that indicates the settings that were applied to the capture. All "Auto" features have now been resolved to on or off. The AVCaptureResolvedPhotoSettings object passed in the client's delegate callbacks has a uniqueID identical to the AVCapturePhotoSettings request. This uniqueID allows clients to pair unresolved and resolved settings objects. See AVCapturePhotoCaptureDelegate below for a detailed discussion of the delegate callbacks.
--
-- Enabling certain photo features (Live Photo capture and high resolution capture) requires a reconfiguration of the capture render pipeline. Clients wishing to opt in for these features should call -setLivePhotoCaptureEnabled: and/or -setHighResolutionCaptureEnabled: before calling -startRunning on the AVCaptureSession. Changing any of these properties while the session is running requires a disruptive reconfiguration of the capture render pipeline. Live Photo captures in progress will be ended immediately; unfulfilled photo requests will be aborted; video preview will temporarily freeze. If you wish to capture Live Photos containing sound, you must add an audio AVCaptureDeviceInput to your AVCaptureSession.
--
-- Simultaneous Live Photo capture and MovieFileOutput capture is not supported. If an AVCaptureMovieFileOutput is added to your session, AVCapturePhotoOutput's livePhotoCaptureSupported property returns NO. Note that simultaneous Live Photo capture and AVCaptureVideoDataOutput is supported.
--
-- AVCaptureStillImageOutput and AVCapturePhotoOutput may not both be added to a capture session. You must use one or the other. If you add both to a session, a NSInvalidArgumentException is thrown.
--
-- AVCapturePhotoOutput implicitly supports wide color photo capture, following the activeColorSpace of the source AVCaptureDevice. If the source device's activeColorSpace is AVCaptureColorSpace_P3_D65, photos are encoded with wide color information, unless you've specified an output format of '420v', which does not support wide color.
-- 
-- Phantom type for @AVCapturePhotoOutput@.
data AVCapturePhotoOutput

instance IsObjCObject (Id AVCapturePhotoOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCapturePhotoOutput"

class IsAVCaptureOutput a => IsAVCapturePhotoOutput a where
  toAVCapturePhotoOutput :: a -> Id AVCapturePhotoOutput

instance IsAVCapturePhotoOutput (Id AVCapturePhotoOutput) where
  toAVCapturePhotoOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCapturePhotoOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCapturePhotoOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureStillImageOutput ----------

-- | AVCaptureStillImageOutput
--
-- AVCaptureStillImageOutput is a concrete subclass of AVCaptureOutput that can be used to capture high-quality still images with accompanying metadata.
--
-- Instances of AVCaptureStillImageOutput can be used to capture, on demand, high quality snapshots from a realtime capture source. Clients can request a still image for the current time using the captureStillImageAsynchronouslyFromConnection:completionHandler: method. Clients can also configure still image outputs to produce still images in specific image formats.
-- 
-- Phantom type for @AVCaptureStillImageOutput@.
data AVCaptureStillImageOutput

instance IsObjCObject (Id AVCaptureStillImageOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureStillImageOutput"

class IsAVCaptureOutput a => IsAVCaptureStillImageOutput a where
  toAVCaptureStillImageOutput :: a -> Id AVCaptureStillImageOutput

instance IsAVCaptureStillImageOutput (Id AVCaptureStillImageOutput) where
  toAVCaptureStillImageOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureStillImageOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureStillImageOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureVideoDataOutput ----------

-- | AVCaptureVideoDataOutput
--
-- AVCaptureVideoDataOutput is a concrete subclass of AVCaptureOutput that can be used to process uncompressed or compressed frames from the video being captured.
--
-- Instances of AVCaptureVideoDataOutput produce video frames suitable for processing using other media APIs. Applications can access the frames with the captureOutput:didOutputSampleBuffer:fromConnection: delegate method.
-- 
-- Phantom type for @AVCaptureVideoDataOutput@.
data AVCaptureVideoDataOutput

instance IsObjCObject (Id AVCaptureVideoDataOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureVideoDataOutput"

class IsAVCaptureOutput a => IsAVCaptureVideoDataOutput a where
  toAVCaptureVideoDataOutput :: a -> Id AVCaptureVideoDataOutput

instance IsAVCaptureVideoDataOutput (Id AVCaptureVideoDataOutput) where
  toAVCaptureVideoDataOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureVideoDataOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureVideoDataOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureDeferredPhotoProxy ----------

-- | AVCaptureDeferredPhotoProxy
--
-- A lightly-processed photo whose data may be used to process and fetch a higher-resolution asset at a later time.
--
-- An AVCaptureDeferredPhotoProxy behaves like a normal AVCapturePhoto, and approximates the look of the final rendered image.  This object represents intermediate data that can be rendered into a final image and ingested into the user's photo library via PHAsset APIs.  The intermediate data are not accessible by the calling process.
--
-- Use a PHAssetCreationRequest with a resourceType of PHAssetResourceTypePhotoProxy using the fileDataRepresentation of this object.  Image processing to finalize the asset will occur either on-demand when accessing the image data via PHImageManager or PHAssetResource, or will execute in the background when the system has determined that it's a good time to process based on thermals, battery level, and other conditions.  If the data provided to the PHAssetCreationRequest does not come from an AVCaptureDeferredPhotoProxy, then PHAssetCreationRequest will fail and a PHPhotosErrorInvalidResource error will be returned.
--
-- Below is a discussion of how the superclass properties behave on an AVCaptureDeferredPhotoProxy.
--
-- (readonly) CMTime timestamp;
--
-- The time of the capture; proxy and final photos will have the same timestamp.
--
-- (readonly) NSDictionary<NSString *, id> *metadata;
--
-- The metadata of the proxy image may differ slightly from the final photo's metadata where some fields may be updated.
--
-- (readonly, getter=isRawPhoto) BOOL rawPhoto;
--
-- Always NO, as deferred processing isn't available for raw photos.
--
-- (nullable, readonly) NSDictionary<NSString *, id> *embeddedThumbnailPhotoFormat;
--
-- Describes the embedded thumbnail format of both the proxy and the final photo which have the same dimensions and codec.
--
-- (readonly) AVCaptureResolvedPhotoSettings *resolvedSettings;
--
-- Describes the resolved settings of the whole capture, including the proxy and final photo. See AVCaptureResolvedPhotoSettings.deferredPhotoProxyDimensions.
--
-- (readonly) NSInteger photoCount;
--
-- Same for both proxy and final.
--
-- (nullable, readonly) AVCaptureDeviceType sourceDeviceType;
--
-- Same for both proxy and final.
--
-- (nullable, readonly) AVCaptureBracketedStillImageSettings *bracketSettings;
--
-- Same for both proxy and final.
--
-- (readonly) NSInteger sequenceCount;
--
-- Same for both proxy and final.
--
-- (readonly) AVCaptureLensStabilizationStatus lensStabilizationStatus;
--
-- Same for both proxy and final.
--
-- Superclass properties/methods that behave differently than a typical AVCapturePhoto:
--
-- (nullable, readonly) CVPixelBufferRef pixelBuffer NS_RETURNS_INNER_POINTER;
--
-- (nullable, readonly) CVPixelBufferRef previewPixelBuffer NS_RETURNS_INNER_POINTER;
--
-- - (nullable CGImageRef)CGImageRepresentation;         - (nullable CGImageRef)previewCGImageRepresentation;            All of the above properties return the same proxy image, either as a pixel buffer or CGImageRef.
--
-- - (nullable NSData *)fileDataRepresentation;         - (nullable NSData *)fileDataRepresentationWithCustomizer:(id<AVCapturePhotoFileDataRepresentationCustomizer>)customizer;             You may call either of the above two methods to create a NSData representation of the image, but note that it is only the proxy image quality being packaged.
-- 
-- Phantom type for @AVCaptureDeferredPhotoProxy@.
data AVCaptureDeferredPhotoProxy

instance IsObjCObject (Id AVCaptureDeferredPhotoProxy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureDeferredPhotoProxy"

class IsAVCapturePhoto a => IsAVCaptureDeferredPhotoProxy a where
  toAVCaptureDeferredPhotoProxy :: a -> Id AVCaptureDeferredPhotoProxy

instance IsAVCaptureDeferredPhotoProxy (Id AVCaptureDeferredPhotoProxy) where
  toAVCaptureDeferredPhotoProxy = unsafeCastId

instance IsAVCapturePhoto (Id AVCaptureDeferredPhotoProxy) where
  toAVCapturePhoto = unsafeCastId

instance IsNSObject (Id AVCaptureDeferredPhotoProxy) where
  toNSObject = unsafeCastId

-- ---------- AVCapturePhotoBracketSettings ----------

-- | AVCapturePhotoBracketSettings
--
-- A concrete subclass of AVCapturePhotoSettings that describes a bracketed capture.
--
-- In addition to the properties expressed in the base class, an AVCapturePhotoBracketSettings contains an array of AVCaptureBracketedStillImageSettings objects, where each describes one individual photo in the bracket. bracketedSettings.count must be <= AVCapturePhotoOutput's -maxBracketedCapturePhotoCount. Capturing a photo bracket may require the allocation of additional resources.
--
-- When you request a bracketed capture, your AVCapturePhotoCaptureDelegate's -captureOutput:didFinishProcessing{Photo | RawPhoto}... callbacks are called back bracketSettings.count times and provided with the corresponding AVCaptureBracketedStillImageSettings object from your request.
-- 
-- Phantom type for @AVCapturePhotoBracketSettings@.
data AVCapturePhotoBracketSettings

instance IsObjCObject (Id AVCapturePhotoBracketSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCapturePhotoBracketSettings"

class IsAVCapturePhotoSettings a => IsAVCapturePhotoBracketSettings a where
  toAVCapturePhotoBracketSettings :: a -> Id AVCapturePhotoBracketSettings

instance IsAVCapturePhotoBracketSettings (Id AVCapturePhotoBracketSettings) where
  toAVCapturePhotoBracketSettings = unsafeCastId

instance IsAVCapturePhotoSettings (Id AVCapturePhotoBracketSettings) where
  toAVCapturePhotoSettings = unsafeCastId

instance IsNSObject (Id AVCapturePhotoBracketSettings) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureMultiCamSession ----------

-- | AVCaptureMultiCamSession
--
-- A subclass of AVCaptureSession which supports simultaneous capture from multiple inputs of the same media type.
--
-- AVCaptureMultiCamSession's sessionPreset is always AVCaptureSessionPresetInputPriority and may not be set to any other value. Each input's device.activeFormat must be set manually to achieve the desired quality of service.
--
-- AVCaptureMultiCamSession supports dynamic enabling and disabling of individual camera inputs without interrupting preview. In order to stop an individual camera input, set the enabled property on all of its connections or connected ports to NO. When the last active connection or port is disabled, the source camera stops streaming to save power and bandwidth. Other inputs streaming data through the session are unaffected.
--
-- Prior to iOS 26, AVCaptureMultiCamSession requires all input devices to have an activeFormat where multiCamSupported returns YES. In applications linked on or after iOS 26, this requirement is not enforced when only a single input device is used.
-- 
-- Phantom type for @AVCaptureMultiCamSession@.
data AVCaptureMultiCamSession

instance IsObjCObject (Id AVCaptureMultiCamSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureMultiCamSession"

class IsAVCaptureSession a => IsAVCaptureMultiCamSession a where
  toAVCaptureMultiCamSession :: a -> Id AVCaptureMultiCamSession

instance IsAVCaptureMultiCamSession (Id AVCaptureMultiCamSession) where
  toAVCaptureMultiCamSession = unsafeCastId

instance IsAVCaptureSession (Id AVCaptureMultiCamSession) where
  toAVCaptureSession = unsafeCastId

instance IsNSObject (Id AVCaptureMultiCamSession) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSynchronizedDepthData ----------

-- | AVCaptureSynchronizedDepthData
--
-- An concrete subclass of AVCaptureSynchronizedData representing the data delivered by an AVCaptureDepthDataOutput.
--
-- Depth data, like video, may be dropped if not serviced in a timely fashion.
-- 
-- Phantom type for @AVCaptureSynchronizedDepthData@.
data AVCaptureSynchronizedDepthData

instance IsObjCObject (Id AVCaptureSynchronizedDepthData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSynchronizedDepthData"

class IsAVCaptureSynchronizedData a => IsAVCaptureSynchronizedDepthData a where
  toAVCaptureSynchronizedDepthData :: a -> Id AVCaptureSynchronizedDepthData

instance IsAVCaptureSynchronizedDepthData (Id AVCaptureSynchronizedDepthData) where
  toAVCaptureSynchronizedDepthData = unsafeCastId

instance IsAVCaptureSynchronizedData (Id AVCaptureSynchronizedDepthData) where
  toAVCaptureSynchronizedData = unsafeCastId

instance IsNSObject (Id AVCaptureSynchronizedDepthData) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSynchronizedMetadataObjectData ----------

-- | AVCaptureSynchronizedMetadataObjectData
--
-- An concrete subclass of AVCaptureSynchronizedData representing the data delivered by an AVCaptureMetadataOutput.
--
-- A single AVCaptureMetadataOutput may be configured to deliver multiple kinds of metadata objects (such as QRCodes and detected faces). AVCaptureSynchronizedMetadataObjectData's -metadataObjects array may contain multiple AVMetadataObject subclasses, depending on how the AVCaptureMetadataOutput was configured. All synchronized metadata objects share a common timestamp.
-- 
-- Phantom type for @AVCaptureSynchronizedMetadataObjectData@.
data AVCaptureSynchronizedMetadataObjectData

instance IsObjCObject (Id AVCaptureSynchronizedMetadataObjectData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSynchronizedMetadataObjectData"

class IsAVCaptureSynchronizedData a => IsAVCaptureSynchronizedMetadataObjectData a where
  toAVCaptureSynchronizedMetadataObjectData :: a -> Id AVCaptureSynchronizedMetadataObjectData

instance IsAVCaptureSynchronizedMetadataObjectData (Id AVCaptureSynchronizedMetadataObjectData) where
  toAVCaptureSynchronizedMetadataObjectData = unsafeCastId

instance IsAVCaptureSynchronizedData (Id AVCaptureSynchronizedMetadataObjectData) where
  toAVCaptureSynchronizedData = unsafeCastId

instance IsNSObject (Id AVCaptureSynchronizedMetadataObjectData) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureSynchronizedSampleBufferData ----------

-- | AVCaptureSynchronizedSampleBufferData
--
-- An concrete subclass of AVCaptureSynchronizedData representing the data delivered by an AVCaptureVideoDataOutput or AVCaptureAudioDataOutput.
--
-- Synchronized sample buffer data is valid for the duration of AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedData: delegate callback. To extend the sample buffer data beyond the callback, you must CFRetain it, and later call CFRelease when you're done with it.
-- 
-- Phantom type for @AVCaptureSynchronizedSampleBufferData@.
data AVCaptureSynchronizedSampleBufferData

instance IsObjCObject (Id AVCaptureSynchronizedSampleBufferData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureSynchronizedSampleBufferData"

class IsAVCaptureSynchronizedData a => IsAVCaptureSynchronizedSampleBufferData a where
  toAVCaptureSynchronizedSampleBufferData :: a -> Id AVCaptureSynchronizedSampleBufferData

instance IsAVCaptureSynchronizedSampleBufferData (Id AVCaptureSynchronizedSampleBufferData) where
  toAVCaptureSynchronizedSampleBufferData = unsafeCastId

instance IsAVCaptureSynchronizedData (Id AVCaptureSynchronizedSampleBufferData) where
  toAVCaptureSynchronizedData = unsafeCastId

instance IsNSObject (Id AVCaptureSynchronizedSampleBufferData) where
  toNSObject = unsafeCastId

-- ---------- AVPersistableContentKeyRequest ----------

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPersistableContentKeyRequest@.
data AVPersistableContentKeyRequest

instance IsObjCObject (Id AVPersistableContentKeyRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPersistableContentKeyRequest"

class IsAVContentKeyRequest a => IsAVPersistableContentKeyRequest a where
  toAVPersistableContentKeyRequest :: a -> Id AVPersistableContentKeyRequest

instance IsAVPersistableContentKeyRequest (Id AVPersistableContentKeyRequest) where
  toAVPersistableContentKeyRequest = unsafeCastId

instance IsAVContentKeyRequest (Id AVPersistableContentKeyRequest) where
  toAVContentKeyRequest = unsafeCastId

instance IsNSObject (Id AVPersistableContentKeyRequest) where
  toNSObject = unsafeCastId

-- ---------- AVDelegatingPlaybackCoordinatorBufferingCommand ----------

-- | A playback command requesting buffering in anticipation of playback.
--
-- Receiving this command should be reflected to the user as playback in a buffering state. To cancel the group intent to begin playback and move back into a paused state, call [AVDelegatingPlaybackCoordinator coordinateRateChangeToRate:0 options: 0]
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVDelegatingPlaybackCoordinatorBufferingCommand@.
data AVDelegatingPlaybackCoordinatorBufferingCommand

instance IsObjCObject (Id AVDelegatingPlaybackCoordinatorBufferingCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDelegatingPlaybackCoordinatorBufferingCommand"

class IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand a => IsAVDelegatingPlaybackCoordinatorBufferingCommand a where
  toAVDelegatingPlaybackCoordinatorBufferingCommand :: a -> Id AVDelegatingPlaybackCoordinatorBufferingCommand

instance IsAVDelegatingPlaybackCoordinatorBufferingCommand (Id AVDelegatingPlaybackCoordinatorBufferingCommand) where
  toAVDelegatingPlaybackCoordinatorBufferingCommand = unsafeCastId

instance IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand (Id AVDelegatingPlaybackCoordinatorBufferingCommand) where
  toAVDelegatingPlaybackCoordinatorPlaybackControlCommand = unsafeCastId

instance IsNSObject (Id AVDelegatingPlaybackCoordinatorBufferingCommand) where
  toNSObject = unsafeCastId

-- ---------- AVDelegatingPlaybackCoordinatorPauseCommand ----------

-- | A playback command requesting a pause
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVDelegatingPlaybackCoordinatorPauseCommand@.
data AVDelegatingPlaybackCoordinatorPauseCommand

instance IsObjCObject (Id AVDelegatingPlaybackCoordinatorPauseCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDelegatingPlaybackCoordinatorPauseCommand"

class IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand a => IsAVDelegatingPlaybackCoordinatorPauseCommand a where
  toAVDelegatingPlaybackCoordinatorPauseCommand :: a -> Id AVDelegatingPlaybackCoordinatorPauseCommand

instance IsAVDelegatingPlaybackCoordinatorPauseCommand (Id AVDelegatingPlaybackCoordinatorPauseCommand) where
  toAVDelegatingPlaybackCoordinatorPauseCommand = unsafeCastId

instance IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand (Id AVDelegatingPlaybackCoordinatorPauseCommand) where
  toAVDelegatingPlaybackCoordinatorPlaybackControlCommand = unsafeCastId

instance IsNSObject (Id AVDelegatingPlaybackCoordinatorPauseCommand) where
  toNSObject = unsafeCastId

-- ---------- AVDelegatingPlaybackCoordinatorPlayCommand ----------

-- | A playback command requesting playback with specific timing.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVDelegatingPlaybackCoordinatorPlayCommand@.
data AVDelegatingPlaybackCoordinatorPlayCommand

instance IsObjCObject (Id AVDelegatingPlaybackCoordinatorPlayCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDelegatingPlaybackCoordinatorPlayCommand"

class IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand a => IsAVDelegatingPlaybackCoordinatorPlayCommand a where
  toAVDelegatingPlaybackCoordinatorPlayCommand :: a -> Id AVDelegatingPlaybackCoordinatorPlayCommand

instance IsAVDelegatingPlaybackCoordinatorPlayCommand (Id AVDelegatingPlaybackCoordinatorPlayCommand) where
  toAVDelegatingPlaybackCoordinatorPlayCommand = unsafeCastId

instance IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand (Id AVDelegatingPlaybackCoordinatorPlayCommand) where
  toAVDelegatingPlaybackCoordinatorPlaybackControlCommand = unsafeCastId

instance IsNSObject (Id AVDelegatingPlaybackCoordinatorPlayCommand) where
  toNSObject = unsafeCastId

-- ---------- AVDelegatingPlaybackCoordinatorSeekCommand ----------

-- | A playback command requesting a seek.
--
-- If the current playback rate is non-zero, playback should not automatically resume after the seek. Instead the delegate should pause and wait for the coordinator to issue another PlayCommand. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVDelegatingPlaybackCoordinatorSeekCommand@.
data AVDelegatingPlaybackCoordinatorSeekCommand

instance IsObjCObject (Id AVDelegatingPlaybackCoordinatorSeekCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDelegatingPlaybackCoordinatorSeekCommand"

class IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand a => IsAVDelegatingPlaybackCoordinatorSeekCommand a where
  toAVDelegatingPlaybackCoordinatorSeekCommand :: a -> Id AVDelegatingPlaybackCoordinatorSeekCommand

instance IsAVDelegatingPlaybackCoordinatorSeekCommand (Id AVDelegatingPlaybackCoordinatorSeekCommand) where
  toAVDelegatingPlaybackCoordinatorSeekCommand = unsafeCastId

instance IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand (Id AVDelegatingPlaybackCoordinatorSeekCommand) where
  toAVDelegatingPlaybackCoordinatorPlaybackControlCommand = unsafeCastId

instance IsNSObject (Id AVDelegatingPlaybackCoordinatorSeekCommand) where
  toNSObject = unsafeCastId

-- ---------- AVFragmentedMovieMinder ----------

-- | AVFragmentedMovieMinder
--
-- A class that periodically checks whether additional movie fragments have been appended to fragmented movie files.
--
-- AVFragmentedMovieMinder is identical to AVFragmentedAssetMinder except that it's capable of minding only assets of class AVFragmentedMovie.
-- 
-- Phantom type for @AVFragmentedMovieMinder@.
data AVFragmentedMovieMinder

instance IsObjCObject (Id AVFragmentedMovieMinder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFragmentedMovieMinder"

class IsAVFragmentedAssetMinder a => IsAVFragmentedMovieMinder a where
  toAVFragmentedMovieMinder :: a -> Id AVFragmentedMovieMinder

instance IsAVFragmentedMovieMinder (Id AVFragmentedMovieMinder) where
  toAVFragmentedMovieMinder = unsafeCastId

instance IsAVFragmentedAssetMinder (Id AVFragmentedMovieMinder) where
  toAVFragmentedAssetMinder = unsafeCastId

instance IsNSObject (Id AVFragmentedMovieMinder) where
  toNSObject = unsafeCastId

-- ---------- AVMutableMediaSelection ----------

-- | Phantom type for @AVMutableMediaSelection@.
data AVMutableMediaSelection

instance IsObjCObject (Id AVMutableMediaSelection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableMediaSelection"

class IsAVMediaSelection a => IsAVMutableMediaSelection a where
  toAVMutableMediaSelection :: a -> Id AVMutableMediaSelection

instance IsAVMutableMediaSelection (Id AVMutableMediaSelection) where
  toAVMutableMediaSelection = unsafeCastId

instance IsAVMediaSelection (Id AVMutableMediaSelection) where
  toAVMediaSelection = unsafeCastId

instance IsNSObject (Id AVMutableMediaSelection) where
  toNSObject = unsafeCastId

-- ---------- AVAssetWriterInputGroup ----------

-- | Phantom type for @AVAssetWriterInputGroup@.
data AVAssetWriterInputGroup

instance IsObjCObject (Id AVAssetWriterInputGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetWriterInputGroup"

class IsAVMediaSelectionGroup a => IsAVAssetWriterInputGroup a where
  toAVAssetWriterInputGroup :: a -> Id AVAssetWriterInputGroup

instance IsAVAssetWriterInputGroup (Id AVAssetWriterInputGroup) where
  toAVAssetWriterInputGroup = unsafeCastId

instance IsAVMediaSelectionGroup (Id AVAssetWriterInputGroup) where
  toAVMediaSelectionGroup = unsafeCastId

instance IsNSObject (Id AVAssetWriterInputGroup) where
  toNSObject = unsafeCastId

-- ---------- AVDateRangeMetadataGroup ----------

-- | AVDateRangeMetadataGroup
--
-- AVDateRangeMetadataGroup is used to represent a collection of metadata items that are valid for use within a specific range of dates.
-- 
-- Phantom type for @AVDateRangeMetadataGroup@.
data AVDateRangeMetadataGroup

instance IsObjCObject (Id AVDateRangeMetadataGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDateRangeMetadataGroup"

class IsAVMetadataGroup a => IsAVDateRangeMetadataGroup a where
  toAVDateRangeMetadataGroup :: a -> Id AVDateRangeMetadataGroup

instance IsAVDateRangeMetadataGroup (Id AVDateRangeMetadataGroup) where
  toAVDateRangeMetadataGroup = unsafeCastId

instance IsAVMetadataGroup (Id AVDateRangeMetadataGroup) where
  toAVMetadataGroup = unsafeCastId

instance IsNSObject (Id AVDateRangeMetadataGroup) where
  toNSObject = unsafeCastId

-- ---------- AVTimedMetadataGroup ----------

-- | AVTimedMetadataGroup
--
-- AVTimedMetadataGroup is used to represent a collection of metadata items that are valid for use during a specific range of time. For example, AVTimedMetadataGroups are used to represent chapters, optionally containing metadata items for chapter titles and chapter images.
-- 
-- Phantom type for @AVTimedMetadataGroup@.
data AVTimedMetadataGroup

instance IsObjCObject (Id AVTimedMetadataGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVTimedMetadataGroup"

class IsAVMetadataGroup a => IsAVTimedMetadataGroup a where
  toAVTimedMetadataGroup :: a -> Id AVTimedMetadataGroup

instance IsAVTimedMetadataGroup (Id AVTimedMetadataGroup) where
  toAVTimedMetadataGroup = unsafeCastId

instance IsAVMetadataGroup (Id AVTimedMetadataGroup) where
  toAVMetadataGroup = unsafeCastId

instance IsNSObject (Id AVTimedMetadataGroup) where
  toNSObject = unsafeCastId

-- ---------- AVMutableMetadataItem ----------

-- | Phantom type for @AVMutableMetadataItem@.
data AVMutableMetadataItem

instance IsObjCObject (Id AVMutableMetadataItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableMetadataItem"

class IsAVMetadataItem a => IsAVMutableMetadataItem a where
  toAVMutableMetadataItem :: a -> Id AVMutableMetadataItem

instance IsAVMutableMetadataItem (Id AVMutableMetadataItem) where
  toAVMutableMetadataItem = unsafeCastId

instance IsAVMetadataItem (Id AVMutableMetadataItem) where
  toAVMetadataItem = unsafeCastId

instance IsNSObject (Id AVMutableMetadataItem) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataBodyObject ----------

-- | AVMetadataBodyObject
--
-- AVMetadataBodyObject is an abstract class that defines an interface for a body metadata object used by AVFoundation.
--
-- AVMetadataBodyObject represents a single detected body in a picture. It is the base object used to represent bodies, for example AVMetadataHumanBodyObject, AVMetadataCatBodyObject, AVMetadataDogBodyObject.
-- 
-- Phantom type for @AVMetadataBodyObject@.
data AVMetadataBodyObject

instance IsObjCObject (Id AVMetadataBodyObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataBodyObject"

class IsAVMetadataObject a => IsAVMetadataBodyObject a where
  toAVMetadataBodyObject :: a -> Id AVMetadataBodyObject

instance IsAVMetadataBodyObject (Id AVMetadataBodyObject) where
  toAVMetadataBodyObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataBodyObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataBodyObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataCatHeadObject ----------

-- | A concrete metadata object subclass representing a cat head.
--
-- ``AVMetadataCatHeadObject`` is a concrete subclass of ``AVMetadataObject`` representing a cat head.
-- 
-- Phantom type for @AVMetadataCatHeadObject@.
data AVMetadataCatHeadObject

instance IsObjCObject (Id AVMetadataCatHeadObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataCatHeadObject"

class IsAVMetadataObject a => IsAVMetadataCatHeadObject a where
  toAVMetadataCatHeadObject :: a -> Id AVMetadataCatHeadObject

instance IsAVMetadataCatHeadObject (Id AVMetadataCatHeadObject) where
  toAVMetadataCatHeadObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataCatHeadObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataCatHeadObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataDogHeadObject ----------

-- | A concrete metadata object subclass representing a dog head.
--
-- ``AVMetadataDogHeadObject`` is a concrete subclass of ``AVMetadataObject`` representing a dog head.
-- 
-- Phantom type for @AVMetadataDogHeadObject@.
data AVMetadataDogHeadObject

instance IsObjCObject (Id AVMetadataDogHeadObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataDogHeadObject"

class IsAVMetadataObject a => IsAVMetadataDogHeadObject a where
  toAVMetadataDogHeadObject :: a -> Id AVMetadataDogHeadObject

instance IsAVMetadataDogHeadObject (Id AVMetadataDogHeadObject) where
  toAVMetadataDogHeadObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataDogHeadObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataDogHeadObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataFaceObject ----------

-- | AVMetadataFaceObject
--
-- AVMetadataFaceObject is a concrete subclass of AVMetadataObject defining the features of a detected face.
--
-- AVMetadataFaceObject represents a single detected face in a picture. It is an immutable object describing the various features found in the face.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected face objects. See AVCaptureOutput.h.
-- 
-- Phantom type for @AVMetadataFaceObject@.
data AVMetadataFaceObject

instance IsObjCObject (Id AVMetadataFaceObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataFaceObject"

class IsAVMetadataObject a => IsAVMetadataFaceObject a where
  toAVMetadataFaceObject :: a -> Id AVMetadataFaceObject

instance IsAVMetadataFaceObject (Id AVMetadataFaceObject) where
  toAVMetadataFaceObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataFaceObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataFaceObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataMachineReadableCodeObject ----------

-- | AVMetadataMachineReadableCodeObject
--
-- AVMetadataMachineReadableCodeObject is a concrete subclass of AVMetadataObject defining the features of a detected one-dimensional or two-dimensional barcode.
--
-- AVMetadataMachineReadableCodeObject represents a single detected machine readable code in a picture. It is an immutable object describing the features and payload of a barcode.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected machine readable code objects. See AVCaptureMetadataOutput.h.
-- 
-- Phantom type for @AVMetadataMachineReadableCodeObject@.
data AVMetadataMachineReadableCodeObject

instance IsObjCObject (Id AVMetadataMachineReadableCodeObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataMachineReadableCodeObject"

class IsAVMetadataObject a => IsAVMetadataMachineReadableCodeObject a where
  toAVMetadataMachineReadableCodeObject :: a -> Id AVMetadataMachineReadableCodeObject

instance IsAVMetadataMachineReadableCodeObject (Id AVMetadataMachineReadableCodeObject) where
  toAVMetadataMachineReadableCodeObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataMachineReadableCodeObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataMachineReadableCodeObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataSalientObject ----------

-- | AVMetadataSalientObject
--
-- AVMetadataSalientObject is a concrete subclass of AVMetadataObject defining the features of a salient object.
--
-- AVMetadataSalientObject represents a single detected salient area in a picture. It is an immutable object describing the salient object.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected salient objects. See AVCaptureOutput.h.
-- 
-- Phantom type for @AVMetadataSalientObject@.
data AVMetadataSalientObject

instance IsObjCObject (Id AVMetadataSalientObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataSalientObject"

class IsAVMetadataObject a => IsAVMetadataSalientObject a where
  toAVMetadataSalientObject :: a -> Id AVMetadataSalientObject

instance IsAVMetadataSalientObject (Id AVMetadataSalientObject) where
  toAVMetadataSalientObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataSalientObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataSalientObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetricContentKeyRequestEvent ----------

-- | Represents a metric event associated with a HLS content key resource request.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricContentKeyRequestEvent@.
data AVMetricContentKeyRequestEvent

instance IsObjCObject (Id AVMetricContentKeyRequestEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricContentKeyRequestEvent"

class IsAVMetricEvent a => IsAVMetricContentKeyRequestEvent a where
  toAVMetricContentKeyRequestEvent :: a -> Id AVMetricContentKeyRequestEvent

instance IsAVMetricContentKeyRequestEvent (Id AVMetricContentKeyRequestEvent) where
  toAVMetricContentKeyRequestEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricContentKeyRequestEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricContentKeyRequestEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricDownloadSummaryEvent ----------

-- | Represents a summary metric event with aggregated metrics for the entire download task.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricDownloadSummaryEvent@.
data AVMetricDownloadSummaryEvent

instance IsObjCObject (Id AVMetricDownloadSummaryEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricDownloadSummaryEvent"

class IsAVMetricEvent a => IsAVMetricDownloadSummaryEvent a where
  toAVMetricDownloadSummaryEvent :: a -> Id AVMetricDownloadSummaryEvent

instance IsAVMetricDownloadSummaryEvent (Id AVMetricDownloadSummaryEvent) where
  toAVMetricDownloadSummaryEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricDownloadSummaryEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricDownloadSummaryEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricErrorEvent ----------

-- | Represents a metric event when an error occurred.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricErrorEvent@.
data AVMetricErrorEvent

instance IsObjCObject (Id AVMetricErrorEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricErrorEvent"

class IsAVMetricEvent a => IsAVMetricErrorEvent a where
  toAVMetricErrorEvent :: a -> Id AVMetricErrorEvent

instance IsAVMetricErrorEvent (Id AVMetricErrorEvent) where
  toAVMetricErrorEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricErrorEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricErrorEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricHLSMediaSegmentRequestEvent ----------

-- | Represents a metric event associated with a HLS media segment resource request.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricHLSMediaSegmentRequestEvent@.
data AVMetricHLSMediaSegmentRequestEvent

instance IsObjCObject (Id AVMetricHLSMediaSegmentRequestEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricHLSMediaSegmentRequestEvent"

class IsAVMetricEvent a => IsAVMetricHLSMediaSegmentRequestEvent a where
  toAVMetricHLSMediaSegmentRequestEvent :: a -> Id AVMetricHLSMediaSegmentRequestEvent

instance IsAVMetricHLSMediaSegmentRequestEvent (Id AVMetricHLSMediaSegmentRequestEvent) where
  toAVMetricHLSMediaSegmentRequestEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricHLSMediaSegmentRequestEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricHLSMediaSegmentRequestEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricHLSPlaylistRequestEvent ----------

-- | Represents a metric event associated with a HLS playlist resource request.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricHLSPlaylistRequestEvent@.
data AVMetricHLSPlaylistRequestEvent

instance IsObjCObject (Id AVMetricHLSPlaylistRequestEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricHLSPlaylistRequestEvent"

class IsAVMetricEvent a => IsAVMetricHLSPlaylistRequestEvent a where
  toAVMetricHLSPlaylistRequestEvent :: a -> Id AVMetricHLSPlaylistRequestEvent

instance IsAVMetricHLSPlaylistRequestEvent (Id AVMetricHLSPlaylistRequestEvent) where
  toAVMetricHLSPlaylistRequestEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricHLSPlaylistRequestEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricHLSPlaylistRequestEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricMediaResourceRequestEvent ----------

-- | Represents a metric event associated with media resource requests.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricMediaResourceRequestEvent@.
data AVMetricMediaResourceRequestEvent

instance IsObjCObject (Id AVMetricMediaResourceRequestEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricMediaResourceRequestEvent"

class IsAVMetricEvent a => IsAVMetricMediaResourceRequestEvent a where
  toAVMetricMediaResourceRequestEvent :: a -> Id AVMetricMediaResourceRequestEvent

instance IsAVMetricMediaResourceRequestEvent (Id AVMetricMediaResourceRequestEvent) where
  toAVMetricMediaResourceRequestEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricMediaResourceRequestEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricMediaResourceRequestEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemLikelyToKeepUpEvent ----------

-- | Represents a metric event when playback was likely to play through without stalling.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemLikelyToKeepUpEvent@.
data AVMetricPlayerItemLikelyToKeepUpEvent

instance IsObjCObject (Id AVMetricPlayerItemLikelyToKeepUpEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemLikelyToKeepUpEvent"

class IsAVMetricEvent a => IsAVMetricPlayerItemLikelyToKeepUpEvent a where
  toAVMetricPlayerItemLikelyToKeepUpEvent :: a -> Id AVMetricPlayerItemLikelyToKeepUpEvent

instance IsAVMetricPlayerItemLikelyToKeepUpEvent (Id AVMetricPlayerItemLikelyToKeepUpEvent) where
  toAVMetricPlayerItemLikelyToKeepUpEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemLikelyToKeepUpEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemLikelyToKeepUpEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemPlaybackSummaryEvent ----------

-- | Represents a summary metric event with aggregated metrics for the entire playback session.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemPlaybackSummaryEvent@.
data AVMetricPlayerItemPlaybackSummaryEvent

instance IsObjCObject (Id AVMetricPlayerItemPlaybackSummaryEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemPlaybackSummaryEvent"

class IsAVMetricEvent a => IsAVMetricPlayerItemPlaybackSummaryEvent a where
  toAVMetricPlayerItemPlaybackSummaryEvent :: a -> Id AVMetricPlayerItemPlaybackSummaryEvent

instance IsAVMetricPlayerItemPlaybackSummaryEvent (Id AVMetricPlayerItemPlaybackSummaryEvent) where
  toAVMetricPlayerItemPlaybackSummaryEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemPlaybackSummaryEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemPlaybackSummaryEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemRateChangeEvent ----------

-- | Represents a metric event when playback rate change occurred.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemRateChangeEvent@.
data AVMetricPlayerItemRateChangeEvent

instance IsObjCObject (Id AVMetricPlayerItemRateChangeEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemRateChangeEvent"

class IsAVMetricEvent a => IsAVMetricPlayerItemRateChangeEvent a where
  toAVMetricPlayerItemRateChangeEvent :: a -> Id AVMetricPlayerItemRateChangeEvent

instance IsAVMetricPlayerItemRateChangeEvent (Id AVMetricPlayerItemRateChangeEvent) where
  toAVMetricPlayerItemRateChangeEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemRateChangeEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemRateChangeEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemVariantSwitchEvent ----------

-- | Represents a metric event when variant switch was completed.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemVariantSwitchEvent@.
data AVMetricPlayerItemVariantSwitchEvent

instance IsObjCObject (Id AVMetricPlayerItemVariantSwitchEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemVariantSwitchEvent"

class IsAVMetricEvent a => IsAVMetricPlayerItemVariantSwitchEvent a where
  toAVMetricPlayerItemVariantSwitchEvent :: a -> Id AVMetricPlayerItemVariantSwitchEvent

instance IsAVMetricPlayerItemVariantSwitchEvent (Id AVMetricPlayerItemVariantSwitchEvent) where
  toAVMetricPlayerItemVariantSwitchEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemVariantSwitchEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemVariantSwitchEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemVariantSwitchStartEvent ----------

-- | Represents a metric event when variant switch was attempted.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemVariantSwitchStartEvent@.
data AVMetricPlayerItemVariantSwitchStartEvent

instance IsObjCObject (Id AVMetricPlayerItemVariantSwitchStartEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemVariantSwitchStartEvent"

class IsAVMetricEvent a => IsAVMetricPlayerItemVariantSwitchStartEvent a where
  toAVMetricPlayerItemVariantSwitchStartEvent :: a -> Id AVMetricPlayerItemVariantSwitchStartEvent

instance IsAVMetricPlayerItemVariantSwitchStartEvent (Id AVMetricPlayerItemVariantSwitchStartEvent) where
  toAVMetricPlayerItemVariantSwitchStartEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemVariantSwitchStartEvent) where
  toAVMetricEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemVariantSwitchStartEvent) where
  toNSObject = unsafeCastId

-- ---------- AVDelegatingPlaybackCoordinator ----------

-- | An AVPlaybackCoordinator subclass for controlling a custom playback object.
--
-- - NOTE: Use AVPlayer's playbackCoordinator property to get an AVPlaybackCoordinator for an AVPlayer.
-- 
-- Phantom type for @AVDelegatingPlaybackCoordinator@.
data AVDelegatingPlaybackCoordinator

instance IsObjCObject (Id AVDelegatingPlaybackCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVDelegatingPlaybackCoordinator"

class IsAVPlaybackCoordinator a => IsAVDelegatingPlaybackCoordinator a where
  toAVDelegatingPlaybackCoordinator :: a -> Id AVDelegatingPlaybackCoordinator

instance IsAVDelegatingPlaybackCoordinator (Id AVDelegatingPlaybackCoordinator) where
  toAVDelegatingPlaybackCoordinator = unsafeCastId

instance IsAVPlaybackCoordinator (Id AVDelegatingPlaybackCoordinator) where
  toAVPlaybackCoordinator = unsafeCastId

instance IsNSObject (Id AVDelegatingPlaybackCoordinator) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerPlaybackCoordinator ----------

-- | An AVPlaybackCoordinator subclass for controlling an AVPlayer
--
-- While the coordinator is connected to other participants, it will intercept rate changes and seeks issued to the player to share these with other participants if appropriate. Clients of AVPlayer can thus use the AVPlayer interfaces to modify the playback state of connected participants. When appropriate, the coordinator will also impose rate changes and seeks from other participants on the player. If this occurs, the corresponding notifications will carry an originating participant in their payload. See AVPlayer's playbackCoordinator property for more details about player behavior changes. AVPlayerPlaybackCoordinator may begin suspensions on behalf of the player when the player's timeControlStatus changes from AVPlayerTimeControlStatusPlaying to AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate or AVPlayerTimeControlStatusPaused. These suspensions will end when the player's timeControlStatus changes back to AVPlayerTimeControlStatusPlaying. This means that a suspension that begun because the player entered a waiting state, will end automatically when the player is done waiting. A suspension that begun because the player paused, will only end once the player's rate changes back to non-zero.
-- 
-- Phantom type for @AVPlayerPlaybackCoordinator@.
data AVPlayerPlaybackCoordinator

instance IsObjCObject (Id AVPlayerPlaybackCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerPlaybackCoordinator"

class IsAVPlaybackCoordinator a => IsAVPlayerPlaybackCoordinator a where
  toAVPlayerPlaybackCoordinator :: a -> Id AVPlayerPlaybackCoordinator

instance IsAVPlayerPlaybackCoordinator (Id AVPlayerPlaybackCoordinator) where
  toAVPlayerPlaybackCoordinator = unsafeCastId

instance IsAVPlaybackCoordinator (Id AVPlayerPlaybackCoordinator) where
  toAVPlaybackCoordinator = unsafeCastId

instance IsNSObject (Id AVPlayerPlaybackCoordinator) where
  toNSObject = unsafeCastId

-- ---------- AVQueuePlayer ----------

-- | AVQueuePlayer is a subclass of AVPlayer that offers an interface for multiple-item playback.
--
-- AVQueuePlayer extends AVPlayer with methods for managing a queue of items to be played in sequence. It plays these items as gaplessly as possible in the current runtime environment, depending on  the timely availability of media data for the enqueued items.
--
-- For best performance clients should typically enqueue only as many AVPlayerItems as are necessary to ensure smooth playback. Note that once an item is enqueued it becomes eligible to be loaded and made ready for playback, with whatever I/O and processing overhead that entails.
-- 
-- Phantom type for @AVQueuePlayer@.
data AVQueuePlayer

instance IsObjCObject (Id AVQueuePlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVQueuePlayer"

class IsAVPlayer a => IsAVQueuePlayer a where
  toAVQueuePlayer :: a -> Id AVQueuePlayer

instance IsAVQueuePlayer (Id AVQueuePlayer) where
  toAVQueuePlayer = unsafeCastId

instance IsAVPlayer (Id AVQueuePlayer) where
  toAVPlayer = unsafeCastId

instance IsNSObject (Id AVQueuePlayer) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerInterstitialEventController ----------

-- | An AVPlayerInterstitialEventController allows you to specify a schedule of interstitial events for items played by a primary player. By creating an instance of AVPlayerInterstitialEventController and setting a schedule of interstitial events, you pre-empt directives the are intrinsic to the items played by the primary player, if any exist, causing them to be ignored.
--
-- The schedule of interstitial events is specified as an array of AVPlayerInterstitialEvents. For each AVPlayerInterstitialEvent, when the primary player's current item is the primary item of the interstitial event and its currentDate reaches the date of the event, playback of the primary item by the primary player is temporarily suspended, i.e. its timeControlStatus changes to AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate and its reasonForWaitingToPlay will change to AVPlayerWaitingDuringInterstitialEventReason. During this suspension, playback of items that replicate the interstitial template items of the event are played by the interstitial player, which temporarily assumes the output configuration of the primary player; for example, its visual content will be routed to AVPlayerLayers that reference the primary player. Once the interstitial player has advanced through playback of the interstitial items specified by the event or its current item otherwise becomes nil, playback of the primary content will resume, at an offset from the time at which it was suspended as specified by the event.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerInterstitialEventController@.
data AVPlayerInterstitialEventController

instance IsObjCObject (Id AVPlayerInterstitialEventController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerInterstitialEventController"

class IsAVPlayerInterstitialEventMonitor a => IsAVPlayerInterstitialEventController a where
  toAVPlayerInterstitialEventController :: a -> Id AVPlayerInterstitialEventController

instance IsAVPlayerInterstitialEventController (Id AVPlayerInterstitialEventController) where
  toAVPlayerInterstitialEventController = unsafeCastId

instance IsAVPlayerInterstitialEventMonitor (Id AVPlayerInterstitialEventController) where
  toAVPlayerInterstitialEventMonitor = unsafeCastId

instance IsNSObject (Id AVPlayerInterstitialEventController) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemMetadataCollector ----------

-- | AVPlayerItemMetadataCollector
--
-- A subclass of AVPlayerItemMediaDataCollector that provides AVMetadataGroups for an AVPlayerItem.
--
-- This class can be used to inform clients of the current set of AVMetadataGroups on an AVPlayerItem, and when new AVMetadataGroups become available - e.g. in a Live HLS stream.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemMetadataCollector@.
data AVPlayerItemMetadataCollector

instance IsObjCObject (Id AVPlayerItemMetadataCollector) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemMetadataCollector"

class IsAVPlayerItemMediaDataCollector a => IsAVPlayerItemMetadataCollector a where
  toAVPlayerItemMetadataCollector :: a -> Id AVPlayerItemMetadataCollector

instance IsAVPlayerItemMetadataCollector (Id AVPlayerItemMetadataCollector) where
  toAVPlayerItemMetadataCollector = unsafeCastId

instance IsAVPlayerItemMediaDataCollector (Id AVPlayerItemMetadataCollector) where
  toAVPlayerItemMediaDataCollector = unsafeCastId

instance IsNSObject (Id AVPlayerItemMetadataCollector) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemLegibleOutput ----------

-- | AVPlayerItemLegibleOutput
--
-- A subclass of AVPlayerItemOutput that can vend media with a legible characteristic as NSAttributedStrings.
--
-- An instance of AVPlayerItemLegibleOutput is typically initialized using the -init method.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemLegibleOutput@.
data AVPlayerItemLegibleOutput

instance IsObjCObject (Id AVPlayerItemLegibleOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemLegibleOutput"

class IsAVPlayerItemOutput a => IsAVPlayerItemLegibleOutput a where
  toAVPlayerItemLegibleOutput :: a -> Id AVPlayerItemLegibleOutput

instance IsAVPlayerItemLegibleOutput (Id AVPlayerItemLegibleOutput) where
  toAVPlayerItemLegibleOutput = unsafeCastId

instance IsAVPlayerItemOutput (Id AVPlayerItemLegibleOutput) where
  toAVPlayerItemOutput = unsafeCastId

instance IsNSObject (Id AVPlayerItemLegibleOutput) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemMetadataOutput ----------

-- | AVPlayerItemMetadataOutput
--
-- A subclass of AVPlayerItemOutput that vends collections of metadata items carried in metadata tracks.
--
-- Setting the value of suppressesPlayerRendering on an instance of AVPlayerItemMetadataOutput has no effect.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemMetadataOutput@.
data AVPlayerItemMetadataOutput

instance IsObjCObject (Id AVPlayerItemMetadataOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemMetadataOutput"

class IsAVPlayerItemOutput a => IsAVPlayerItemMetadataOutput a where
  toAVPlayerItemMetadataOutput :: a -> Id AVPlayerItemMetadataOutput

instance IsAVPlayerItemMetadataOutput (Id AVPlayerItemMetadataOutput) where
  toAVPlayerItemMetadataOutput = unsafeCastId

instance IsAVPlayerItemOutput (Id AVPlayerItemMetadataOutput) where
  toAVPlayerItemOutput = unsafeCastId

instance IsNSObject (Id AVPlayerItemMetadataOutput) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemRenderedLegibleOutput ----------

-- | AVPlayerItemRenderedLegibleOutput
--
-- A subclass of AVPlayerItemOutput that can vend media with a legible characteristic as rendered CVPixelBufferRefs.
--
-- An instance of AVPlayerItemRenderedLegibleOutput is initialized using the -init method.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVPlayerItemRenderedLegibleOutput@.
data AVPlayerItemRenderedLegibleOutput

instance IsObjCObject (Id AVPlayerItemRenderedLegibleOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemRenderedLegibleOutput"

class IsAVPlayerItemOutput a => IsAVPlayerItemRenderedLegibleOutput a where
  toAVPlayerItemRenderedLegibleOutput :: a -> Id AVPlayerItemRenderedLegibleOutput

instance IsAVPlayerItemRenderedLegibleOutput (Id AVPlayerItemRenderedLegibleOutput) where
  toAVPlayerItemRenderedLegibleOutput = unsafeCastId

instance IsAVPlayerItemOutput (Id AVPlayerItemRenderedLegibleOutput) where
  toAVPlayerItemOutput = unsafeCastId

instance IsNSObject (Id AVPlayerItemRenderedLegibleOutput) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerItemVideoOutput ----------

-- | Phantom type for @AVPlayerItemVideoOutput@.
data AVPlayerItemVideoOutput

instance IsObjCObject (Id AVPlayerItemVideoOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerItemVideoOutput"

class IsAVPlayerItemOutput a => IsAVPlayerItemVideoOutput a where
  toAVPlayerItemVideoOutput :: a -> Id AVPlayerItemVideoOutput

instance IsAVPlayerItemVideoOutput (Id AVPlayerItemVideoOutput) where
  toAVPlayerItemVideoOutput = unsafeCastId

instance IsAVPlayerItemOutput (Id AVPlayerItemVideoOutput) where
  toAVPlayerItemOutput = unsafeCastId

instance IsNSObject (Id AVPlayerItemVideoOutput) where
  toNSObject = unsafeCastId

-- ---------- AVMutableVideoComposition ----------

-- | Phantom type for @AVMutableVideoComposition@.
data AVMutableVideoComposition

instance IsObjCObject (Id AVMutableVideoComposition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableVideoComposition"

class IsAVVideoComposition a => IsAVMutableVideoComposition a where
  toAVMutableVideoComposition :: a -> Id AVMutableVideoComposition

instance IsAVMutableVideoComposition (Id AVMutableVideoComposition) where
  toAVMutableVideoComposition = unsafeCastId

instance IsAVVideoComposition (Id AVMutableVideoComposition) where
  toAVVideoComposition = unsafeCastId

instance IsNSObject (Id AVMutableVideoComposition) where
  toNSObject = unsafeCastId

-- ---------- AVMutableVideoCompositionInstruction ----------

-- | Phantom type for @AVMutableVideoCompositionInstruction@.
data AVMutableVideoCompositionInstruction

instance IsObjCObject (Id AVMutableVideoCompositionInstruction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableVideoCompositionInstruction"

class IsAVVideoCompositionInstruction a => IsAVMutableVideoCompositionInstruction a where
  toAVMutableVideoCompositionInstruction :: a -> Id AVMutableVideoCompositionInstruction

instance IsAVMutableVideoCompositionInstruction (Id AVMutableVideoCompositionInstruction) where
  toAVMutableVideoCompositionInstruction = unsafeCastId

instance IsAVVideoCompositionInstruction (Id AVMutableVideoCompositionInstruction) where
  toAVVideoCompositionInstruction = unsafeCastId

instance IsNSObject (Id AVMutableVideoCompositionInstruction) where
  toNSObject = unsafeCastId

-- ---------- AVMutableVideoCompositionLayerInstruction ----------

-- | Phantom type for @AVMutableVideoCompositionLayerInstruction@.
data AVMutableVideoCompositionLayerInstruction

instance IsObjCObject (Id AVMutableVideoCompositionLayerInstruction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableVideoCompositionLayerInstruction"

class IsAVVideoCompositionLayerInstruction a => IsAVMutableVideoCompositionLayerInstruction a where
  toAVMutableVideoCompositionLayerInstruction :: a -> Id AVMutableVideoCompositionLayerInstruction

instance IsAVMutableVideoCompositionLayerInstruction (Id AVMutableVideoCompositionLayerInstruction) where
  toAVMutableVideoCompositionLayerInstruction = unsafeCastId

instance IsAVVideoCompositionLayerInstruction (Id AVMutableVideoCompositionLayerInstruction) where
  toAVVideoCompositionLayerInstruction = unsafeCastId

instance IsNSObject (Id AVMutableVideoCompositionLayerInstruction) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureVideoPreviewLayer ----------

-- | AVCaptureVideoPreviewLayer
--
-- A CoreAnimation layer subclass for previewing the visual output of an AVCaptureSession.
--
-- An AVCaptureVideoPreviewLayer instance is a subclass of CALayer and is therefore suitable for insertion in a layer hierarchy as part of a graphical interface. One creates an AVCaptureVideoPreviewLayer instance with the capture session to be previewed, using +layerWithSession: or -initWithSession:. Using the "videoGravity" property, one can influence how content is viewed relative to the layer bounds. On some hardware configurations, the orientation of the layer can be manipulated using \@"orientation" and \@"mirrored".
-- 
-- Phantom type for @AVCaptureVideoPreviewLayer@.
data AVCaptureVideoPreviewLayer

instance IsObjCObject (Id AVCaptureVideoPreviewLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureVideoPreviewLayer"

class IsCALayer a => IsAVCaptureVideoPreviewLayer a where
  toAVCaptureVideoPreviewLayer :: a -> Id AVCaptureVideoPreviewLayer

instance IsAVCaptureVideoPreviewLayer (Id AVCaptureVideoPreviewLayer) where
  toAVCaptureVideoPreviewLayer = unsafeCastId

instance IsCALayer (Id AVCaptureVideoPreviewLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id AVCaptureVideoPreviewLayer) where
  toNSObject = unsafeCastId

-- ---------- AVPlayerLayer ----------

-- | Phantom type for @AVPlayerLayer@.
data AVPlayerLayer

instance IsObjCObject (Id AVPlayerLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerLayer"

class IsCALayer a => IsAVPlayerLayer a where
  toAVPlayerLayer :: a -> Id AVPlayerLayer

instance IsAVPlayerLayer (Id AVPlayerLayer) where
  toAVPlayerLayer = unsafeCastId

instance IsCALayer (Id AVPlayerLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id AVPlayerLayer) where
  toNSObject = unsafeCastId

-- ---------- AVSampleBufferDisplayLayer ----------

-- | Phantom type for @AVSampleBufferDisplayLayer@.
data AVSampleBufferDisplayLayer

instance IsObjCObject (Id AVSampleBufferDisplayLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSampleBufferDisplayLayer"

class IsCALayer a => IsAVSampleBufferDisplayLayer a where
  toAVSampleBufferDisplayLayer :: a -> Id AVSampleBufferDisplayLayer

instance IsAVSampleBufferDisplayLayer (Id AVSampleBufferDisplayLayer) where
  toAVSampleBufferDisplayLayer = unsafeCastId

instance IsCALayer (Id AVSampleBufferDisplayLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id AVSampleBufferDisplayLayer) where
  toNSObject = unsafeCastId

-- ---------- AVSynchronizedLayer ----------

-- | Phantom type for @AVSynchronizedLayer@.
data AVSynchronizedLayer

instance IsObjCObject (Id AVSynchronizedLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVSynchronizedLayer"

class IsCALayer a => IsAVSynchronizedLayer a where
  toAVSynchronizedLayer :: a -> Id AVSynchronizedLayer

instance IsAVSynchronizedLayer (Id AVSynchronizedLayer) where
  toAVSynchronizedLayer = unsafeCastId

instance IsCALayer (Id AVSynchronizedLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id AVSynchronizedLayer) where
  toNSObject = unsafeCastId

-- ---------- AVAssetDownloadURLSession ----------

-- | A subclass of NSURLSession to support AVAssetDownloadTask.
-- 
-- Phantom type for @AVAssetDownloadURLSession@.
data AVAssetDownloadURLSession

instance IsObjCObject (Id AVAssetDownloadURLSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetDownloadURLSession"

class IsNSURLSession a => IsAVAssetDownloadURLSession a where
  toAVAssetDownloadURLSession :: a -> Id AVAssetDownloadURLSession

instance IsAVAssetDownloadURLSession (Id AVAssetDownloadURLSession) where
  toAVAssetDownloadURLSession = unsafeCastId

instance IsNSObject (Id AVAssetDownloadURLSession) where
  toNSObject = unsafeCastId

instance IsNSURLSession (Id AVAssetDownloadURLSession) where
  toNSURLSession = unsafeCastId

-- ---------- AVAggregateAssetDownloadTask ----------

-- | An AVAssetDownloadTask used for downloading multiple AVMediaSelections for a single AVAsset, under the umbrella of a single download task.
--
-- Should be created with -[AVAssetDownloadURLSession aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:. For progress tracking, monitor the delegate callbacks for each childAssetDownloadTask.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVAggregateAssetDownloadTask@.
data AVAggregateAssetDownloadTask

instance IsObjCObject (Id AVAggregateAssetDownloadTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAggregateAssetDownloadTask"

class IsNSURLSessionTask a => IsAVAggregateAssetDownloadTask a where
  toAVAggregateAssetDownloadTask :: a -> Id AVAggregateAssetDownloadTask

instance IsAVAggregateAssetDownloadTask (Id AVAggregateAssetDownloadTask) where
  toAVAggregateAssetDownloadTask = unsafeCastId

instance IsNSObject (Id AVAggregateAssetDownloadTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionTask (Id AVAggregateAssetDownloadTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- AVAssetDownloadTask ----------

-- | A NSURLSessionTask that accepts remote AVURLAssets to download locally.
--
-- Should be created with -[AVAssetDownloadURLSession assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:]. To utilize local data for playback for downloads that are in-progress, re-use the URLAsset supplied in initialization. An AVAssetDownloadTask may be instantiated with a destinationURL pointing to an existing asset on disk, for the purpose of completing or augmenting a downloaded asset.
-- 
-- Phantom type for @AVAssetDownloadTask@.
data AVAssetDownloadTask

instance IsObjCObject (Id AVAssetDownloadTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVAssetDownloadTask"

class IsNSURLSessionTask a => IsAVAssetDownloadTask a where
  toAVAssetDownloadTask :: a -> Id AVAssetDownloadTask

instance IsAVAssetDownloadTask (Id AVAssetDownloadTask) where
  toAVAssetDownloadTask = unsafeCastId

instance IsNSObject (Id AVAssetDownloadTask) where
  toNSObject = unsafeCastId

instance IsNSURLSessionTask (Id AVAssetDownloadTask) where
  toNSURLSessionTask = unsafeCastId

-- ---------- AVMutableComposition ----------

-- | Phantom type for @AVMutableComposition@.
data AVMutableComposition

instance IsObjCObject (Id AVMutableComposition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableComposition"

class IsAVComposition a => IsAVMutableComposition a where
  toAVMutableComposition :: a -> Id AVMutableComposition

instance IsAVMutableComposition (Id AVMutableComposition) where
  toAVMutableComposition = unsafeCastId

instance IsAVAsset (Id AVMutableComposition) where
  toAVAsset = unsafeCastId

instance IsAVComposition (Id AVMutableComposition) where
  toAVComposition = unsafeCastId

instance IsNSObject (Id AVMutableComposition) where
  toNSObject = unsafeCastId

-- ---------- AVFragmentedMovie ----------

-- | Phantom type for @AVFragmentedMovie@.
data AVFragmentedMovie

instance IsObjCObject (Id AVFragmentedMovie) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFragmentedMovie"

class IsAVMovie a => IsAVFragmentedMovie a where
  toAVFragmentedMovie :: a -> Id AVFragmentedMovie

instance IsAVFragmentedMovie (Id AVFragmentedMovie) where
  toAVFragmentedMovie = unsafeCastId

instance IsAVAsset (Id AVFragmentedMovie) where
  toAVAsset = unsafeCastId

instance IsAVMovie (Id AVFragmentedMovie) where
  toAVMovie = unsafeCastId

instance IsNSObject (Id AVFragmentedMovie) where
  toNSObject = unsafeCastId

-- ---------- AVMutableMovie ----------

-- | AVMutableMovie
--
-- AVMutableMovie adds to its immutable superclass, AVMovie, several categories of methods for editing QuickTime movie files, e.g. inserting and removing time ranges of media, adding and removing tracks, and modifying the metadata collections stored therein.
--
-- By default, after creating an AVMutableMovie the defaultMediaDataStorage property will be nil and each associated AVMutableMovieTrack's mediaDataStorage property will be nil. If you want to create an AVMutableMovie from a file and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
-- 
-- Phantom type for @AVMutableMovie@.
data AVMutableMovie

instance IsObjCObject (Id AVMutableMovie) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableMovie"

class IsAVMovie a => IsAVMutableMovie a where
  toAVMutableMovie :: a -> Id AVMutableMovie

instance IsAVMutableMovie (Id AVMutableMovie) where
  toAVMutableMovie = unsafeCastId

instance IsAVAsset (Id AVMutableMovie) where
  toAVAsset = unsafeCastId

instance IsAVMovie (Id AVMutableMovie) where
  toAVMovie = unsafeCastId

instance IsNSObject (Id AVMutableMovie) where
  toNSObject = unsafeCastId

-- ---------- AVFragmentedAsset ----------

-- | A subclass of AVURLAsset that represents media resources that can be extended in total duration without modifying previously existing data structures.
--
-- Such media resources include QuickTime movie files and MPEG-4 files that indicate, via an 'mvex' box in their 'moov' box, that they accommodate additional fragments. Media resources of other types may also be supported. To check whether a given instance of AVFragmentedAsset can be used to monitor the addition of fragments, check the value of the AVURLAsset property canContainFragments.
--
-- An AVFragmentedAsset is capable of changing the values of certain of its properties and those of its tracks, while an operation that appends fragments to the underlying media resource in in progress, if the AVFragmentedAsset is associated with an instance of AVFragmentedAssetMinder.
--
-- While associated with an AVFragmentedAssetMinder, AVFragmentedAsset posts AVAssetDurationDidChangeNotification whenever new fragments are detected, as appropriate. It may also post AVAssetContainsFragmentsDidChangeNotification and AVAssetWasDefragmentedNotification, as discussed in documentation of those notifications. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVFragmentedAsset@.
data AVFragmentedAsset

instance IsObjCObject (Id AVFragmentedAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFragmentedAsset"

class IsAVURLAsset a => IsAVFragmentedAsset a where
  toAVFragmentedAsset :: a -> Id AVFragmentedAsset

instance IsAVFragmentedAsset (Id AVFragmentedAsset) where
  toAVFragmentedAsset = unsafeCastId

instance IsAVAsset (Id AVFragmentedAsset) where
  toAVAsset = unsafeCastId

instance IsAVURLAsset (Id AVFragmentedAsset) where
  toAVURLAsset = unsafeCastId

instance IsNSObject (Id AVFragmentedAsset) where
  toNSObject = unsafeCastId

-- ---------- AVMutableCompositionTrack ----------

-- | Phantom type for @AVMutableCompositionTrack@.
data AVMutableCompositionTrack

instance IsObjCObject (Id AVMutableCompositionTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableCompositionTrack"

class IsAVCompositionTrack a => IsAVMutableCompositionTrack a where
  toAVMutableCompositionTrack :: a -> Id AVMutableCompositionTrack

instance IsAVMutableCompositionTrack (Id AVMutableCompositionTrack) where
  toAVMutableCompositionTrack = unsafeCastId

instance IsAVAssetTrack (Id AVMutableCompositionTrack) where
  toAVAssetTrack = unsafeCastId

instance IsAVCompositionTrack (Id AVMutableCompositionTrack) where
  toAVCompositionTrack = unsafeCastId

instance IsNSObject (Id AVMutableCompositionTrack) where
  toNSObject = unsafeCastId

-- ---------- AVFragmentedMovieTrack ----------

-- | Phantom type for @AVFragmentedMovieTrack@.
data AVFragmentedMovieTrack

instance IsObjCObject (Id AVFragmentedMovieTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVFragmentedMovieTrack"

class IsAVMovieTrack a => IsAVFragmentedMovieTrack a where
  toAVFragmentedMovieTrack :: a -> Id AVFragmentedMovieTrack

instance IsAVFragmentedMovieTrack (Id AVFragmentedMovieTrack) where
  toAVFragmentedMovieTrack = unsafeCastId

instance IsAVAssetTrack (Id AVFragmentedMovieTrack) where
  toAVAssetTrack = unsafeCastId

instance IsAVMovieTrack (Id AVFragmentedMovieTrack) where
  toAVMovieTrack = unsafeCastId

instance IsNSObject (Id AVFragmentedMovieTrack) where
  toNSObject = unsafeCastId

-- ---------- AVMutableMovieTrack ----------

-- | Phantom type for @AVMutableMovieTrack@.
data AVMutableMovieTrack

instance IsObjCObject (Id AVMutableMovieTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableMovieTrack"

class IsAVMovieTrack a => IsAVMutableMovieTrack a where
  toAVMutableMovieTrack :: a -> Id AVMutableMovieTrack

instance IsAVMutableMovieTrack (Id AVMutableMovieTrack) where
  toAVMutableMovieTrack = unsafeCastId

instance IsAVAssetTrack (Id AVMutableMovieTrack) where
  toAVAssetTrack = unsafeCastId

instance IsAVMovieTrack (Id AVMutableMovieTrack) where
  toAVMovieTrack = unsafeCastId

instance IsNSObject (Id AVMutableMovieTrack) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureAudioFileOutput ----------

-- | AVCaptureAudioFileOutput
--
-- AVCaptureAudioFileOutput is a concrete subclass of AVCaptureFileOutput that writes captured audio to any audio file type supported by CoreAudio.
--
-- AVCaptureAudioFileOutput implements the complete file recording interface declared by AVCaptureFileOutput for writing media data to audio files. In addition, instances of AVCaptureAudioFileOutput allow clients to configure options specific to the audio file formats, including allowing them to write metadata collections to each file and specify audio encoding options.
-- 
-- Phantom type for @AVCaptureAudioFileOutput@.
data AVCaptureAudioFileOutput

instance IsObjCObject (Id AVCaptureAudioFileOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureAudioFileOutput"

class IsAVCaptureFileOutput a => IsAVCaptureAudioFileOutput a where
  toAVCaptureAudioFileOutput :: a -> Id AVCaptureAudioFileOutput

instance IsAVCaptureAudioFileOutput (Id AVCaptureAudioFileOutput) where
  toAVCaptureAudioFileOutput = unsafeCastId

instance IsAVCaptureFileOutput (Id AVCaptureAudioFileOutput) where
  toAVCaptureFileOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureAudioFileOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureAudioFileOutput) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureMovieFileOutput ----------

-- | AVCaptureMovieFileOutput
--
-- AVCaptureMovieFileOutput is a concrete subclass of AVCaptureFileOutput that writes captured media to QuickTime movie files.
--
-- AVCaptureMovieFileOutput implements the complete file recording interface declared by AVCaptureFileOutput for writing media data to QuickTime movie files. In addition, instances of AVCaptureMovieFileOutput allow clients to configure options specific to the QuickTime file format, including allowing them to write metadata collections to each file, specify media encoding options for each track (macOS), and specify an interval at which movie fragments should be written.
-- 
-- Phantom type for @AVCaptureMovieFileOutput@.
data AVCaptureMovieFileOutput

instance IsObjCObject (Id AVCaptureMovieFileOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureMovieFileOutput"

class IsAVCaptureFileOutput a => IsAVCaptureMovieFileOutput a where
  toAVCaptureMovieFileOutput :: a -> Id AVCaptureMovieFileOutput

instance IsAVCaptureMovieFileOutput (Id AVCaptureMovieFileOutput) where
  toAVCaptureMovieFileOutput = unsafeCastId

instance IsAVCaptureFileOutput (Id AVCaptureMovieFileOutput) where
  toAVCaptureFileOutput = unsafeCastId

instance IsAVCaptureOutput (Id AVCaptureMovieFileOutput) where
  toAVCaptureOutput = unsafeCastId

instance IsNSObject (Id AVCaptureMovieFileOutput) where
  toNSObject = unsafeCastId

-- ---------- AVMutableDateRangeMetadataGroup ----------

-- | AVMutableDateRangeMetadataGroup
--
-- AVMutableDateRangeMetadataGroup is used to represent a mutable collection of metadata items that are valid for use within a specific range of dates.
-- 
-- Phantom type for @AVMutableDateRangeMetadataGroup@.
data AVMutableDateRangeMetadataGroup

instance IsObjCObject (Id AVMutableDateRangeMetadataGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableDateRangeMetadataGroup"

class IsAVDateRangeMetadataGroup a => IsAVMutableDateRangeMetadataGroup a where
  toAVMutableDateRangeMetadataGroup :: a -> Id AVMutableDateRangeMetadataGroup

instance IsAVMutableDateRangeMetadataGroup (Id AVMutableDateRangeMetadataGroup) where
  toAVMutableDateRangeMetadataGroup = unsafeCastId

instance IsAVDateRangeMetadataGroup (Id AVMutableDateRangeMetadataGroup) where
  toAVDateRangeMetadataGroup = unsafeCastId

instance IsAVMetadataGroup (Id AVMutableDateRangeMetadataGroup) where
  toAVMetadataGroup = unsafeCastId

instance IsNSObject (Id AVMutableDateRangeMetadataGroup) where
  toNSObject = unsafeCastId

-- ---------- AVMutableTimedMetadataGroup ----------

-- | AVMutableTimedMetadataGroup
--
-- AVMutableTimedMetadataGroup is used to represent a mutable collection of metadata items that are valid for use during a specific range of time.
-- 
-- Phantom type for @AVMutableTimedMetadataGroup@.
data AVMutableTimedMetadataGroup

instance IsObjCObject (Id AVMutableTimedMetadataGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMutableTimedMetadataGroup"

class IsAVTimedMetadataGroup a => IsAVMutableTimedMetadataGroup a where
  toAVMutableTimedMetadataGroup :: a -> Id AVMutableTimedMetadataGroup

instance IsAVMutableTimedMetadataGroup (Id AVMutableTimedMetadataGroup) where
  toAVMutableTimedMetadataGroup = unsafeCastId

instance IsAVMetadataGroup (Id AVMutableTimedMetadataGroup) where
  toAVMetadataGroup = unsafeCastId

instance IsAVTimedMetadataGroup (Id AVMutableTimedMetadataGroup) where
  toAVTimedMetadataGroup = unsafeCastId

instance IsNSObject (Id AVMutableTimedMetadataGroup) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataCatBodyObject ----------

-- | AVMetadataCatBodyObject
--
-- AVMetadataCatBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected cat body.
--
-- AVMetadataCatBodyObject represents a single detected cat body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected cat body objects. See AVCaptureOutput.h.
-- 
-- Phantom type for @AVMetadataCatBodyObject@.
data AVMetadataCatBodyObject

instance IsObjCObject (Id AVMetadataCatBodyObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataCatBodyObject"

class IsAVMetadataBodyObject a => IsAVMetadataCatBodyObject a where
  toAVMetadataCatBodyObject :: a -> Id AVMetadataCatBodyObject

instance IsAVMetadataCatBodyObject (Id AVMetadataCatBodyObject) where
  toAVMetadataCatBodyObject = unsafeCastId

instance IsAVMetadataBodyObject (Id AVMetadataCatBodyObject) where
  toAVMetadataBodyObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataCatBodyObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataCatBodyObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataDogBodyObject ----------

-- | AVMetadataDogBodyObject
--
-- AVMetadataDogBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected dog body.
--
-- AVMetadataDogBodyObject represents a single detected dog body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected dog body objects. See AVCaptureOutput.h.
-- 
-- Phantom type for @AVMetadataDogBodyObject@.
data AVMetadataDogBodyObject

instance IsObjCObject (Id AVMetadataDogBodyObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataDogBodyObject"

class IsAVMetadataBodyObject a => IsAVMetadataDogBodyObject a where
  toAVMetadataDogBodyObject :: a -> Id AVMetadataDogBodyObject

instance IsAVMetadataDogBodyObject (Id AVMetadataDogBodyObject) where
  toAVMetadataDogBodyObject = unsafeCastId

instance IsAVMetadataBodyObject (Id AVMetadataDogBodyObject) where
  toAVMetadataBodyObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataDogBodyObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataDogBodyObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataHumanBodyObject ----------

-- | AVMetadataHumanBodyObject
--
-- AVMetadataHumanBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected human body.
--
-- AVMetadataHumanBodyObject represents a single detected human body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected human body objects. See AVCaptureOutput.h.
-- 
-- Phantom type for @AVMetadataHumanBodyObject@.
data AVMetadataHumanBodyObject

instance IsObjCObject (Id AVMetadataHumanBodyObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataHumanBodyObject"

class IsAVMetadataBodyObject a => IsAVMetadataHumanBodyObject a where
  toAVMetadataHumanBodyObject :: a -> Id AVMetadataHumanBodyObject

instance IsAVMetadataHumanBodyObject (Id AVMetadataHumanBodyObject) where
  toAVMetadataHumanBodyObject = unsafeCastId

instance IsAVMetadataBodyObject (Id AVMetadataHumanBodyObject) where
  toAVMetadataBodyObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataHumanBodyObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataHumanBodyObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetadataHumanFullBodyObject ----------

-- | AVMetadataHumanFullBodyObject
--
-- AVMetadataHumanFullBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected human full body.
--
-- AVMetadataHumanFullBodyObject represents a single detected human full body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected human full body objects. See AVCaptureOutput.h.
-- 
-- Phantom type for @AVMetadataHumanFullBodyObject@.
data AVMetadataHumanFullBodyObject

instance IsObjCObject (Id AVMetadataHumanFullBodyObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetadataHumanFullBodyObject"

class IsAVMetadataBodyObject a => IsAVMetadataHumanFullBodyObject a where
  toAVMetadataHumanFullBodyObject :: a -> Id AVMetadataHumanFullBodyObject

instance IsAVMetadataHumanFullBodyObject (Id AVMetadataHumanFullBodyObject) where
  toAVMetadataHumanFullBodyObject = unsafeCastId

instance IsAVMetadataBodyObject (Id AVMetadataHumanFullBodyObject) where
  toAVMetadataBodyObject = unsafeCastId

instance IsAVMetadataObject (Id AVMetadataHumanFullBodyObject) where
  toAVMetadataObject = unsafeCastId

instance IsNSObject (Id AVMetadataHumanFullBodyObject) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemInitialLikelyToKeepUpEvent ----------

-- | Represents a metric event when playback was first likely to play through without stalling.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemInitialLikelyToKeepUpEvent@.
data AVMetricPlayerItemInitialLikelyToKeepUpEvent

instance IsObjCObject (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemInitialLikelyToKeepUpEvent"

class IsAVMetricPlayerItemLikelyToKeepUpEvent a => IsAVMetricPlayerItemInitialLikelyToKeepUpEvent a where
  toAVMetricPlayerItemInitialLikelyToKeepUpEvent :: a -> Id AVMetricPlayerItemInitialLikelyToKeepUpEvent

instance IsAVMetricPlayerItemInitialLikelyToKeepUpEvent (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent) where
  toAVMetricPlayerItemInitialLikelyToKeepUpEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent) where
  toAVMetricEvent = unsafeCastId

instance IsAVMetricPlayerItemLikelyToKeepUpEvent (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent) where
  toAVMetricPlayerItemLikelyToKeepUpEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemInitialLikelyToKeepUpEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemSeekDidCompleteEvent ----------

-- | Represents a metric event when playback seek completed.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemSeekDidCompleteEvent@.
data AVMetricPlayerItemSeekDidCompleteEvent

instance IsObjCObject (Id AVMetricPlayerItemSeekDidCompleteEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemSeekDidCompleteEvent"

class IsAVMetricPlayerItemRateChangeEvent a => IsAVMetricPlayerItemSeekDidCompleteEvent a where
  toAVMetricPlayerItemSeekDidCompleteEvent :: a -> Id AVMetricPlayerItemSeekDidCompleteEvent

instance IsAVMetricPlayerItemSeekDidCompleteEvent (Id AVMetricPlayerItemSeekDidCompleteEvent) where
  toAVMetricPlayerItemSeekDidCompleteEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemSeekDidCompleteEvent) where
  toAVMetricEvent = unsafeCastId

instance IsAVMetricPlayerItemRateChangeEvent (Id AVMetricPlayerItemSeekDidCompleteEvent) where
  toAVMetricPlayerItemRateChangeEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemSeekDidCompleteEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemSeekEvent ----------

-- | Represents a metric event when playback seeked.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemSeekEvent@.
data AVMetricPlayerItemSeekEvent

instance IsObjCObject (Id AVMetricPlayerItemSeekEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemSeekEvent"

class IsAVMetricPlayerItemRateChangeEvent a => IsAVMetricPlayerItemSeekEvent a where
  toAVMetricPlayerItemSeekEvent :: a -> Id AVMetricPlayerItemSeekEvent

instance IsAVMetricPlayerItemSeekEvent (Id AVMetricPlayerItemSeekEvent) where
  toAVMetricPlayerItemSeekEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemSeekEvent) where
  toAVMetricEvent = unsafeCastId

instance IsAVMetricPlayerItemRateChangeEvent (Id AVMetricPlayerItemSeekEvent) where
  toAVMetricPlayerItemRateChangeEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemSeekEvent) where
  toNSObject = unsafeCastId

-- ---------- AVMetricPlayerItemStallEvent ----------

-- | Represents a metric event when playback stalled.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
-- 
-- Phantom type for @AVMetricPlayerItemStallEvent@.
data AVMetricPlayerItemStallEvent

instance IsObjCObject (Id AVMetricPlayerItemStallEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVMetricPlayerItemStallEvent"

class IsAVMetricPlayerItemRateChangeEvent a => IsAVMetricPlayerItemStallEvent a where
  toAVMetricPlayerItemStallEvent :: a -> Id AVMetricPlayerItemStallEvent

instance IsAVMetricPlayerItemStallEvent (Id AVMetricPlayerItemStallEvent) where
  toAVMetricPlayerItemStallEvent = unsafeCastId

instance IsAVMetricEvent (Id AVMetricPlayerItemStallEvent) where
  toAVMetricEvent = unsafeCastId

instance IsAVMetricPlayerItemRateChangeEvent (Id AVMetricPlayerItemStallEvent) where
  toAVMetricPlayerItemRateChangeEvent = unsafeCastId

instance IsNSObject (Id AVMetricPlayerItemStallEvent) where
  toNSObject = unsafeCastId
