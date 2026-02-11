{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @AVURLAsset@.
module ObjC.AVFoundation.AVURLAsset
  ( AVURLAsset
  , IsAVURLAsset(..)
  , init_
  , new
  , audiovisualTypes
  , audiovisualMIMETypes
  , isPlayableExtendedMIMEType
  , urlAssetWithURL_options
  , initWithURL_options
  , compatibleTrackForCompositionTrack
  , findCompatibleTrackForCompositionTrack_completionHandler
  , audiovisualContentTypes
  , url
  , httpSessionIdentifier
  , mayRequireContentKeysForMediaDataProcessing
  , mediaExtensionProperties
  , variants
  , initSelector
  , newSelector
  , audiovisualTypesSelector
  , audiovisualMIMETypesSelector
  , isPlayableExtendedMIMETypeSelector
  , urlAssetWithURL_optionsSelector
  , initWithURL_optionsSelector
  , compatibleTrackForCompositionTrackSelector
  , findCompatibleTrackForCompositionTrack_completionHandlerSelector
  , audiovisualContentTypesSelector
  , urlSelector
  , httpSessionIdentifierSelector
  , mayRequireContentKeysForMediaDataProcessingSelector
  , mediaExtensionPropertiesSelector
  , variantsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsAVURLAsset avurlAsset => avurlAsset -> IO (Id AVURLAsset)
init_ avurlAsset  =
  sendMsg avurlAsset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVURLAsset)
new  =
  do
    cls' <- getRequiredClass "AVURLAsset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Provides the file types the AVURLAsset class understands.
--
-- - Returns: An NSArray of UTIs identifying the file types the AVURLAsset class understands.
--
-- ObjC selector: @+ audiovisualTypes@
audiovisualTypes :: IO (Id NSArray)
audiovisualTypes  =
  do
    cls' <- getRequiredClass "AVURLAsset"
    sendClassMsg cls' (mkSelector "audiovisualTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the MIME types the AVURLAsset class understands.
--
-- - Returns: An NSArray of NSStrings containing MIME types the AVURLAsset class understands.
--
-- ObjC selector: @+ audiovisualMIMETypes@
audiovisualMIMETypes :: IO (Id NSArray)
audiovisualMIMETypes  =
  do
    cls' <- getRequiredClass "AVURLAsset"
    sendClassMsg cls' (mkSelector "audiovisualMIMETypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns YES if asset is playable with the codec(s) and container type specified in extendedMIMEType. Returns NO otherwise.
--
-- On releases prior to macOS 14, iOS 17, tvOS 17, and watchOS 10, regardless of the specified MIME type this method interprets all codecs parameters according to the ISO family syntax defined by RFC 6381 and evaluates playability according to whether the indicated codecs are supported when carried in container formats that conform to the ISO BMFF specification, such as the MPEG-4 file format. On releases starting with macOS 14, iOS 17, tvOS 17, and watchOS 10, this method interprets codecs parameters according to the syntax and namespace determined by the specified MIME type and evaluates playability according to whether the indicated codecs are supported when carried in the container format indicated by that MIME type. Codecs parameters for each of the following MIME types are supported: video/mp4 (per RFC 6381, ISO/IEC 14496-15 Annex E, et al), video/quicktime (RFC 6381 et al), video/mp2t (ISO/IEC 13818-1), audio/vnd.wave (RFC 2361), audio/aiff (using the CoreAudio AudioFormatID namespace), audio/x-caf (also using the CoreAudio AudioFormatID namespace), and audio/mpeg (e.g. codecs="mp3"). MIME types supported as alternatives for the same container formats, e.g audio/mp4, are equivalently treated. If the indicated MIME type defines no supported syntax and namespace for codecs parameters, when any codecs parameter is present this method returns NO.
--
-- - Parameter extendedMIMEType:
--
-- - Returns: YES or NO.
--
-- ObjC selector: @+ isPlayableExtendedMIMEType:@
isPlayableExtendedMIMEType :: IsNSString extendedMIMEType => extendedMIMEType -> IO Bool
isPlayableExtendedMIMEType extendedMIMEType =
  do
    cls' <- getRequiredClass "AVURLAsset"
    withObjCPtr extendedMIMEType $ \raw_extendedMIMEType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isPlayableExtendedMIMEType:") retCULong [argPtr (castPtr raw_extendedMIMEType :: Ptr ())]

-- | Returns an instance of AVURLAsset for inspection of a media resource.
--
-- - Parameter URL: An instance of NSURL that references a media resource. - Parameter options: An instance of NSDictionary that contains keys for specifying options for the initialization of the AVURLAsset. See AVURLAssetPreferPreciseDurationAndTimingKey and AVURLAssetReferenceRestrictionsKey above.
--
-- - Returns: An instance of AVURLAsset.
--
-- ObjC selector: @+ URLAssetWithURL:options:@
urlAssetWithURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id AVURLAsset)
urlAssetWithURL_options url options =
  do
    cls' <- getRequiredClass "AVURLAsset"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "URLAssetWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an instance of AVURLAsset for inspection of a media resource.
--
-- - Parameter URL: An instance of NSURL that references a media resource. - Parameter options: An instance of NSDictionary that contains keys for specifying options for the initialization of the AVURLAsset. See AVURLAssetPreferPreciseDurationAndTimingKey and AVURLAssetReferenceRestrictionsKey above.
--
-- - Returns: An instance of AVURLAsset.
--
-- ObjC selector: @- initWithURL:options:@
initWithURL_options :: (IsAVURLAsset avurlAsset, IsNSURL url, IsNSDictionary options) => avurlAsset -> url -> options -> IO (Id AVURLAsset)
initWithURL_options avurlAsset  url options =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
      sendMsg avurlAsset (mkSelector "initWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | Provides a reference to an AVAssetTrack of the target from which any timeRange can be inserted into a mutable composition track (via -[AVMutableCompositionTrack insertTimeRange:ofTrack:atTime:error:]).
--
-- Finds a track of the target with content that can be accommodated by the specified composition track. The logical complement of -[AVMutableComposition mutableTrackCompatibleWithTrack:].
--
-- - Parameter compositionTrack: The composition track for which a compatible AVAssetTrack is requested.
--
-- - Returns: an instance of AVAssetTrack
--
-- ObjC selector: @- compatibleTrackForCompositionTrack:@
compatibleTrackForCompositionTrack :: (IsAVURLAsset avurlAsset, IsAVCompositionTrack compositionTrack) => avurlAsset -> compositionTrack -> IO (Id AVAssetTrack)
compatibleTrackForCompositionTrack avurlAsset  compositionTrack =
withObjCPtr compositionTrack $ \raw_compositionTrack ->
    sendMsg avurlAsset (mkSelector "compatibleTrackForCompositionTrack:") (retPtr retVoid) [argPtr (castPtr raw_compositionTrack :: Ptr ())] >>= retainedObject . castPtr

-- | Loads a reference to an AVAssetTrack of the target from which any timeRange can be inserted into a mutable composition track (via -[AVMutableCompositionTrack insertTimeRange:ofTrack:atTime:error:]).
--
-- Finds a track of the target with content that can be accommodated by the specified composition track. The logical complement of -[AVMutableComposition mutableTrackCompatibleWithTrack:].
--
-- - Parameter compositionTrack: The composition track for which a compatible AVAssetTrack is requested. - Parameter completionHandler: A block that is invoked when loading is complete, vending an instance of AVAssetTrack or an error.
--
-- ObjC selector: @- findCompatibleTrackForCompositionTrack:completionHandler:@
findCompatibleTrackForCompositionTrack_completionHandler :: (IsAVURLAsset avurlAsset, IsAVCompositionTrack compositionTrack) => avurlAsset -> compositionTrack -> Ptr () -> IO ()
findCompatibleTrackForCompositionTrack_completionHandler avurlAsset  compositionTrack completionHandler =
withObjCPtr compositionTrack $ \raw_compositionTrack ->
    sendMsg avurlAsset (mkSelector "findCompatibleTrackForCompositionTrack:completionHandler:") retVoid [argPtr (castPtr raw_compositionTrack :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Provides the content types the AVURLAsset class understands.
--
-- - Returns: An NSArray of UTTypes identifying the content types the AVURLAsset class understands.
--
-- ObjC selector: @+ audiovisualContentTypes@
audiovisualContentTypes :: IO (Id NSArray)
audiovisualContentTypes  =
  do
    cls' <- getRequiredClass "AVURLAsset"
    sendClassMsg cls' (mkSelector "audiovisualContentTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the URL with which the instance of AVURLAsset was initialized.
--
-- ObjC selector: @- URL@
url :: IsAVURLAsset avurlAsset => avurlAsset -> IO (Id NSURL)
url avurlAsset  =
  sendMsg avurlAsset (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the identifier that's automatically included in any HTTP request issued on behalf of this asset in the HTTP header field "X-Playback-Session-Id".
--
-- The value is an NSUUID from which the UUID string can be obtained. Note that copies of an AVURLAsset vend an equivalent httpSessionIdentifier.
--
-- ObjC selector: @- httpSessionIdentifier@
httpSessionIdentifier :: IsAVURLAsset avurlAsset => avurlAsset -> IO (Id NSUUID)
httpSessionIdentifier avurlAsset  =
  sendMsg avurlAsset (mkSelector "httpSessionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Allows AVURLAsset to be added as a content key recipient to an AVContentKeySession.
--
-- ObjC selector: @- mayRequireContentKeysForMediaDataProcessing@
mayRequireContentKeysForMediaDataProcessing :: IsAVURLAsset avurlAsset => avurlAsset -> IO Bool
mayRequireContentKeysForMediaDataProcessing avurlAsset  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avurlAsset (mkSelector "mayRequireContentKeysForMediaDataProcessing") retCULong []

-- | The properties of the MediaExtension format reader for the asset.
--
-- If the asset is being decoded using a MediaExtension format reader, this property will return a AVMediaExtensionProperties object describing the extension. If the asset is not being decoded with a MediaExtension format reader, this property will return nil.
--
-- ObjC selector: @- mediaExtensionProperties@
mediaExtensionProperties :: IsAVURLAsset avurlAsset => avurlAsset -> IO (Id AVMediaExtensionProperties)
mediaExtensionProperties avurlAsset  =
  sendMsg avurlAsset (mkSelector "mediaExtensionProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of AVAssetVariants contained in the asset
--
-- Some variants may not be playable according to the current device configuration.
--
-- ObjC selector: @- variants@
variants :: IsAVURLAsset avurlAsset => avurlAsset -> IO (Id NSArray)
variants avurlAsset  =
  sendMsg avurlAsset (mkSelector "variants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @audiovisualTypes@
audiovisualTypesSelector :: Selector
audiovisualTypesSelector = mkSelector "audiovisualTypes"

-- | @Selector@ for @audiovisualMIMETypes@
audiovisualMIMETypesSelector :: Selector
audiovisualMIMETypesSelector = mkSelector "audiovisualMIMETypes"

-- | @Selector@ for @isPlayableExtendedMIMEType:@
isPlayableExtendedMIMETypeSelector :: Selector
isPlayableExtendedMIMETypeSelector = mkSelector "isPlayableExtendedMIMEType:"

-- | @Selector@ for @URLAssetWithURL:options:@
urlAssetWithURL_optionsSelector :: Selector
urlAssetWithURL_optionsSelector = mkSelector "URLAssetWithURL:options:"

-- | @Selector@ for @initWithURL:options:@
initWithURL_optionsSelector :: Selector
initWithURL_optionsSelector = mkSelector "initWithURL:options:"

-- | @Selector@ for @compatibleTrackForCompositionTrack:@
compatibleTrackForCompositionTrackSelector :: Selector
compatibleTrackForCompositionTrackSelector = mkSelector "compatibleTrackForCompositionTrack:"

-- | @Selector@ for @findCompatibleTrackForCompositionTrack:completionHandler:@
findCompatibleTrackForCompositionTrack_completionHandlerSelector :: Selector
findCompatibleTrackForCompositionTrack_completionHandlerSelector = mkSelector "findCompatibleTrackForCompositionTrack:completionHandler:"

-- | @Selector@ for @audiovisualContentTypes@
audiovisualContentTypesSelector :: Selector
audiovisualContentTypesSelector = mkSelector "audiovisualContentTypes"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @httpSessionIdentifier@
httpSessionIdentifierSelector :: Selector
httpSessionIdentifierSelector = mkSelector "httpSessionIdentifier"

-- | @Selector@ for @mayRequireContentKeysForMediaDataProcessing@
mayRequireContentKeysForMediaDataProcessingSelector :: Selector
mayRequireContentKeysForMediaDataProcessingSelector = mkSelector "mayRequireContentKeysForMediaDataProcessing"

-- | @Selector@ for @mediaExtensionProperties@
mediaExtensionPropertiesSelector :: Selector
mediaExtensionPropertiesSelector = mkSelector "mediaExtensionProperties"

-- | @Selector@ for @variants@
variantsSelector :: Selector
variantsSelector = mkSelector "variants"

