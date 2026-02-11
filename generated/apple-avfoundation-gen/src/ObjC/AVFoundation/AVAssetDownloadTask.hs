{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A NSURLSessionTask that accepts remote AVURLAssets to download locally.
--
-- Should be created with -[AVAssetDownloadURLSession assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:]. To utilize local data for playback for downloads that are in-progress, re-use the URLAsset supplied in initialization. An AVAssetDownloadTask may be instantiated with a destinationURL pointing to an existing asset on disk, for the purpose of completing or augmenting a downloaded asset.
--
-- Generated bindings for @AVAssetDownloadTask@.
module ObjC.AVFoundation.AVAssetDownloadTask
  ( AVAssetDownloadTask
  , IsAVAssetDownloadTask(..)
  , init_
  , new
  , urlAsset
  , destinationURL
  , options
  , loadedTimeRanges
  , originalRequest
  , currentRequest
  , response
  , initSelector
  , newSelector
  , urlAssetSelector
  , destinationURLSelector
  , optionsSelector
  , loadedTimeRangesSelector
  , originalRequestSelector
  , currentRequestSelector
  , responseSelector


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

-- | @- init@
init_ :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id AVAssetDownloadTask)
init_ avAssetDownloadTask  =
    sendMsg avAssetDownloadTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetDownloadTask)
new  =
  do
    cls' <- getRequiredClass "AVAssetDownloadTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The asset supplied to the download task upon initialization.
--
-- ObjC selector: @- URLAsset@
urlAsset :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id AVURLAsset)
urlAsset avAssetDownloadTask  =
    sendMsg avAssetDownloadTask (mkSelector "URLAsset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The file URL supplied to the download task upon initialization.
--
-- This URL may have been appended with the appropriate extension for the asset.
--
-- ObjC selector: @- destinationURL@
destinationURL :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id NSURL)
destinationURL avAssetDownloadTask  =
    sendMsg avAssetDownloadTask (mkSelector "destinationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The options supplied to the download task upon initialization.
--
-- ObjC selector: @- options@
options :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id NSDictionary)
options avAssetDownloadTask  =
    sendMsg avAssetDownloadTask (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property provides a collection of time ranges for which the download task has media data already downloaded and playable. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id NSArray)
loadedTimeRanges avAssetDownloadTask  =
    sendMsg avAssetDownloadTask (mkSelector "loadedTimeRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- originalRequest@
originalRequest :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO RawId
originalRequest avAssetDownloadTask  =
    fmap (RawId . castPtr) $ sendMsg avAssetDownloadTask (mkSelector "originalRequest") (retPtr retVoid) []

-- | @- currentRequest@
currentRequest :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO RawId
currentRequest avAssetDownloadTask  =
    fmap (RawId . castPtr) $ sendMsg avAssetDownloadTask (mkSelector "currentRequest") (retPtr retVoid) []

-- | @- response@
response :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO RawId
response avAssetDownloadTask  =
    fmap (RawId . castPtr) $ sendMsg avAssetDownloadTask (mkSelector "response") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @URLAsset@
urlAssetSelector :: Selector
urlAssetSelector = mkSelector "URLAsset"

-- | @Selector@ for @destinationURL@
destinationURLSelector :: Selector
destinationURLSelector = mkSelector "destinationURL"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector
currentRequestSelector = mkSelector "currentRequest"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

