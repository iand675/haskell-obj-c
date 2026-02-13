{-# LANGUAGE DataKinds #-}
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
  , currentRequestSelector
  , destinationURLSelector
  , initSelector
  , loadedTimeRangesSelector
  , newSelector
  , optionsSelector
  , originalRequestSelector
  , responseSelector
  , urlAssetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id AVAssetDownloadTask)
init_ avAssetDownloadTask =
  sendOwnedMessage avAssetDownloadTask initSelector

-- | @+ new@
new :: IO (Id AVAssetDownloadTask)
new  =
  do
    cls' <- getRequiredClass "AVAssetDownloadTask"
    sendOwnedClassMessage cls' newSelector

-- | The asset supplied to the download task upon initialization.
--
-- ObjC selector: @- URLAsset@
urlAsset :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id AVURLAsset)
urlAsset avAssetDownloadTask =
  sendMessage avAssetDownloadTask urlAssetSelector

-- | The file URL supplied to the download task upon initialization.
--
-- This URL may have been appended with the appropriate extension for the asset.
--
-- ObjC selector: @- destinationURL@
destinationURL :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id NSURL)
destinationURL avAssetDownloadTask =
  sendMessage avAssetDownloadTask destinationURLSelector

-- | The options supplied to the download task upon initialization.
--
-- ObjC selector: @- options@
options :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id NSDictionary)
options avAssetDownloadTask =
  sendMessage avAssetDownloadTask optionsSelector

-- | This property provides a collection of time ranges for which the download task has media data already downloaded and playable. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO (Id NSArray)
loadedTimeRanges avAssetDownloadTask =
  sendMessage avAssetDownloadTask loadedTimeRangesSelector

-- | @- originalRequest@
originalRequest :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO RawId
originalRequest avAssetDownloadTask =
  sendMessage avAssetDownloadTask originalRequestSelector

-- | @- currentRequest@
currentRequest :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO RawId
currentRequest avAssetDownloadTask =
  sendMessage avAssetDownloadTask currentRequestSelector

-- | @- response@
response :: IsAVAssetDownloadTask avAssetDownloadTask => avAssetDownloadTask -> IO RawId
response avAssetDownloadTask =
  sendMessage avAssetDownloadTask responseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetDownloadTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetDownloadTask)
newSelector = mkSelector "new"

-- | @Selector@ for @URLAsset@
urlAssetSelector :: Selector '[] (Id AVURLAsset)
urlAssetSelector = mkSelector "URLAsset"

-- | @Selector@ for @destinationURL@
destinationURLSelector :: Selector '[] (Id NSURL)
destinationURLSelector = mkSelector "destinationURL"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSDictionary)
optionsSelector = mkSelector "options"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector '[] (Id NSArray)
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector '[] RawId
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector '[] RawId
currentRequestSelector = mkSelector "currentRequest"

-- | @Selector@ for @response@
responseSelector :: Selector '[] RawId
responseSelector = mkSelector "response"

