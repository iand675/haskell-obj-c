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
  , options
  , loadedTimeRanges
  , initSelector
  , newSelector
  , urlAssetSelector
  , optionsSelector
  , loadedTimeRangesSelector


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

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

