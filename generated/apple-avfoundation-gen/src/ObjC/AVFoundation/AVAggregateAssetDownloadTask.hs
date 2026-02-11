{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVAssetDownloadTask used for downloading multiple AVMediaSelections for a single AVAsset, under the umbrella of a single download task.
--
-- Should be created with -[AVAssetDownloadURLSession aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:. For progress tracking, monitor the delegate callbacks for each childAssetDownloadTask.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAggregateAssetDownloadTask@.
module ObjC.AVFoundation.AVAggregateAssetDownloadTask
  ( AVAggregateAssetDownloadTask
  , IsAVAggregateAssetDownloadTask(..)
  , init_
  , new
  , urlAsset
  , originalRequest
  , currentRequest
  , response
  , initSelector
  , newSelector
  , urlAssetSelector
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
init_ :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO (Id AVAggregateAssetDownloadTask)
init_ avAggregateAssetDownloadTask  =
    sendMsg avAggregateAssetDownloadTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAggregateAssetDownloadTask)
new  =
  do
    cls' <- getRequiredClass "AVAggregateAssetDownloadTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The asset supplied to the download task upon initialization.
--
-- ObjC selector: @- URLAsset@
urlAsset :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO (Id AVURLAsset)
urlAsset avAggregateAssetDownloadTask  =
    sendMsg avAggregateAssetDownloadTask (mkSelector "URLAsset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- originalRequest@
originalRequest :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO RawId
originalRequest avAggregateAssetDownloadTask  =
    fmap (RawId . castPtr) $ sendMsg avAggregateAssetDownloadTask (mkSelector "originalRequest") (retPtr retVoid) []

-- | @- currentRequest@
currentRequest :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO RawId
currentRequest avAggregateAssetDownloadTask  =
    fmap (RawId . castPtr) $ sendMsg avAggregateAssetDownloadTask (mkSelector "currentRequest") (retPtr retVoid) []

-- | @- response@
response :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO RawId
response avAggregateAssetDownloadTask  =
    fmap (RawId . castPtr) $ sendMsg avAggregateAssetDownloadTask (mkSelector "response") (retPtr retVoid) []

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

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector
currentRequestSelector = mkSelector "currentRequest"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

