{-# LANGUAGE DataKinds #-}
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
  , currentRequestSelector
  , initSelector
  , newSelector
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
init_ :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO (Id AVAggregateAssetDownloadTask)
init_ avAggregateAssetDownloadTask =
  sendOwnedMessage avAggregateAssetDownloadTask initSelector

-- | @+ new@
new :: IO (Id AVAggregateAssetDownloadTask)
new  =
  do
    cls' <- getRequiredClass "AVAggregateAssetDownloadTask"
    sendOwnedClassMessage cls' newSelector

-- | The asset supplied to the download task upon initialization.
--
-- ObjC selector: @- URLAsset@
urlAsset :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO (Id AVURLAsset)
urlAsset avAggregateAssetDownloadTask =
  sendMessage avAggregateAssetDownloadTask urlAssetSelector

-- | @- originalRequest@
originalRequest :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO RawId
originalRequest avAggregateAssetDownloadTask =
  sendMessage avAggregateAssetDownloadTask originalRequestSelector

-- | @- currentRequest@
currentRequest :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO RawId
currentRequest avAggregateAssetDownloadTask =
  sendMessage avAggregateAssetDownloadTask currentRequestSelector

-- | @- response@
response :: IsAVAggregateAssetDownloadTask avAggregateAssetDownloadTask => avAggregateAssetDownloadTask -> IO RawId
response avAggregateAssetDownloadTask =
  sendMessage avAggregateAssetDownloadTask responseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAggregateAssetDownloadTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAggregateAssetDownloadTask)
newSelector = mkSelector "new"

-- | @Selector@ for @URLAsset@
urlAssetSelector :: Selector '[] (Id AVURLAsset)
urlAssetSelector = mkSelector "URLAsset"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector '[] RawId
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector '[] RawId
currentRequestSelector = mkSelector "currentRequest"

-- | @Selector@ for @response@
responseSelector :: Selector '[] RawId
responseSelector = mkSelector "response"

