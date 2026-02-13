{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetPlaybackAssistant provides playback information for an asset.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetPlaybackAssistant@.
module ObjC.AVFoundation.AVAssetPlaybackAssistant
  ( AVAssetPlaybackAssistant
  , IsAVAssetPlaybackAssistant(..)
  , init_
  , new
  , assetPlaybackAssistantWithAsset
  , assetPlaybackAssistantWithAssetSelector
  , initSelector
  , newSelector


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
init_ :: IsAVAssetPlaybackAssistant avAssetPlaybackAssistant => avAssetPlaybackAssistant -> IO (Id AVAssetPlaybackAssistant)
init_ avAssetPlaybackAssistant =
  sendOwnedMessage avAssetPlaybackAssistant initSelector

-- | @+ new@
new :: IO (Id AVAssetPlaybackAssistant)
new  =
  do
    cls' <- getRequiredClass "AVAssetPlaybackAssistant"
    sendOwnedClassMessage cls' newSelector

-- | Returns an instance of AVAssetPlaybackAssistant for inspection of an AVAsset object.
--
-- - Parameter asset: An instance of AVAsset.
--
-- - Returns: An instance of AVAssetPlaybackAssistant.
--
-- ObjC selector: @+ assetPlaybackAssistantWithAsset:@
assetPlaybackAssistantWithAsset :: IsAVAsset asset => asset -> IO (Id AVAssetPlaybackAssistant)
assetPlaybackAssistantWithAsset asset =
  do
    cls' <- getRequiredClass "AVAssetPlaybackAssistant"
    sendClassMessage cls' assetPlaybackAssistantWithAssetSelector (toAVAsset asset)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetPlaybackAssistant)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetPlaybackAssistant)
newSelector = mkSelector "new"

-- | @Selector@ for @assetPlaybackAssistantWithAsset:@
assetPlaybackAssistantWithAssetSelector :: Selector '[Id AVAsset] (Id AVAssetPlaybackAssistant)
assetPlaybackAssistantWithAssetSelector = mkSelector "assetPlaybackAssistantWithAsset:"

