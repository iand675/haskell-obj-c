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
  , initSelector
  , newSelector
  , assetPlaybackAssistantWithAssetSelector


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
init_ :: IsAVAssetPlaybackAssistant avAssetPlaybackAssistant => avAssetPlaybackAssistant -> IO (Id AVAssetPlaybackAssistant)
init_ avAssetPlaybackAssistant  =
  sendMsg avAssetPlaybackAssistant (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetPlaybackAssistant)
new  =
  do
    cls' <- getRequiredClass "AVAssetPlaybackAssistant"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "assetPlaybackAssistantWithAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetPlaybackAssistantWithAsset:@
assetPlaybackAssistantWithAssetSelector :: Selector
assetPlaybackAssistantWithAssetSelector = mkSelector "assetPlaybackAssistantWithAsset:"

