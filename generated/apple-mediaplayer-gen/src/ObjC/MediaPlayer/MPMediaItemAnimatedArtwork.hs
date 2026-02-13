{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An animated image, such as an animated music album cover art, for a media item.
--
-- A single instance of animated artwork is comprised of two assets: an artwork video asset, and a preview image which should match the first frame of the artwork video. The preview image may be used when displaying the animated artwork whilst the video becomes available.
--
-- Both the preview image and artwork video can be fetched asynchronously and will only be requested when required at point of display. Aim to provide preview images as quickly as possible once requested, and ideally synchronously.
--
-- Video asset @URL@s you provide must be local file @URL@s. You should make the associated assets available locally before providing them via the relevant handler, for example by fetching the associated video asset over the network. The @URL@s should remain valid for the lifetime of the ``MPMediaItemAnimatedArtwork``, once provided.
--
-- ``MPMediaItemAnimatedArtwork`` should not be subclassed.
--
-- Generated bindings for @MPMediaItemAnimatedArtwork@.
module ObjC.MediaPlayer.MPMediaItemAnimatedArtwork
  ( MPMediaItemAnimatedArtwork
  , IsMPMediaItemAnimatedArtwork(..)
  , new
  , init_
  , initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandler
  , initSelector
  , initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandlerSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMediaItemAnimatedArtwork)
new  =
  do
    cls' <- getRequiredClass "MPMediaItemAnimatedArtwork"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPMediaItemAnimatedArtwork mpMediaItemAnimatedArtwork => mpMediaItemAnimatedArtwork -> IO (Id MPMediaItemAnimatedArtwork)
init_ mpMediaItemAnimatedArtwork =
  sendOwnedMessage mpMediaItemAnimatedArtwork initSelector

-- | Creates an animated artwork.
--
-- - Parameters:   - artworkID: A unique identifier for this animated artwork. This identifier should encapsulate   the identity of both the preview frame and video asset. If you change either, you should   provide an ``MPMediaItemAnimatedArtwork`` with an updated @artworkID@.   - previewImageRequestHandler: A handler to return a preview image for this artwork, for the   requested @CGSize@ in pixels. Once requested, you should pass the preview image to the   provided completion handler, or you can pass @nil@ if the preview image cannot be resolved   for any reason. You can call the completion handler on an arbitrary queue, however it must   only be called once. The @NSImage@ you provide should ideally have a size equal to the   requested @CGSize@, however an image of the same aspect ratio is acceptable. Images that   diverge significantly from the requested aspect ratio may be rejected by the system. Aim to   provide preview images quickly and ideally synchronously, and if possible you should preload   these images in order to reduce perceived latency when displaying animated artwork to the   user.   - videoAssetFileURLRequestHandler: A handler to return a file @URL@ for the artwork video   asset for this artwork, for the requested @CGSize@ in pixels. Once requested, you should pass   the @URL@ to the provided completion handler, or you can pass @nil@ if the artwork video asset   cannot be resolved for any reason. You can call the completion handler on an arbitrary queue,   however it must only be called once. The @URL@ you provide must reference a local asset,   ideally with a size equal to the requested @CGSize@, however an asset with the same aspect   ratio is acceptable. Assets that diverge significantly from the requested aspect ratio may be   rejected by the system. The video assets you provide should loop cleanly, and should be   available relatively quickly from this handler (particularly when re-fetched). It’s advised   that assets are cached for subsequent fetches.
--
-- ObjC selector: @- initWithArtworkID:previewImageRequestHandler:videoAssetFileURLRequestHandler:@
initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandler :: (IsMPMediaItemAnimatedArtwork mpMediaItemAnimatedArtwork, IsNSString artworkID) => mpMediaItemAnimatedArtwork -> artworkID -> Ptr () -> Ptr () -> IO (Id MPMediaItemAnimatedArtwork)
initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandler mpMediaItemAnimatedArtwork artworkID previewImageRequestHandler videoAssetFileURLRequestHandler =
  sendOwnedMessage mpMediaItemAnimatedArtwork initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandlerSelector (toNSString artworkID) previewImageRequestHandler videoAssetFileURLRequestHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPMediaItemAnimatedArtwork)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPMediaItemAnimatedArtwork)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithArtworkID:previewImageRequestHandler:videoAssetFileURLRequestHandler:@
initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandlerSelector :: Selector '[Id NSString, Ptr (), Ptr ()] (Id MPMediaItemAnimatedArtwork)
initWithArtworkID_previewImageRequestHandler_videoAssetFileURLRequestHandlerSelector = mkSelector "initWithArtworkID:previewImageRequestHandler:videoAssetFileURLRequestHandler:"

