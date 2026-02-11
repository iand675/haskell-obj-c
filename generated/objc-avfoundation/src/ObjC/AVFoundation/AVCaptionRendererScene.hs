{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @AVCaptionRendererScene@.
module ObjC.AVFoundation.AVCaptionRendererScene
  ( AVCaptionRendererScene
  , IsAVCaptionRendererScene(..)
  , init_
  , new
  , hasActiveCaptions
  , needsPeriodicRefresh
  , initSelector
  , newSelector
  , hasActiveCaptionsSelector
  , needsPeriodicRefreshSelector


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
init_ :: IsAVCaptionRendererScene avCaptionRendererScene => avCaptionRendererScene -> IO (Id AVCaptionRendererScene)
init_ avCaptionRendererScene  =
  sendMsg avCaptionRendererScene (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionRendererScene)
new  =
  do
    cls' <- getRequiredClass "AVCaptionRendererScene"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | hasActiveCaptions
--
-- The scene contains one or more active captions.
--
-- Clients should not use this to restrict their drawing and should call renderInContext:atTime: to draw "emptiness". However, this information may be useful for purposes such as scrubbing to times where captions are present, skipping scenes in which no captions are present.
--
-- ObjC selector: @- hasActiveCaptions@
hasActiveCaptions :: IsAVCaptionRendererScene avCaptionRendererScene => avCaptionRendererScene -> IO Bool
hasActiveCaptions avCaptionRendererScene  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptionRendererScene (mkSelector "hasActiveCaptions") retCULong []

-- | needsPeriodicRefresh
--
-- The scene may have embedded animations or other state where periodic redrawing while playing through this scene is needed.
--
-- This property indicates if refreshing should occur if the client is progressing through the content. If the client is not progressing (i.e., it is treating playback as though the rate is 0.0), a single render at the current render time suffices. This property does not prescribe a refresh rate. A client is free to choose a refresh rate corresponding to rates of associated video frames or other timing appropriate for the client.
--
-- ObjC selector: @- needsPeriodicRefresh@
needsPeriodicRefresh :: IsAVCaptionRendererScene avCaptionRendererScene => avCaptionRendererScene -> IO Bool
needsPeriodicRefresh avCaptionRendererScene  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptionRendererScene (mkSelector "needsPeriodicRefresh") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @hasActiveCaptions@
hasActiveCaptionsSelector :: Selector
hasActiveCaptionsSelector = mkSelector "hasActiveCaptions"

-- | @Selector@ for @needsPeriodicRefresh@
needsPeriodicRefreshSelector :: Selector
needsPeriodicRefreshSelector = mkSelector "needsPeriodicRefresh"

