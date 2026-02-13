{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents focus & detection information at a particular time.
--
-- Indicates where to focus (disparity) and what to focus on (detection) at a particular time in the movie. It also provides access to all known detections that can be focused on at that time. Utility methods support looking up a detection by detectionID or detectionGroupID.
--
-- Frames are obtained from the cinematic script using @frame(at:tolerance:)@ or @frames(in:)@.
--
-- Generated bindings for @CNScriptFrame@.
module ObjC.Cinematic.CNScriptFrame
  ( CNScriptFrame
  , IsCNScriptFrame(..)
  , init_
  , new
  , detectionForID
  , bestDetectionForGroupID
  , focusDisparity
  , focusDetection
  , allDetections
  , allDetectionsSelector
  , bestDetectionForGroupIDSelector
  , detectionForIDSelector
  , focusDetectionSelector
  , focusDisparitySelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO (Id CNScriptFrame)
init_ cnScriptFrame =
  sendOwnedMessage cnScriptFrame initSelector

-- | @+ new@
new :: IO (Id CNScriptFrame)
new  =
  do
    cls' <- getRequiredClass "CNScriptFrame"
    sendOwnedClassMessage cls' newSelector

-- | The detection in this frame with the given detection ID, if any.
--
-- ObjC selector: @- detectionForID:@
detectionForID :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> CLong -> IO (Id CNDetection)
detectionForID cnScriptFrame detectionID =
  sendMessage cnScriptFrame detectionForIDSelector detectionID

-- | The best detection to focus on in this frame among those with the given detectionGroupID. For example, a face is preferred to the corresponding torso, even though both have the same detectionGroupID.
--
-- ObjC selector: @- bestDetectionForGroupID:@
bestDetectionForGroupID :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> CLong -> IO (Id CNDetection)
bestDetectionForGroupID cnScriptFrame detectionGroupID =
  sendMessage cnScriptFrame bestDetectionForGroupIDSelector detectionGroupID

-- | The disparity value representing the focus plane at which the script is focused in this frame.
--
-- A larger disparity results in the focus plane being closer to the camera. The scale and offset of disparity is not defined.
--
-- Pass this to the rendering session when rendering the corresponding frame of the movie to focus at the recommended depth.
--
-- ObjC selector: @- focusDisparity@
focusDisparity :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO CFloat
focusDisparity cnScriptFrame =
  sendMessage cnScriptFrame focusDisparitySelector

-- | The detection on which the script is focused in this frame.
--
-- The focusDisparity of the focusDetection can be different from that of the frame such as when a rack focus is in progress.
--
-- ObjC selector: @- focusDetection@
focusDetection :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO (Id CNDetection)
focusDetection cnScriptFrame =
  sendMessage cnScriptFrame focusDetectionSelector

-- | All detected objects in this frame.
--
-- ObjC selector: @- allDetections@
allDetections :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO (Id NSArray)
allDetections cnScriptFrame =
  sendMessage cnScriptFrame allDetectionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNScriptFrame)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNScriptFrame)
newSelector = mkSelector "new"

-- | @Selector@ for @detectionForID:@
detectionForIDSelector :: Selector '[CLong] (Id CNDetection)
detectionForIDSelector = mkSelector "detectionForID:"

-- | @Selector@ for @bestDetectionForGroupID:@
bestDetectionForGroupIDSelector :: Selector '[CLong] (Id CNDetection)
bestDetectionForGroupIDSelector = mkSelector "bestDetectionForGroupID:"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector '[] CFloat
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @focusDetection@
focusDetectionSelector :: Selector '[] (Id CNDetection)
focusDetectionSelector = mkSelector "focusDetection"

-- | @Selector@ for @allDetections@
allDetectionsSelector :: Selector '[] (Id NSArray)
allDetectionsSelector = mkSelector "allDetections"

