{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Frame-specific information required to render a frame in a rendering session.
--
-- Generated bindings for @CNRenderingSessionFrameAttributes@.
module ObjC.Cinematic.CNRenderingSessionFrameAttributes
  ( CNRenderingSessionFrameAttributes
  , IsCNRenderingSessionFrameAttributes(..)
  , initWithSampleBuffer_sessionAttributes
  , initWithTimedMetadataGroup_sessionAttributes
  , init_
  , new
  , focusDisparity
  , setFocusDisparity
  , fNumber
  , setFNumber
  , fNumberSelector
  , focusDisparitySelector
  , initSelector
  , initWithSampleBuffer_sessionAttributesSelector
  , initWithTimedMetadataGroup_sessionAttributesSelector
  , newSelector
  , setFNumberSelector
  , setFocusDisparitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize rendering frame attributes from a sample buffer read from a cinematic metadata track. - Parameters:   - sampleBuffer: A sample buffer read from the timed cinematic metadata track of a cinematic asset.   - sessionAttributes: Rendering session attributes loaded from a cinematic asset.
--
-- ObjC selector: @- initWithSampleBuffer:sessionAttributes:@
initWithSampleBuffer_sessionAttributes :: (IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes, IsCNRenderingSessionAttributes sessionAttributes) => cnRenderingSessionFrameAttributes -> Ptr () -> sessionAttributes -> IO (Id CNRenderingSessionFrameAttributes)
initWithSampleBuffer_sessionAttributes cnRenderingSessionFrameAttributes sampleBuffer sessionAttributes =
  sendOwnedMessage cnRenderingSessionFrameAttributes initWithSampleBuffer_sessionAttributesSelector sampleBuffer (toCNRenderingSessionAttributes sessionAttributes)

-- | Initialize rendering frame attributes from a timed metadata group read from a cinematic metadata track. - Parameters:   - metadataGroup: An AVTimedMetadataGroup read from the timed cinematic metadata track of a cinematic asset.   - sessionAttributes: Rendering session attributes loaded from a cinematic asset.
--
-- ObjC selector: @- initWithTimedMetadataGroup:sessionAttributes:@
initWithTimedMetadataGroup_sessionAttributes :: (IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes, IsAVTimedMetadataGroup metadataGroup, IsCNRenderingSessionAttributes sessionAttributes) => cnRenderingSessionFrameAttributes -> metadataGroup -> sessionAttributes -> IO (Id CNRenderingSessionFrameAttributes)
initWithTimedMetadataGroup_sessionAttributes cnRenderingSessionFrameAttributes metadataGroup sessionAttributes =
  sendOwnedMessage cnRenderingSessionFrameAttributes initWithTimedMetadataGroup_sessionAttributesSelector (toAVTimedMetadataGroup metadataGroup) (toCNRenderingSessionAttributes sessionAttributes)

-- | @- init@
init_ :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> IO (Id CNRenderingSessionFrameAttributes)
init_ cnRenderingSessionFrameAttributes =
  sendOwnedMessage cnRenderingSessionFrameAttributes initSelector

-- | @+ new@
new :: IO (Id CNRenderingSessionFrameAttributes)
new  =
  do
    cls' <- getRequiredClass "CNRenderingSessionFrameAttributes"
    sendOwnedClassMessage cls' newSelector

-- | The disparity value which represents the focus plane at which the rendered image should be in focus.
--
-- A larger disparity results in the focus plane being closer to the camera. The scale and offset of disparity is not defined. It is best practice to obtain disparity values from detections or by interpolation between known disparity values.
--
-- ObjC selector: @- focusDisparity@
focusDisparity :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> IO CFloat
focusDisparity cnRenderingSessionFrameAttributes =
  sendMessage cnRenderingSessionFrameAttributes focusDisparitySelector

-- | The disparity value which represents the focus plane at which the rendered image should be in focus.
--
-- A larger disparity results in the focus plane being closer to the camera. The scale and offset of disparity is not defined. It is best practice to obtain disparity values from detections or by interpolation between known disparity values.
--
-- ObjC selector: @- setFocusDisparity:@
setFocusDisparity :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> CFloat -> IO ()
setFocusDisparity cnRenderingSessionFrameAttributes value =
  sendMessage cnRenderingSessionFrameAttributes setFocusDisparitySelector value

-- | The f-stop value which inversely affects the aperture used to render the image.
--
-- A smaller f/ number results in larger bokeh and a shallower depth of field in the rendered image.
--
-- ObjC selector: @- fNumber@
fNumber :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> IO CFloat
fNumber cnRenderingSessionFrameAttributes =
  sendMessage cnRenderingSessionFrameAttributes fNumberSelector

-- | The f-stop value which inversely affects the aperture used to render the image.
--
-- A smaller f/ number results in larger bokeh and a shallower depth of field in the rendered image.
--
-- ObjC selector: @- setFNumber:@
setFNumber :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> CFloat -> IO ()
setFNumber cnRenderingSessionFrameAttributes value =
  sendMessage cnRenderingSessionFrameAttributes setFNumberSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSampleBuffer:sessionAttributes:@
initWithSampleBuffer_sessionAttributesSelector :: Selector '[Ptr (), Id CNRenderingSessionAttributes] (Id CNRenderingSessionFrameAttributes)
initWithSampleBuffer_sessionAttributesSelector = mkSelector "initWithSampleBuffer:sessionAttributes:"

-- | @Selector@ for @initWithTimedMetadataGroup:sessionAttributes:@
initWithTimedMetadataGroup_sessionAttributesSelector :: Selector '[Id AVTimedMetadataGroup, Id CNRenderingSessionAttributes] (Id CNRenderingSessionFrameAttributes)
initWithTimedMetadataGroup_sessionAttributesSelector = mkSelector "initWithTimedMetadataGroup:sessionAttributes:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNRenderingSessionFrameAttributes)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNRenderingSessionFrameAttributes)
newSelector = mkSelector "new"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector '[] CFloat
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @setFocusDisparity:@
setFocusDisparitySelector :: Selector '[CFloat] ()
setFocusDisparitySelector = mkSelector "setFocusDisparity:"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector '[] CFloat
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @setFNumber:@
setFNumberSelector :: Selector '[CFloat] ()
setFNumberSelector = mkSelector "setFNumber:"

