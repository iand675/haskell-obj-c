{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CNRenderingSession@.
module ObjC.Cinematic.CNRenderingSession
  ( CNRenderingSession
  , IsCNRenderingSession(..)
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImage
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBA
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChroma
  , init_
  , new
  , commandQueue
  , sessionAttributes
  , quality
  , sourcePixelFormatTypes
  , destinationPixelFormatTypes
  , commandQueueSelector
  , destinationPixelFormatTypesSelector
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector
  , initSelector
  , newSelector
  , qualitySelector
  , sessionAttributesSelector
  , sourcePixelFormatTypesSelector

  -- * Enum types
  , CNRenderingQuality(CNRenderingQuality)
  , pattern CNRenderingQualityThumbnail
  , pattern CNRenderingQualityPreview
  , pattern CNRenderingQualityExport
  , pattern CNRenderingQualityExportHigh

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Cinematic.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Encode a command to render a shallow depth of field (SDoF) image to a pixel buffer. - Parameters:   - commandBuffer: the metal command buffer on which to encode the command   - frameAttributes: controls the focus distance and aperture of the rendering   - sourceImage: a pixel buffer read from the cinematicVideoTrack   - sourceDisparity: a pixel buffer read from the cinematicDisparityTrack   - destinationImage: the pixel buffer to which the SDoF image is rendered - Returns: whether encoding the render command was successful
--
-- ObjC selector: @- encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImage :: (IsCNRenderingSession cnRenderingSession, IsCNRenderingSessionFrameAttributes frameAttributes) => cnRenderingSession -> RawId -> frameAttributes -> Ptr () -> Ptr () -> Ptr () -> IO Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImage cnRenderingSession commandBuffer frameAttributes sourceImage sourceDisparity destinationImage =
  sendMessage cnRenderingSession encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector commandBuffer (toCNRenderingSessionFrameAttributes frameAttributes) sourceImage sourceDisparity destinationImage

-- | Encode a command to render a shallow depth of field (SDoF) image to a metal texture as RGBA. - Parameters:   - commandBuffer: the metal command buffer on which to encode the command   - frameAttributes: controls the focus distance and aperture of the rendering   - sourceImage: a pixel buffer read from the cinematicVideoTrack   - sourceDisparity: a pixel buffer read from the cinematicDisparityTrack   - destinationRGBA: a metal texture to which the SDoF image is rendered in RGBA format - Returns: whether encoding the render command was successful
--
-- ObjC selector: @- encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBA :: (IsCNRenderingSession cnRenderingSession, IsCNRenderingSessionFrameAttributes frameAttributes) => cnRenderingSession -> RawId -> frameAttributes -> Ptr () -> Ptr () -> RawId -> IO Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBA cnRenderingSession commandBuffer frameAttributes sourceImage sourceDisparity destinationRGBA =
  sendMessage cnRenderingSession encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector commandBuffer (toCNRenderingSessionFrameAttributes frameAttributes) sourceImage sourceDisparity destinationRGBA

-- | Encode a command to render a shallow depth of field (SDoF) image to two metal textures as luma and chroma. - Parameters:   - commandBuffer: the metal command buffer on which to encode the command   - frameAttributes: controls the focus distance and aperture of the rendering   - sourceImage: a pixel buffer read from the cinematicVideoTrack   - sourceDisparity: a pixel buffer read from the cinematicDisparityTrack   - destinationLuma: a metal texture to which the luma of the SDoF image is rendered   - destinationChroma: a metal texture to which the chroma of the SDoF image is rendered - Returns: whether encoding the render command was successful
--
-- ObjC selector: @- encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChroma :: (IsCNRenderingSession cnRenderingSession, IsCNRenderingSessionFrameAttributes frameAttributes) => cnRenderingSession -> RawId -> frameAttributes -> Ptr () -> Ptr () -> RawId -> RawId -> IO Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChroma cnRenderingSession commandBuffer frameAttributes sourceImage sourceDisparity destinationLuma destinationChroma =
  sendMessage cnRenderingSession encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector commandBuffer (toCNRenderingSessionFrameAttributes frameAttributes) sourceImage sourceDisparity destinationLuma destinationChroma

-- | @- init@
init_ :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO (Id CNRenderingSession)
init_ cnRenderingSession =
  sendOwnedMessage cnRenderingSession initSelector

-- | @+ new@
new :: IO (Id CNRenderingSession)
new  =
  do
    cls' <- getRequiredClass "CNRenderingSession"
    sendOwnedClassMessage cls' newSelector

-- | @- commandQueue@
commandQueue :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO RawId
commandQueue cnRenderingSession =
  sendMessage cnRenderingSession commandQueueSelector

-- | @- sessionAttributes@
sessionAttributes :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO (Id CNRenderingSessionAttributes)
sessionAttributes cnRenderingSession =
  sendMessage cnRenderingSession sessionAttributesSelector

-- | @- quality@
quality :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO CNRenderingQuality
quality cnRenderingSession =
  sendMessage cnRenderingSession qualitySelector

-- | The pixel format types supported for the input source.
--
-- Use with kCVPixelBufferPixelFormatTypeKey in the video compositor's sourcePixelBufferAttributes dictionary when implementing AVVideoCompositing.
--
-- ObjC selector: @+ sourcePixelFormatTypes@
sourcePixelFormatTypes :: IO (Id NSArray)
sourcePixelFormatTypes  =
  do
    cls' <- getRequiredClass "CNRenderingSession"
    sendClassMessage cls' sourcePixelFormatTypesSelector

-- | The pixel format types supported for the output destination.
--
-- Use with kCVPixelBufferPixelFormatTypeKey in the video compositor's requiredPixelBufferAttributesForRenderContext dictionary when implementing AVVideoCompositing.
--
-- ObjC selector: @+ destinationPixelFormatTypes@
destinationPixelFormatTypes :: IO (Id NSArray)
destinationPixelFormatTypes  =
  do
    cls' <- getRequiredClass "CNRenderingSession"
    sendClassMessage cls' destinationPixelFormatTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector :: Selector '[RawId, Id CNRenderingSessionFrameAttributes, Ptr (), Ptr (), Ptr ()] Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector = mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:"

-- | @Selector@ for @encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector :: Selector '[RawId, Id CNRenderingSessionFrameAttributes, Ptr (), Ptr (), RawId] Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector = mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:"

-- | @Selector@ for @encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector :: Selector '[RawId, Id CNRenderingSessionFrameAttributes, Ptr (), Ptr (), RawId, RawId] Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector = mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNRenderingSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNRenderingSession)
newSelector = mkSelector "new"

-- | @Selector@ for @commandQueue@
commandQueueSelector :: Selector '[] RawId
commandQueueSelector = mkSelector "commandQueue"

-- | @Selector@ for @sessionAttributes@
sessionAttributesSelector :: Selector '[] (Id CNRenderingSessionAttributes)
sessionAttributesSelector = mkSelector "sessionAttributes"

-- | @Selector@ for @quality@
qualitySelector :: Selector '[] CNRenderingQuality
qualitySelector = mkSelector "quality"

-- | @Selector@ for @sourcePixelFormatTypes@
sourcePixelFormatTypesSelector :: Selector '[] (Id NSArray)
sourcePixelFormatTypesSelector = mkSelector "sourcePixelFormatTypes"

-- | @Selector@ for @destinationPixelFormatTypes@
destinationPixelFormatTypesSelector :: Selector '[] (Id NSArray)
destinationPixelFormatTypesSelector = mkSelector "destinationPixelFormatTypes"

