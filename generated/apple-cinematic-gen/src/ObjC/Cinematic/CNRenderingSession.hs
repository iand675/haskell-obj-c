{-# LANGUAGE PatternSynonyms #-}
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
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector
  , encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector
  , initSelector
  , newSelector
  , commandQueueSelector
  , sessionAttributesSelector
  , qualitySelector
  , sourcePixelFormatTypesSelector
  , destinationPixelFormatTypesSelector

  -- * Enum types
  , CNRenderingQuality(CNRenderingQuality)
  , pattern CNRenderingQualityThumbnail
  , pattern CNRenderingQualityPreview
  , pattern CNRenderingQualityExport
  , pattern CNRenderingQualityExportHigh

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Cinematic.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Encode a command to render a shallow depth of field (SDoF) image to a pixel buffer. - Parameters:   - commandBuffer: the metal command buffer on which to encode the command   - frameAttributes: controls the focus distance and aperture of the rendering   - sourceImage: a pixel buffer read from the cinematicVideoTrack   - sourceDisparity: a pixel buffer read from the cinematicDisparityTrack   - destinationImage: the pixel buffer to which the SDoF image is rendered - Returns: whether encoding the render command was successful
--
-- ObjC selector: @- encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImage :: (IsCNRenderingSession cnRenderingSession, IsCNRenderingSessionFrameAttributes frameAttributes) => cnRenderingSession -> RawId -> frameAttributes -> Ptr () -> Ptr () -> Ptr () -> IO Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImage cnRenderingSession  commandBuffer frameAttributes sourceImage sourceDisparity destinationImage =
  withObjCPtr frameAttributes $ \raw_frameAttributes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnRenderingSession (mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:") retCULong [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_frameAttributes :: Ptr ()), argPtr sourceImage, argPtr sourceDisparity, argPtr destinationImage]

-- | Encode a command to render a shallow depth of field (SDoF) image to a metal texture as RGBA. - Parameters:   - commandBuffer: the metal command buffer on which to encode the command   - frameAttributes: controls the focus distance and aperture of the rendering   - sourceImage: a pixel buffer read from the cinematicVideoTrack   - sourceDisparity: a pixel buffer read from the cinematicDisparityTrack   - destinationRGBA: a metal texture to which the SDoF image is rendered in RGBA format - Returns: whether encoding the render command was successful
--
-- ObjC selector: @- encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBA :: (IsCNRenderingSession cnRenderingSession, IsCNRenderingSessionFrameAttributes frameAttributes) => cnRenderingSession -> RawId -> frameAttributes -> Ptr () -> Ptr () -> RawId -> IO Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBA cnRenderingSession  commandBuffer frameAttributes sourceImage sourceDisparity destinationRGBA =
  withObjCPtr frameAttributes $ \raw_frameAttributes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnRenderingSession (mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:") retCULong [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_frameAttributes :: Ptr ()), argPtr sourceImage, argPtr sourceDisparity, argPtr (castPtr (unRawId destinationRGBA) :: Ptr ())]

-- | Encode a command to render a shallow depth of field (SDoF) image to two metal textures as luma and chroma. - Parameters:   - commandBuffer: the metal command buffer on which to encode the command   - frameAttributes: controls the focus distance and aperture of the rendering   - sourceImage: a pixel buffer read from the cinematicVideoTrack   - sourceDisparity: a pixel buffer read from the cinematicDisparityTrack   - destinationLuma: a metal texture to which the luma of the SDoF image is rendered   - destinationChroma: a metal texture to which the chroma of the SDoF image is rendered - Returns: whether encoding the render command was successful
--
-- ObjC selector: @- encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChroma :: (IsCNRenderingSession cnRenderingSession, IsCNRenderingSessionFrameAttributes frameAttributes) => cnRenderingSession -> RawId -> frameAttributes -> Ptr () -> Ptr () -> RawId -> RawId -> IO Bool
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChroma cnRenderingSession  commandBuffer frameAttributes sourceImage sourceDisparity destinationLuma destinationChroma =
  withObjCPtr frameAttributes $ \raw_frameAttributes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnRenderingSession (mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:") retCULong [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_frameAttributes :: Ptr ()), argPtr sourceImage, argPtr sourceDisparity, argPtr (castPtr (unRawId destinationLuma) :: Ptr ()), argPtr (castPtr (unRawId destinationChroma) :: Ptr ())]

-- | @- init@
init_ :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO (Id CNRenderingSession)
init_ cnRenderingSession  =
    sendMsg cnRenderingSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNRenderingSession)
new  =
  do
    cls' <- getRequiredClass "CNRenderingSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- commandQueue@
commandQueue :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO RawId
commandQueue cnRenderingSession  =
    fmap (RawId . castPtr) $ sendMsg cnRenderingSession (mkSelector "commandQueue") (retPtr retVoid) []

-- | @- sessionAttributes@
sessionAttributes :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO (Id CNRenderingSessionAttributes)
sessionAttributes cnRenderingSession  =
    sendMsg cnRenderingSession (mkSelector "sessionAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quality@
quality :: IsCNRenderingSession cnRenderingSession => cnRenderingSession -> IO CNRenderingQuality
quality cnRenderingSession  =
    fmap (coerce :: CLong -> CNRenderingQuality) $ sendMsg cnRenderingSession (mkSelector "quality") retCLong []

-- | The pixel format types supported for the input source.
--
-- Use with kCVPixelBufferPixelFormatTypeKey in the video compositor's sourcePixelBufferAttributes dictionary when implementing AVVideoCompositing.
--
-- ObjC selector: @+ sourcePixelFormatTypes@
sourcePixelFormatTypes :: IO (Id NSArray)
sourcePixelFormatTypes  =
  do
    cls' <- getRequiredClass "CNRenderingSession"
    sendClassMsg cls' (mkSelector "sourcePixelFormatTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The pixel format types supported for the output destination.
--
-- Use with kCVPixelBufferPixelFormatTypeKey in the video compositor's requiredPixelBufferAttributesForRenderContext dictionary when implementing AVVideoCompositing.
--
-- ObjC selector: @+ destinationPixelFormatTypes@
destinationPixelFormatTypes :: IO (Id NSArray)
destinationPixelFormatTypes  =
  do
    cls' <- getRequiredClass "CNRenderingSession"
    sendClassMsg cls' (mkSelector "destinationPixelFormatTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector :: Selector
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationImageSelector = mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationImage:"

-- | @Selector@ for @encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector :: Selector
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationRGBASelector = mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationRGBA:"

-- | @Selector@ for @encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:@
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector :: Selector
encodeRenderToCommandBuffer_frameAttributes_sourceImage_sourceDisparity_destinationLuma_destinationChromaSelector = mkSelector "encodeRenderToCommandBuffer:frameAttributes:sourceImage:sourceDisparity:destinationLuma:destinationChroma:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @commandQueue@
commandQueueSelector :: Selector
commandQueueSelector = mkSelector "commandQueue"

-- | @Selector@ for @sessionAttributes@
sessionAttributesSelector :: Selector
sessionAttributesSelector = mkSelector "sessionAttributes"

-- | @Selector@ for @quality@
qualitySelector :: Selector
qualitySelector = mkSelector "quality"

-- | @Selector@ for @sourcePixelFormatTypes@
sourcePixelFormatTypesSelector :: Selector
sourcePixelFormatTypesSelector = mkSelector "sourcePixelFormatTypes"

-- | @Selector@ for @destinationPixelFormatTypes@
destinationPixelFormatTypesSelector :: Selector
destinationPixelFormatTypesSelector = mkSelector "destinationPixelFormatTypes"

