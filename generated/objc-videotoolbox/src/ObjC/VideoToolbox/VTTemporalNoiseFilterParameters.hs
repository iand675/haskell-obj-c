{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Encapsulates the frame-level parameters necessary for processing a source frame using temporal noise-filter processor.
--
-- This object is intended for sending input parameters into the @processWithParameters@ method of the @VTFrameProcessor@ class. Temporal noise-filter processor utilizes past and future reference frames, provided in presentation time order, to reduce noise from the source frame. The @previousFrameCount@ and @nextFrameCount@ properties in ``VTTemporalNoiseFilterConfiguration`` represent the maximum number of past and future reference frames that the processor can use to achieve optimum noise reduction quality. The number of reference frames provided shall depend on their availability, but at a minimum, you must provide one reference frame, either past or future. The parameter @destinationFrame@ stores the output frame that the processor returns to the caller upon the successful completion of the @processWithParameters@ operation.
--
-- Generated bindings for @VTTemporalNoiseFilterParameters@.
module ObjC.VideoToolbox.VTTemporalNoiseFilterParameters
  ( VTTemporalNoiseFilterParameters
  , IsVTTemporalNoiseFilterParameters(..)
  , initWithSourceFrame_nextFrames_previousFrames_destinationFrame_filterStrength_hasDiscontinuity
  , init_
  , new
  , sourceFrame
  , nextFrames
  , previousFrames
  , filterStrength
  , setFilterStrength
  , hasDiscontinuity
  , setHasDiscontinuity
  , destinationFrame
  , initWithSourceFrame_nextFrames_previousFrames_destinationFrame_filterStrength_hasDiscontinuitySelector
  , initSelector
  , newSelector
  , sourceFrameSelector
  , nextFramesSelector
  , previousFramesSelector
  , filterStrengthSelector
  , setFilterStrengthSelector
  , hasDiscontinuitySelector
  , setHasDiscontinuitySelector
  , destinationFrameSelector


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

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new @VTTemporalNoiseFilterParameters@ object.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - nextFrames: Future reference frames in presentation time order to use for processing the source frame. The number   of frames can vary from 0 to the number specified by ``VTTemporalNoiseFilterConfiguration/nextFrameCount`` property.   - previousFrames: Past reference frames in presentation time order to use for processing the source frame. The number   of frames can vary from 0 to the number specified by ``VTTemporalNoiseFilterConfiguration/previousFrameCount`` property.   - destinationFrame: User-allocated pixel buffer that receives the output frame. The pixel format of @destinationFrame@   must match with that of the @sourceFrame@.   - filterStrength: Strength of the noise-filtering to use. The value can range from the minimum strength of 0.0 to the   maximum strength of 1.0. Change in filter strength causes the processor to flush all frames in the queue prior to   processing the source frame.   - hasDiscontinuity: Marks sequence discontinuity, forcing the processor to reset prior to processing the source frame.
--
-- ObjC selector: @- initWithSourceFrame:nextFrames:previousFrames:destinationFrame:filterStrength:hasDiscontinuity:@
initWithSourceFrame_nextFrames_previousFrames_destinationFrame_filterStrength_hasDiscontinuity :: (IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters, IsVTFrameProcessorFrame sourceFrame, IsNSArray nextFrames, IsNSArray previousFrames, IsVTFrameProcessorFrame destinationFrame) => vtTemporalNoiseFilterParameters -> sourceFrame -> nextFrames -> previousFrames -> destinationFrame -> CFloat -> CUChar -> IO (Id VTTemporalNoiseFilterParameters)
initWithSourceFrame_nextFrames_previousFrames_destinationFrame_filterStrength_hasDiscontinuity vtTemporalNoiseFilterParameters  sourceFrame nextFrames previousFrames destinationFrame filterStrength hasDiscontinuity =
withObjCPtr sourceFrame $ \raw_sourceFrame ->
  withObjCPtr nextFrames $ \raw_nextFrames ->
    withObjCPtr previousFrames $ \raw_previousFrames ->
      withObjCPtr destinationFrame $ \raw_destinationFrame ->
          sendMsg vtTemporalNoiseFilterParameters (mkSelector "initWithSourceFrame:nextFrames:previousFrames:destinationFrame:filterStrength:hasDiscontinuity:") (retPtr retVoid) [argPtr (castPtr raw_sourceFrame :: Ptr ()), argPtr (castPtr raw_nextFrames :: Ptr ()), argPtr (castPtr raw_previousFrames :: Ptr ()), argPtr (castPtr raw_destinationFrame :: Ptr ()), argCFloat (fromIntegral filterStrength), argCUChar (fromIntegral hasDiscontinuity)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO (Id VTTemporalNoiseFilterParameters)
init_ vtTemporalNoiseFilterParameters  =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTTemporalNoiseFilterParameters)
new  =
  do
    cls' <- getRequiredClass "VTTemporalNoiseFilterParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Current source frame; must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtTemporalNoiseFilterParameters  =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Future reference frames in presentation time order that you use to process the source frame.
--
-- The number of frames can vary from 0 to the number specified by the @nextFrameCount@ property in @VTTemporalNoiseFilterConfiguration@.
--
-- ObjC selector: @- nextFrames@
nextFrames :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO (Id NSArray)
nextFrames vtTemporalNoiseFilterParameters  =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "nextFrames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Past reference frames in presentation time order that you use to process the source frame.
--
-- The number of frames can vary from 0 to the number specified by the @previousFrameCount@ property in @VTTemporalNoiseFilterConfiguration@.
--
-- ObjC selector: @- previousFrames@
previousFrames :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO (Id NSArray)
previousFrames vtTemporalNoiseFilterParameters  =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "previousFrames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A parameter to control the strength of noise-filtering. The value can range from the minimum strength of 0.0 to the maximum strength of 1.0. Change in filter strength causes the processor to flush all frames in the queue prior to processing the source frame.
--
-- ObjC selector: @- filterStrength@
filterStrength :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO CFloat
filterStrength vtTemporalNoiseFilterParameters  =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "filterStrength") retCFloat []

-- | A parameter to control the strength of noise-filtering. The value can range from the minimum strength of 0.0 to the maximum strength of 1.0. Change in filter strength causes the processor to flush all frames in the queue prior to processing the source frame.
--
-- ObjC selector: @- setFilterStrength:@
setFilterStrength :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> CFloat -> IO ()
setFilterStrength vtTemporalNoiseFilterParameters  value =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "setFilterStrength:") retVoid [argCFloat (fromIntegral value)]

-- | A Boolean that indicates sequence discontinuity, forcing the processor to reset prior to processing the source frame.
--
-- ObjC selector: @- hasDiscontinuity@
hasDiscontinuity :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO Bool
hasDiscontinuity vtTemporalNoiseFilterParameters  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vtTemporalNoiseFilterParameters (mkSelector "hasDiscontinuity") retCULong []

-- | A Boolean that indicates sequence discontinuity, forcing the processor to reset prior to processing the source frame.
--
-- ObjC selector: @- setHasDiscontinuity:@
setHasDiscontinuity :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> Bool -> IO ()
setHasDiscontinuity vtTemporalNoiseFilterParameters  value =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "setHasDiscontinuity:") retVoid [argCULong (if value then 1 else 0)]

-- | Destination frame that contains a user-allocated pixel buffer that receives the output frame.
--
-- ObjC selector: @- destinationFrame@
destinationFrame :: IsVTTemporalNoiseFilterParameters vtTemporalNoiseFilterParameters => vtTemporalNoiseFilterParameters -> IO (Id VTFrameProcessorFrame)
destinationFrame vtTemporalNoiseFilterParameters  =
  sendMsg vtTemporalNoiseFilterParameters (mkSelector "destinationFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrames:previousFrames:destinationFrame:filterStrength:hasDiscontinuity:@
initWithSourceFrame_nextFrames_previousFrames_destinationFrame_filterStrength_hasDiscontinuitySelector :: Selector
initWithSourceFrame_nextFrames_previousFrames_destinationFrame_filterStrength_hasDiscontinuitySelector = mkSelector "initWithSourceFrame:nextFrames:previousFrames:destinationFrame:filterStrength:hasDiscontinuity:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @nextFrames@
nextFramesSelector :: Selector
nextFramesSelector = mkSelector "nextFrames"

-- | @Selector@ for @previousFrames@
previousFramesSelector :: Selector
previousFramesSelector = mkSelector "previousFrames"

-- | @Selector@ for @filterStrength@
filterStrengthSelector :: Selector
filterStrengthSelector = mkSelector "filterStrength"

-- | @Selector@ for @setFilterStrength:@
setFilterStrengthSelector :: Selector
setFilterStrengthSelector = mkSelector "setFilterStrength:"

-- | @Selector@ for @hasDiscontinuity@
hasDiscontinuitySelector :: Selector
hasDiscontinuitySelector = mkSelector "hasDiscontinuity"

-- | @Selector@ for @setHasDiscontinuity:@
setHasDiscontinuitySelector :: Selector
setHasDiscontinuitySelector = mkSelector "setHasDiscontinuity:"

-- | @Selector@ for @destinationFrame@
destinationFrameSelector :: Selector
destinationFrameSelector = mkSelector "destinationFrame"

