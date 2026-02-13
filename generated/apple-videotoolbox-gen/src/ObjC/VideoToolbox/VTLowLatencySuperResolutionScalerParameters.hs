{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains both input and output parameters that the low-latency super-resolution scaler frame processor needs.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class.
--
-- @VTLowLatencySuperResolutionScalerParameters@ are frame-level parameters.
--
-- Generated bindings for @VTLowLatencySuperResolutionScalerParameters@.
module ObjC.VideoToolbox.VTLowLatencySuperResolutionScalerParameters
  ( VTLowLatencySuperResolutionScalerParameters
  , IsVTLowLatencySuperResolutionScalerParameters(..)
  , initWithSourceFrame_destinationFrame
  , init_
  , new
  , sourceFrame
  , destinationFrame
  , destinationFrameSelector
  , initSelector
  , initWithSourceFrame_destinationFrameSelector
  , newSelector
  , sourceFrameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new low-latency, super-resolution scaler parameters object.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - destinationFrame: User-allocated pixel buffer that receives the scaled processor output; must be non @nil@.
--
-- ObjC selector: @- initWithSourceFrame:destinationFrame:@
initWithSourceFrame_destinationFrame :: (IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame destinationFrame) => vtLowLatencySuperResolutionScalerParameters -> sourceFrame -> destinationFrame -> IO (Id VTLowLatencySuperResolutionScalerParameters)
initWithSourceFrame_destinationFrame vtLowLatencySuperResolutionScalerParameters sourceFrame destinationFrame =
  sendOwnedMessage vtLowLatencySuperResolutionScalerParameters initWithSourceFrame_destinationFrameSelector (toVTFrameProcessorFrame sourceFrame) (toVTFrameProcessorFrame destinationFrame)

-- | @- init@
init_ :: IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters => vtLowLatencySuperResolutionScalerParameters -> IO (Id VTLowLatencySuperResolutionScalerParameters)
init_ vtLowLatencySuperResolutionScalerParameters =
  sendOwnedMessage vtLowLatencySuperResolutionScalerParameters initSelector

-- | @+ new@
new :: IO (Id VTLowLatencySuperResolutionScalerParameters)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerParameters"
    sendOwnedClassMessage cls' newSelector

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters => vtLowLatencySuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtLowLatencySuperResolutionScalerParameters =
  sendMessage vtLowLatencySuperResolutionScalerParameters sourceFrameSelector

-- | Destination frame that contains user-allocated pixel buffer that receives the scaled processor output.
--
-- ObjC selector: @- destinationFrame@
destinationFrame :: IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters => vtLowLatencySuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
destinationFrame vtLowLatencySuperResolutionScalerParameters =
  sendMessage vtLowLatencySuperResolutionScalerParameters destinationFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:destinationFrame:@
initWithSourceFrame_destinationFrameSelector :: Selector '[Id VTFrameProcessorFrame, Id VTFrameProcessorFrame] (Id VTLowLatencySuperResolutionScalerParameters)
initWithSourceFrame_destinationFrameSelector = mkSelector "initWithSourceFrame:destinationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTLowLatencySuperResolutionScalerParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTLowLatencySuperResolutionScalerParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @destinationFrame@
destinationFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
destinationFrameSelector = mkSelector "destinationFrame"

