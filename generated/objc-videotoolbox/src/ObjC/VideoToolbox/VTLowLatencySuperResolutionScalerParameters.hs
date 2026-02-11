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
  , initWithSourceFrame_destinationFrameSelector
  , initSelector
  , newSelector
  , sourceFrameSelector
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

-- | Creates a new low-latency, super-resolution scaler parameters object.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - destinationFrame: User-allocated pixel buffer that receives the scaled processor output; must be non @nil@.
--
-- ObjC selector: @- initWithSourceFrame:destinationFrame:@
initWithSourceFrame_destinationFrame :: (IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame destinationFrame) => vtLowLatencySuperResolutionScalerParameters -> sourceFrame -> destinationFrame -> IO (Id VTLowLatencySuperResolutionScalerParameters)
initWithSourceFrame_destinationFrame vtLowLatencySuperResolutionScalerParameters  sourceFrame destinationFrame =
withObjCPtr sourceFrame $ \raw_sourceFrame ->
  withObjCPtr destinationFrame $ \raw_destinationFrame ->
      sendMsg vtLowLatencySuperResolutionScalerParameters (mkSelector "initWithSourceFrame:destinationFrame:") (retPtr retVoid) [argPtr (castPtr raw_sourceFrame :: Ptr ()), argPtr (castPtr raw_destinationFrame :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters => vtLowLatencySuperResolutionScalerParameters -> IO (Id VTLowLatencySuperResolutionScalerParameters)
init_ vtLowLatencySuperResolutionScalerParameters  =
  sendMsg vtLowLatencySuperResolutionScalerParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTLowLatencySuperResolutionScalerParameters)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters => vtLowLatencySuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtLowLatencySuperResolutionScalerParameters  =
  sendMsg vtLowLatencySuperResolutionScalerParameters (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Destination frame that contains user-allocated pixel buffer that receives the scaled processor output.
--
-- ObjC selector: @- destinationFrame@
destinationFrame :: IsVTLowLatencySuperResolutionScalerParameters vtLowLatencySuperResolutionScalerParameters => vtLowLatencySuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
destinationFrame vtLowLatencySuperResolutionScalerParameters  =
  sendMsg vtLowLatencySuperResolutionScalerParameters (mkSelector "destinationFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:destinationFrame:@
initWithSourceFrame_destinationFrameSelector :: Selector
initWithSourceFrame_destinationFrameSelector = mkSelector "initWithSourceFrame:destinationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @destinationFrame@
destinationFrameSelector :: Selector
destinationFrameSelector = mkSelector "destinationFrame"

