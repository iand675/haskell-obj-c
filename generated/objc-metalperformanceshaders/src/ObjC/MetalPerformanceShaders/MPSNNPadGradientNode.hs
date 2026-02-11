{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNPadGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNPadGradientNode
  ( MPSNNPadGradientNode
  , IsMPSNNPadGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState
  , initWithSourceGradient_sourceImage_gradientState
  , nodeWithSourceGradient_sourceImage_gradientStateSelector
  , initWithSourceGradient_sourceImage_gradientStateSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A node to represent the gradient of a padding node.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The input image from the forward padding node.
--
-- Returns: A MPSNNPadGradientNode
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNPadGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSNNPadGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= retainedObject . castPtr

-- | A node to represent the gradient of a padding node.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The input image from the forward reshape node.
--
-- Returns: A MPSNNPadGradientNode
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSNNPadGradientNode mpsnnPadGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnPadGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNPadGradientNode)
initWithSourceGradient_sourceImage_gradientState mpsnnPadGradientNode  sourceGradient sourceImage gradientState =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpsnnPadGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

