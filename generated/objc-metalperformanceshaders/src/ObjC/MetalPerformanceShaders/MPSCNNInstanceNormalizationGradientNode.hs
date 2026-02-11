{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNInstanceNormalizationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNInstanceNormalizationGradientNode
  ( MPSCNNInstanceNormalizationGradientNode
  , IsMPSCNNInstanceNormalizationGradientNode(..)
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

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSCNNInstanceNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSCNNInstanceNormalizationGradientNode mpscnnInstanceNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnInstanceNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSCNNInstanceNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState mpscnnInstanceNormalizationGradientNode  sourceGradient sourceImage gradientState =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnInstanceNormalizationGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

