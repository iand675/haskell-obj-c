{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNGeometryTessellator
--
-- A geometry tessellator describes how a more detailed surface is calculated from the geometry's initial surface.
--
-- Generated bindings for @SCNGeometryTessellator@.
module ObjC.SceneKit.SCNGeometryTessellator
  ( SCNGeometryTessellator
  , IsSCNGeometryTessellator(..)
  , tessellationFactorScale
  , setTessellationFactorScale
  , tessellationPartitionMode
  , setTessellationPartitionMode
  , adaptive
  , setAdaptive
  , screenSpace
  , setScreenSpace
  , edgeTessellationFactor
  , setEdgeTessellationFactor
  , insideTessellationFactor
  , setInsideTessellationFactor
  , maximumEdgeLength
  , setMaximumEdgeLength
  , smoothingMode
  , setSmoothingMode
  , tessellationFactorScaleSelector
  , setTessellationFactorScaleSelector
  , tessellationPartitionModeSelector
  , setTessellationPartitionModeSelector
  , adaptiveSelector
  , setAdaptiveSelector
  , screenSpaceSelector
  , setScreenSpaceSelector
  , edgeTessellationFactorSelector
  , setEdgeTessellationFactorSelector
  , insideTessellationFactorSelector
  , setInsideTessellationFactorSelector
  , maximumEdgeLengthSelector
  , setMaximumEdgeLengthSelector
  , smoothingModeSelector
  , setSmoothingModeSelector

  -- * Enum types
  , MTLTessellationPartitionMode(MTLTessellationPartitionMode)
  , pattern MTLTessellationPartitionModePow2
  , pattern MTLTessellationPartitionModeInteger
  , pattern MTLTessellationPartitionModeFractionalOdd
  , pattern MTLTessellationPartitionModeFractionalEven
  , SCNTessellationSmoothingMode(SCNTessellationSmoothingMode)
  , pattern SCNTessellationSmoothingModeNone
  , pattern SCNTessellationSmoothingModePNTriangles
  , pattern SCNTessellationSmoothingModePhong

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | tessellationFactorScale
--
-- Specifies the scale factor applied to the per-patch tessellation factors. Defaults to 1.
--
-- ObjC selector: @- tessellationFactorScale@
tessellationFactorScale :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
tessellationFactorScale scnGeometryTessellator  =
  sendMsg scnGeometryTessellator (mkSelector "tessellationFactorScale") retCDouble []

-- | tessellationFactorScale
--
-- Specifies the scale factor applied to the per-patch tessellation factors. Defaults to 1.
--
-- ObjC selector: @- setTessellationFactorScale:@
setTessellationFactorScale :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setTessellationFactorScale scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setTessellationFactorScale:") retVoid [argCDouble (fromIntegral value)]

-- | tessellationPartitionMode
--
-- Specifies the tessellation partition mode. Defaults to MTLTessellationPartitionModeInteger.
--
-- ObjC selector: @- tessellationPartitionMode@
tessellationPartitionMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO MTLTessellationPartitionMode
tessellationPartitionMode scnGeometryTessellator  =
  fmap (coerce :: CULong -> MTLTessellationPartitionMode) $ sendMsg scnGeometryTessellator (mkSelector "tessellationPartitionMode") retCULong []

-- | tessellationPartitionMode
--
-- Specifies the tessellation partition mode. Defaults to MTLTessellationPartitionModeInteger.
--
-- ObjC selector: @- setTessellationPartitionMode:@
setTessellationPartitionMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> MTLTessellationPartitionMode -> IO ()
setTessellationPartitionMode scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setTessellationPartitionMode:") retVoid [argCULong (coerce value)]

-- | adaptive
--
-- Specifies if the tessellation should be uniform or adaptive. Defaults to NO.
--
-- ObjC selector: @- adaptive@
adaptive :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO Bool
adaptive scnGeometryTessellator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnGeometryTessellator (mkSelector "adaptive") retCULong []

-- | adaptive
--
-- Specifies if the tessellation should be uniform or adaptive. Defaults to NO.
--
-- ObjC selector: @- setAdaptive:@
setAdaptive :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> Bool -> IO ()
setAdaptive scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setAdaptive:") retVoid [argCULong (if value then 1 else 0)]

-- | screenspace
--
-- Specifies if the level of tessellation should be adapted in screenSpace. Defaults to NO.
--
-- ObjC selector: @- screenSpace@
screenSpace :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO Bool
screenSpace scnGeometryTessellator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnGeometryTessellator (mkSelector "screenSpace") retCULong []

-- | screenspace
--
-- Specifies if the level of tessellation should be adapted in screenSpace. Defaults to NO.
--
-- ObjC selector: @- setScreenSpace:@
setScreenSpace :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> Bool -> IO ()
setScreenSpace scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setScreenSpace:") retVoid [argCULong (if value then 1 else 0)]

-- | edgeTessellationFactor
--
-- Specifies the edge tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- edgeTessellationFactor@
edgeTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
edgeTessellationFactor scnGeometryTessellator  =
  sendMsg scnGeometryTessellator (mkSelector "edgeTessellationFactor") retCDouble []

-- | edgeTessellationFactor
--
-- Specifies the edge tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- setEdgeTessellationFactor:@
setEdgeTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setEdgeTessellationFactor scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setEdgeTessellationFactor:") retVoid [argCDouble (fromIntegral value)]

-- | insideTessellationFactor
--
-- Specifies the inside tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- insideTessellationFactor@
insideTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
insideTessellationFactor scnGeometryTessellator  =
  sendMsg scnGeometryTessellator (mkSelector "insideTessellationFactor") retCDouble []

-- | insideTessellationFactor
--
-- Specifies the inside tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- setInsideTessellationFactor:@
setInsideTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setInsideTessellationFactor scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setInsideTessellationFactor:") retVoid [argCDouble (fromIntegral value)]

-- | maximumEdgeLength
--
-- Specifies the maximum edge length. Defaults to 1.
--
-- This has no effect for non-adaptive subdivision
--
-- ObjC selector: @- maximumEdgeLength@
maximumEdgeLength :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
maximumEdgeLength scnGeometryTessellator  =
  sendMsg scnGeometryTessellator (mkSelector "maximumEdgeLength") retCDouble []

-- | maximumEdgeLength
--
-- Specifies the maximum edge length. Defaults to 1.
--
-- This has no effect for non-adaptive subdivision
--
-- ObjC selector: @- setMaximumEdgeLength:@
setMaximumEdgeLength :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setMaximumEdgeLength scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setMaximumEdgeLength:") retVoid [argCDouble (fromIntegral value)]

-- | smoothingMode
--
-- Defaults to SCNTessellationSmoothingModeNone.
--
-- ObjC selector: @- smoothingMode@
smoothingMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO SCNTessellationSmoothingMode
smoothingMode scnGeometryTessellator  =
  fmap (coerce :: CLong -> SCNTessellationSmoothingMode) $ sendMsg scnGeometryTessellator (mkSelector "smoothingMode") retCLong []

-- | smoothingMode
--
-- Defaults to SCNTessellationSmoothingModeNone.
--
-- ObjC selector: @- setSmoothingMode:@
setSmoothingMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> SCNTessellationSmoothingMode -> IO ()
setSmoothingMode scnGeometryTessellator  value =
  sendMsg scnGeometryTessellator (mkSelector "setSmoothingMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tessellationFactorScale@
tessellationFactorScaleSelector :: Selector
tessellationFactorScaleSelector = mkSelector "tessellationFactorScale"

-- | @Selector@ for @setTessellationFactorScale:@
setTessellationFactorScaleSelector :: Selector
setTessellationFactorScaleSelector = mkSelector "setTessellationFactorScale:"

-- | @Selector@ for @tessellationPartitionMode@
tessellationPartitionModeSelector :: Selector
tessellationPartitionModeSelector = mkSelector "tessellationPartitionMode"

-- | @Selector@ for @setTessellationPartitionMode:@
setTessellationPartitionModeSelector :: Selector
setTessellationPartitionModeSelector = mkSelector "setTessellationPartitionMode:"

-- | @Selector@ for @adaptive@
adaptiveSelector :: Selector
adaptiveSelector = mkSelector "adaptive"

-- | @Selector@ for @setAdaptive:@
setAdaptiveSelector :: Selector
setAdaptiveSelector = mkSelector "setAdaptive:"

-- | @Selector@ for @screenSpace@
screenSpaceSelector :: Selector
screenSpaceSelector = mkSelector "screenSpace"

-- | @Selector@ for @setScreenSpace:@
setScreenSpaceSelector :: Selector
setScreenSpaceSelector = mkSelector "setScreenSpace:"

-- | @Selector@ for @edgeTessellationFactor@
edgeTessellationFactorSelector :: Selector
edgeTessellationFactorSelector = mkSelector "edgeTessellationFactor"

-- | @Selector@ for @setEdgeTessellationFactor:@
setEdgeTessellationFactorSelector :: Selector
setEdgeTessellationFactorSelector = mkSelector "setEdgeTessellationFactor:"

-- | @Selector@ for @insideTessellationFactor@
insideTessellationFactorSelector :: Selector
insideTessellationFactorSelector = mkSelector "insideTessellationFactor"

-- | @Selector@ for @setInsideTessellationFactor:@
setInsideTessellationFactorSelector :: Selector
setInsideTessellationFactorSelector = mkSelector "setInsideTessellationFactor:"

-- | @Selector@ for @maximumEdgeLength@
maximumEdgeLengthSelector :: Selector
maximumEdgeLengthSelector = mkSelector "maximumEdgeLength"

-- | @Selector@ for @setMaximumEdgeLength:@
setMaximumEdgeLengthSelector :: Selector
setMaximumEdgeLengthSelector = mkSelector "setMaximumEdgeLength:"

-- | @Selector@ for @smoothingMode@
smoothingModeSelector :: Selector
smoothingModeSelector = mkSelector "smoothingMode"

-- | @Selector@ for @setSmoothingMode:@
setSmoothingModeSelector :: Selector
setSmoothingModeSelector = mkSelector "setSmoothingMode:"

