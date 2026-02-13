{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , adaptiveSelector
  , edgeTessellationFactorSelector
  , insideTessellationFactorSelector
  , maximumEdgeLengthSelector
  , screenSpaceSelector
  , setAdaptiveSelector
  , setEdgeTessellationFactorSelector
  , setInsideTessellationFactorSelector
  , setMaximumEdgeLengthSelector
  , setScreenSpaceSelector
  , setSmoothingModeSelector
  , setTessellationFactorScaleSelector
  , setTessellationPartitionModeSelector
  , smoothingModeSelector
  , tessellationFactorScaleSelector
  , tessellationPartitionModeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
tessellationFactorScale scnGeometryTessellator =
  sendMessage scnGeometryTessellator tessellationFactorScaleSelector

-- | tessellationFactorScale
--
-- Specifies the scale factor applied to the per-patch tessellation factors. Defaults to 1.
--
-- ObjC selector: @- setTessellationFactorScale:@
setTessellationFactorScale :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setTessellationFactorScale scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setTessellationFactorScaleSelector value

-- | tessellationPartitionMode
--
-- Specifies the tessellation partition mode. Defaults to MTLTessellationPartitionModeInteger.
--
-- ObjC selector: @- tessellationPartitionMode@
tessellationPartitionMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO MTLTessellationPartitionMode
tessellationPartitionMode scnGeometryTessellator =
  sendMessage scnGeometryTessellator tessellationPartitionModeSelector

-- | tessellationPartitionMode
--
-- Specifies the tessellation partition mode. Defaults to MTLTessellationPartitionModeInteger.
--
-- ObjC selector: @- setTessellationPartitionMode:@
setTessellationPartitionMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> MTLTessellationPartitionMode -> IO ()
setTessellationPartitionMode scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setTessellationPartitionModeSelector value

-- | adaptive
--
-- Specifies if the tessellation should be uniform or adaptive. Defaults to NO.
--
-- ObjC selector: @- adaptive@
adaptive :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO Bool
adaptive scnGeometryTessellator =
  sendMessage scnGeometryTessellator adaptiveSelector

-- | adaptive
--
-- Specifies if the tessellation should be uniform or adaptive. Defaults to NO.
--
-- ObjC selector: @- setAdaptive:@
setAdaptive :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> Bool -> IO ()
setAdaptive scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setAdaptiveSelector value

-- | screenspace
--
-- Specifies if the level of tessellation should be adapted in screenSpace. Defaults to NO.
--
-- ObjC selector: @- screenSpace@
screenSpace :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO Bool
screenSpace scnGeometryTessellator =
  sendMessage scnGeometryTessellator screenSpaceSelector

-- | screenspace
--
-- Specifies if the level of tessellation should be adapted in screenSpace. Defaults to NO.
--
-- ObjC selector: @- setScreenSpace:@
setScreenSpace :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> Bool -> IO ()
setScreenSpace scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setScreenSpaceSelector value

-- | edgeTessellationFactor
--
-- Specifies the edge tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- edgeTessellationFactor@
edgeTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
edgeTessellationFactor scnGeometryTessellator =
  sendMessage scnGeometryTessellator edgeTessellationFactorSelector

-- | edgeTessellationFactor
--
-- Specifies the edge tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- setEdgeTessellationFactor:@
setEdgeTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setEdgeTessellationFactor scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setEdgeTessellationFactorSelector value

-- | insideTessellationFactor
--
-- Specifies the inside tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- insideTessellationFactor@
insideTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
insideTessellationFactor scnGeometryTessellator =
  sendMessage scnGeometryTessellator insideTessellationFactorSelector

-- | insideTessellationFactor
--
-- Specifies the inside tessellation factor. Defaults to 1.
--
-- This has no effect for adaptive subdivision
--
-- ObjC selector: @- setInsideTessellationFactor:@
setInsideTessellationFactor :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setInsideTessellationFactor scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setInsideTessellationFactorSelector value

-- | maximumEdgeLength
--
-- Specifies the maximum edge length. Defaults to 1.
--
-- This has no effect for non-adaptive subdivision
--
-- ObjC selector: @- maximumEdgeLength@
maximumEdgeLength :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO CDouble
maximumEdgeLength scnGeometryTessellator =
  sendMessage scnGeometryTessellator maximumEdgeLengthSelector

-- | maximumEdgeLength
--
-- Specifies the maximum edge length. Defaults to 1.
--
-- This has no effect for non-adaptive subdivision
--
-- ObjC selector: @- setMaximumEdgeLength:@
setMaximumEdgeLength :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> CDouble -> IO ()
setMaximumEdgeLength scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setMaximumEdgeLengthSelector value

-- | smoothingMode
--
-- Defaults to SCNTessellationSmoothingModeNone.
--
-- ObjC selector: @- smoothingMode@
smoothingMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> IO SCNTessellationSmoothingMode
smoothingMode scnGeometryTessellator =
  sendMessage scnGeometryTessellator smoothingModeSelector

-- | smoothingMode
--
-- Defaults to SCNTessellationSmoothingModeNone.
--
-- ObjC selector: @- setSmoothingMode:@
setSmoothingMode :: IsSCNGeometryTessellator scnGeometryTessellator => scnGeometryTessellator -> SCNTessellationSmoothingMode -> IO ()
setSmoothingMode scnGeometryTessellator value =
  sendMessage scnGeometryTessellator setSmoothingModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tessellationFactorScale@
tessellationFactorScaleSelector :: Selector '[] CDouble
tessellationFactorScaleSelector = mkSelector "tessellationFactorScale"

-- | @Selector@ for @setTessellationFactorScale:@
setTessellationFactorScaleSelector :: Selector '[CDouble] ()
setTessellationFactorScaleSelector = mkSelector "setTessellationFactorScale:"

-- | @Selector@ for @tessellationPartitionMode@
tessellationPartitionModeSelector :: Selector '[] MTLTessellationPartitionMode
tessellationPartitionModeSelector = mkSelector "tessellationPartitionMode"

-- | @Selector@ for @setTessellationPartitionMode:@
setTessellationPartitionModeSelector :: Selector '[MTLTessellationPartitionMode] ()
setTessellationPartitionModeSelector = mkSelector "setTessellationPartitionMode:"

-- | @Selector@ for @adaptive@
adaptiveSelector :: Selector '[] Bool
adaptiveSelector = mkSelector "adaptive"

-- | @Selector@ for @setAdaptive:@
setAdaptiveSelector :: Selector '[Bool] ()
setAdaptiveSelector = mkSelector "setAdaptive:"

-- | @Selector@ for @screenSpace@
screenSpaceSelector :: Selector '[] Bool
screenSpaceSelector = mkSelector "screenSpace"

-- | @Selector@ for @setScreenSpace:@
setScreenSpaceSelector :: Selector '[Bool] ()
setScreenSpaceSelector = mkSelector "setScreenSpace:"

-- | @Selector@ for @edgeTessellationFactor@
edgeTessellationFactorSelector :: Selector '[] CDouble
edgeTessellationFactorSelector = mkSelector "edgeTessellationFactor"

-- | @Selector@ for @setEdgeTessellationFactor:@
setEdgeTessellationFactorSelector :: Selector '[CDouble] ()
setEdgeTessellationFactorSelector = mkSelector "setEdgeTessellationFactor:"

-- | @Selector@ for @insideTessellationFactor@
insideTessellationFactorSelector :: Selector '[] CDouble
insideTessellationFactorSelector = mkSelector "insideTessellationFactor"

-- | @Selector@ for @setInsideTessellationFactor:@
setInsideTessellationFactorSelector :: Selector '[CDouble] ()
setInsideTessellationFactorSelector = mkSelector "setInsideTessellationFactor:"

-- | @Selector@ for @maximumEdgeLength@
maximumEdgeLengthSelector :: Selector '[] CDouble
maximumEdgeLengthSelector = mkSelector "maximumEdgeLength"

-- | @Selector@ for @setMaximumEdgeLength:@
setMaximumEdgeLengthSelector :: Selector '[CDouble] ()
setMaximumEdgeLengthSelector = mkSelector "setMaximumEdgeLength:"

-- | @Selector@ for @smoothingMode@
smoothingModeSelector :: Selector '[] SCNTessellationSmoothingMode
smoothingModeSelector = mkSelector "smoothingMode"

-- | @Selector@ for @setSmoothingMode:@
setSmoothingModeSelector :: Selector '[SCNTessellationSmoothingMode] ()
setSmoothingModeSelector = mkSelector "setSmoothingMode:"

