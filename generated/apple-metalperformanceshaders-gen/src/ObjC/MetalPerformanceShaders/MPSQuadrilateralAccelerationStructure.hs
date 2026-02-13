{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An acceleration structure built over quadrilaterals
--
-- See MPSPolygonAccelerationStructure for more information
--
-- Generated bindings for @MPSQuadrilateralAccelerationStructure@.
module ObjC.MetalPerformanceShaders.MPSQuadrilateralAccelerationStructure
  ( MPSQuadrilateralAccelerationStructure
  , IsMPSQuadrilateralAccelerationStructure(..)
  , quadrilateralCount
  , setQuadrilateralCount
  , quadrilateralCountSelector
  , setQuadrilateralCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Number of quads. Changes to this property require rebuilding the acceleration structure. This is an alias for the polygonCount property.
--
-- ObjC selector: @- quadrilateralCount@
quadrilateralCount :: IsMPSQuadrilateralAccelerationStructure mpsQuadrilateralAccelerationStructure => mpsQuadrilateralAccelerationStructure -> IO CULong
quadrilateralCount mpsQuadrilateralAccelerationStructure =
  sendMessage mpsQuadrilateralAccelerationStructure quadrilateralCountSelector

-- | Number of quads. Changes to this property require rebuilding the acceleration structure. This is an alias for the polygonCount property.
--
-- ObjC selector: @- setQuadrilateralCount:@
setQuadrilateralCount :: IsMPSQuadrilateralAccelerationStructure mpsQuadrilateralAccelerationStructure => mpsQuadrilateralAccelerationStructure -> CULong -> IO ()
setQuadrilateralCount mpsQuadrilateralAccelerationStructure value =
  sendMessage mpsQuadrilateralAccelerationStructure setQuadrilateralCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quadrilateralCount@
quadrilateralCountSelector :: Selector '[] CULong
quadrilateralCountSelector = mkSelector "quadrilateralCount"

-- | @Selector@ for @setQuadrilateralCount:@
setQuadrilateralCountSelector :: Selector '[CULong] ()
setQuadrilateralCountSelector = mkSelector "setQuadrilateralCount:"

