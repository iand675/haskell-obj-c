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

-- | Number of quads. Changes to this property require rebuilding the acceleration structure. This is an alias for the polygonCount property.
--
-- ObjC selector: @- quadrilateralCount@
quadrilateralCount :: IsMPSQuadrilateralAccelerationStructure mpsQuadrilateralAccelerationStructure => mpsQuadrilateralAccelerationStructure -> IO CULong
quadrilateralCount mpsQuadrilateralAccelerationStructure  =
  sendMsg mpsQuadrilateralAccelerationStructure (mkSelector "quadrilateralCount") retCULong []

-- | Number of quads. Changes to this property require rebuilding the acceleration structure. This is an alias for the polygonCount property.
--
-- ObjC selector: @- setQuadrilateralCount:@
setQuadrilateralCount :: IsMPSQuadrilateralAccelerationStructure mpsQuadrilateralAccelerationStructure => mpsQuadrilateralAccelerationStructure -> CULong -> IO ()
setQuadrilateralCount mpsQuadrilateralAccelerationStructure  value =
  sendMsg mpsQuadrilateralAccelerationStructure (mkSelector "setQuadrilateralCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quadrilateralCount@
quadrilateralCountSelector :: Selector
quadrilateralCountSelector = mkSelector "quadrilateralCount"

-- | @Selector@ for @setQuadrilateralCount:@
setQuadrilateralCountSelector :: Selector
setQuadrilateralCountSelector = mkSelector "setQuadrilateralCount:"

