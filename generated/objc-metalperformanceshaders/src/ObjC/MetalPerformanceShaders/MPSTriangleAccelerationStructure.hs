{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An acceleration structure built over triangles
--
-- See MPSPolygonAccelerationStructure for more information
--
-- Generated bindings for @MPSTriangleAccelerationStructure@.
module ObjC.MetalPerformanceShaders.MPSTriangleAccelerationStructure
  ( MPSTriangleAccelerationStructure
  , IsMPSTriangleAccelerationStructure(..)
  , triangleCount
  , setTriangleCount
  , triangleCountSelector
  , setTriangleCountSelector


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

-- | Number of triangles. Changes to this property require rebuilding the acceleration structure.
--
-- Note that this property is an alias for the polygonCount property.
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMPSTriangleAccelerationStructure mpsTriangleAccelerationStructure => mpsTriangleAccelerationStructure -> IO CULong
triangleCount mpsTriangleAccelerationStructure  =
  sendMsg mpsTriangleAccelerationStructure (mkSelector "triangleCount") retCULong []

-- | Number of triangles. Changes to this property require rebuilding the acceleration structure.
--
-- Note that this property is an alias for the polygonCount property.
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMPSTriangleAccelerationStructure mpsTriangleAccelerationStructure => mpsTriangleAccelerationStructure -> CULong -> IO ()
setTriangleCount mpsTriangleAccelerationStructure  value =
  sendMsg mpsTriangleAccelerationStructure (mkSelector "setTriangleCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triangleCount@
triangleCountSelector :: Selector
triangleCountSelector = mkSelector "triangleCount"

-- | @Selector@ for @setTriangleCount:@
setTriangleCountSelector :: Selector
setTriangleCountSelector = mkSelector "setTriangleCount:"

