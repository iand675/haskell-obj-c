{-# LANGUAGE DataKinds #-}
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
  , setTriangleCountSelector
  , triangleCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
triangleCount mpsTriangleAccelerationStructure =
  sendMessage mpsTriangleAccelerationStructure triangleCountSelector

-- | Number of triangles. Changes to this property require rebuilding the acceleration structure.
--
-- Note that this property is an alias for the polygonCount property.
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMPSTriangleAccelerationStructure mpsTriangleAccelerationStructure => mpsTriangleAccelerationStructure -> CULong -> IO ()
setTriangleCount mpsTriangleAccelerationStructure value =
  sendMessage mpsTriangleAccelerationStructure setTriangleCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triangleCount@
triangleCountSelector :: Selector '[] CULong
triangleCountSelector = mkSelector "triangleCount"

-- | @Selector@ for @setTriangleCount:@
setTriangleCountSelector :: Selector '[CULong] ()
setTriangleCountSelector = mkSelector "setTriangleCount:"

