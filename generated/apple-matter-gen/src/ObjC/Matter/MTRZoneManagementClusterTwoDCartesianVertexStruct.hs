{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterTwoDCartesianVertexStruct@.
module ObjC.Matter.MTRZoneManagementClusterTwoDCartesianVertexStruct
  ( MTRZoneManagementClusterTwoDCartesianVertexStruct
  , IsMTRZoneManagementClusterTwoDCartesianVertexStruct(..)
  , x
  , setX
  , y
  , setY
  , setXSelector
  , setYSelector
  , xSelector
  , ySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- x@
x :: IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct => mtrZoneManagementClusterTwoDCartesianVertexStruct -> IO (Id NSNumber)
x mtrZoneManagementClusterTwoDCartesianVertexStruct =
  sendMessage mtrZoneManagementClusterTwoDCartesianVertexStruct xSelector

-- | @- setX:@
setX :: (IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct, IsNSNumber value) => mtrZoneManagementClusterTwoDCartesianVertexStruct -> value -> IO ()
setX mtrZoneManagementClusterTwoDCartesianVertexStruct value =
  sendMessage mtrZoneManagementClusterTwoDCartesianVertexStruct setXSelector (toNSNumber value)

-- | @- y@
y :: IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct => mtrZoneManagementClusterTwoDCartesianVertexStruct -> IO (Id NSNumber)
y mtrZoneManagementClusterTwoDCartesianVertexStruct =
  sendMessage mtrZoneManagementClusterTwoDCartesianVertexStruct ySelector

-- | @- setY:@
setY :: (IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct, IsNSNumber value) => mtrZoneManagementClusterTwoDCartesianVertexStruct -> value -> IO ()
setY mtrZoneManagementClusterTwoDCartesianVertexStruct value =
  sendMessage mtrZoneManagementClusterTwoDCartesianVertexStruct setYSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @x@
xSelector :: Selector '[] (Id NSNumber)
xSelector = mkSelector "x"

-- | @Selector@ for @setX:@
setXSelector :: Selector '[Id NSNumber] ()
setXSelector = mkSelector "setX:"

-- | @Selector@ for @y@
ySelector :: Selector '[] (Id NSNumber)
ySelector = mkSelector "y"

-- | @Selector@ for @setY:@
setYSelector :: Selector '[Id NSNumber] ()
setYSelector = mkSelector "setY:"

