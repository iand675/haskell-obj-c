{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKPath@.
module ObjC.GameplayKit.GKPath
  ( GKPath
  , IsGKPath(..)
  , pathWithGraphNodes_radius
  , initWithGraphNodes_radius
  , radius
  , setRadius
  , numPoints
  , cyclical
  , setCyclical
  , cyclicalSelector
  , initWithGraphNodes_radiusSelector
  , numPointsSelector
  , pathWithGraphNodes_radiusSelector
  , radiusSelector
  , setCyclicalSelector
  , setRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a path from an array of graph nodes (often a result of pathfinding) Accepts GKGraphNode2D and GKGraphNode3D Cyclical is set to NO
--
-- @graphNodes@ — an array of graph nodes to make a path from
--
-- @radius@ — radius of the path to create
--
-- See: GKGraphNode
--
-- ObjC selector: @+ pathWithGraphNodes:radius:@
pathWithGraphNodes_radius :: IsNSArray graphNodes => graphNodes -> CFloat -> IO (Id GKPath)
pathWithGraphNodes_radius graphNodes radius =
  do
    cls' <- getRequiredClass "GKPath"
    sendClassMessage cls' pathWithGraphNodes_radiusSelector (toNSArray graphNodes) radius

-- | @- initWithGraphNodes:radius:@
initWithGraphNodes_radius :: (IsGKPath gkPath, IsNSArray graphNodes) => gkPath -> graphNodes -> CFloat -> IO (Id GKPath)
initWithGraphNodes_radius gkPath graphNodes radius =
  sendOwnedMessage gkPath initWithGraphNodes_radiusSelector (toNSArray graphNodes) radius

-- | Radius of the pathway.  Defines a spatial area that the path occupies. This can be though of as the union between rectangles between all points, and circles at each point
--
-- ObjC selector: @- radius@
radius :: IsGKPath gkPath => gkPath -> IO CFloat
radius gkPath =
  sendMessage gkPath radiusSelector

-- | Radius of the pathway.  Defines a spatial area that the path occupies. This can be though of as the union between rectangles between all points, and circles at each point
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKPath gkPath => gkPath -> CFloat -> IO ()
setRadius gkPath value =
  sendMessage gkPath setRadiusSelector value

-- | Number of points in this path
--
-- ObjC selector: @- numPoints@
numPoints :: IsGKPath gkPath => gkPath -> IO CULong
numPoints gkPath =
  sendMessage gkPath numPointsSelector

-- | Does this path loop back on itself, creating a cycle?
--
-- ObjC selector: @- cyclical@
cyclical :: IsGKPath gkPath => gkPath -> IO Bool
cyclical gkPath =
  sendMessage gkPath cyclicalSelector

-- | Does this path loop back on itself, creating a cycle?
--
-- ObjC selector: @- setCyclical:@
setCyclical :: IsGKPath gkPath => gkPath -> Bool -> IO ()
setCyclical gkPath value =
  sendMessage gkPath setCyclicalSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pathWithGraphNodes:radius:@
pathWithGraphNodes_radiusSelector :: Selector '[Id NSArray, CFloat] (Id GKPath)
pathWithGraphNodes_radiusSelector = mkSelector "pathWithGraphNodes:radius:"

-- | @Selector@ for @initWithGraphNodes:radius:@
initWithGraphNodes_radiusSelector :: Selector '[Id NSArray, CFloat] (Id GKPath)
initWithGraphNodes_radiusSelector = mkSelector "initWithGraphNodes:radius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CFloat
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CFloat] ()
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @numPoints@
numPointsSelector :: Selector '[] CULong
numPointsSelector = mkSelector "numPoints"

-- | @Selector@ for @cyclical@
cyclicalSelector :: Selector '[] Bool
cyclicalSelector = mkSelector "cyclical"

-- | @Selector@ for @setCyclical:@
setCyclicalSelector :: Selector '[Bool] ()
setCyclicalSelector = mkSelector "setCyclical:"

