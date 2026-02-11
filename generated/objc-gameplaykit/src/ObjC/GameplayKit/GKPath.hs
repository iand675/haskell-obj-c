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
  , pathWithGraphNodes_radiusSelector
  , initWithGraphNodes_radiusSelector
  , radiusSelector
  , setRadiusSelector
  , numPointsSelector
  , cyclicalSelector
  , setCyclicalSelector


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
    withObjCPtr graphNodes $ \raw_graphNodes ->
      sendClassMsg cls' (mkSelector "pathWithGraphNodes:radius:") (retPtr retVoid) [argPtr (castPtr raw_graphNodes :: Ptr ()), argCFloat (fromIntegral radius)] >>= retainedObject . castPtr

-- | @- initWithGraphNodes:radius:@
initWithGraphNodes_radius :: (IsGKPath gkPath, IsNSArray graphNodes) => gkPath -> graphNodes -> CFloat -> IO (Id GKPath)
initWithGraphNodes_radius gkPath  graphNodes radius =
withObjCPtr graphNodes $ \raw_graphNodes ->
    sendMsg gkPath (mkSelector "initWithGraphNodes:radius:") (retPtr retVoid) [argPtr (castPtr raw_graphNodes :: Ptr ()), argCFloat (fromIntegral radius)] >>= ownedObject . castPtr

-- | Radius of the pathway.  Defines a spatial area that the path occupies. This can be though of as the union between rectangles between all points, and circles at each point
--
-- ObjC selector: @- radius@
radius :: IsGKPath gkPath => gkPath -> IO CFloat
radius gkPath  =
  sendMsg gkPath (mkSelector "radius") retCFloat []

-- | Radius of the pathway.  Defines a spatial area that the path occupies. This can be though of as the union between rectangles between all points, and circles at each point
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKPath gkPath => gkPath -> CFloat -> IO ()
setRadius gkPath  value =
  sendMsg gkPath (mkSelector "setRadius:") retVoid [argCFloat (fromIntegral value)]

-- | Number of points in this path
--
-- ObjC selector: @- numPoints@
numPoints :: IsGKPath gkPath => gkPath -> IO CULong
numPoints gkPath  =
  sendMsg gkPath (mkSelector "numPoints") retCULong []

-- | Does this path loop back on itself, creating a cycle?
--
-- ObjC selector: @- cyclical@
cyclical :: IsGKPath gkPath => gkPath -> IO Bool
cyclical gkPath  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkPath (mkSelector "cyclical") retCULong []

-- | Does this path loop back on itself, creating a cycle?
--
-- ObjC selector: @- setCyclical:@
setCyclical :: IsGKPath gkPath => gkPath -> Bool -> IO ()
setCyclical gkPath  value =
  sendMsg gkPath (mkSelector "setCyclical:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pathWithGraphNodes:radius:@
pathWithGraphNodes_radiusSelector :: Selector
pathWithGraphNodes_radiusSelector = mkSelector "pathWithGraphNodes:radius:"

-- | @Selector@ for @initWithGraphNodes:radius:@
initWithGraphNodes_radiusSelector :: Selector
initWithGraphNodes_radiusSelector = mkSelector "initWithGraphNodes:radius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @numPoints@
numPointsSelector :: Selector
numPointsSelector = mkSelector "numPoints"

-- | @Selector@ for @cyclical@
cyclicalSelector :: Selector
cyclicalSelector = mkSelector "cyclical"

-- | @Selector@ for @setCyclical:@
setCyclicalSelector :: Selector
setCyclicalSelector = mkSelector "setCyclical:"

