{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNHitTestResult
--
-- Results returned by the hit-test methods.
--
-- Generated bindings for @SCNHitTestResult@.
module ObjC.SceneKit.SCNHitTestResult
  ( SCNHitTestResult
  , IsSCNHitTestResult(..)
  , node
  , geometryIndex
  , faceIndex
  , localCoordinates
  , worldCoordinates
  , localNormal
  , worldNormal
  , modelTransform
  , boneNode
  , boneNodeSelector
  , faceIndexSelector
  , geometryIndexSelector
  , localCoordinatesSelector
  , localNormalSelector
  , modelTransformSelector
  , nodeSelector
  , worldCoordinatesSelector
  , worldNormalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | The hit node.
--
-- ObjC selector: @- node@
node :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO (Id SCNNode)
node scnHitTestResult =
  sendMessage scnHitTestResult nodeSelector

-- | Index of the hit geometry element.
--
-- ObjC selector: @- geometryIndex@
geometryIndex :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO CLong
geometryIndex scnHitTestResult =
  sendMessage scnHitTestResult geometryIndexSelector

-- | Index of the hit primitive of the geometry element.
--
-- ObjC selector: @- faceIndex@
faceIndex :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO CLong
faceIndex scnHitTestResult =
  sendMessage scnHitTestResult faceIndexSelector

-- | Intersection point in the node's local coordinate system.
--
-- ObjC selector: @- localCoordinates@
localCoordinates :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
localCoordinates scnHitTestResult =
  sendMessage scnHitTestResult localCoordinatesSelector

-- | Intersection point in the world coordinate system.
--
-- ObjC selector: @- worldCoordinates@
worldCoordinates :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
worldCoordinates scnHitTestResult =
  sendMessage scnHitTestResult worldCoordinatesSelector

-- | Intersection normal in the node's local coordinate system.
--
-- ObjC selector: @- localNormal@
localNormal :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
localNormal scnHitTestResult =
  sendMessage scnHitTestResult localNormalSelector

-- | Intersection normal in the world coordinate system.
--
-- ObjC selector: @- worldNormal@
worldNormal :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
worldNormal scnHitTestResult =
  sendMessage scnHitTestResult worldNormalSelector

-- | World transform of the hit node.
--
-- ObjC selector: @- modelTransform@
modelTransform :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNMatrix4
modelTransform scnHitTestResult =
  sendMessage scnHitTestResult modelTransformSelector

-- | The hit bone. Only available if the node hit has a SCNSkinner attached.
--
-- ObjC selector: @- boneNode@
boneNode :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO (Id SCNNode)
boneNode scnHitTestResult =
  sendMessage scnHitTestResult boneNodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id SCNNode)
nodeSelector = mkSelector "node"

-- | @Selector@ for @geometryIndex@
geometryIndexSelector :: Selector '[] CLong
geometryIndexSelector = mkSelector "geometryIndex"

-- | @Selector@ for @faceIndex@
faceIndexSelector :: Selector '[] CLong
faceIndexSelector = mkSelector "faceIndex"

-- | @Selector@ for @localCoordinates@
localCoordinatesSelector :: Selector '[] SCNVector3
localCoordinatesSelector = mkSelector "localCoordinates"

-- | @Selector@ for @worldCoordinates@
worldCoordinatesSelector :: Selector '[] SCNVector3
worldCoordinatesSelector = mkSelector "worldCoordinates"

-- | @Selector@ for @localNormal@
localNormalSelector :: Selector '[] SCNVector3
localNormalSelector = mkSelector "localNormal"

-- | @Selector@ for @worldNormal@
worldNormalSelector :: Selector '[] SCNVector3
worldNormalSelector = mkSelector "worldNormal"

-- | @Selector@ for @modelTransform@
modelTransformSelector :: Selector '[] SCNMatrix4
modelTransformSelector = mkSelector "modelTransform"

-- | @Selector@ for @boneNode@
boneNodeSelector :: Selector '[] (Id SCNNode)
boneNodeSelector = mkSelector "boneNode"

