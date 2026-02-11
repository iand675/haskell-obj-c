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
  , nodeSelector
  , geometryIndexSelector
  , faceIndexSelector
  , localCoordinatesSelector
  , worldCoordinatesSelector
  , localNormalSelector
  , worldNormalSelector
  , modelTransformSelector
  , boneNodeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | The hit node.
--
-- ObjC selector: @- node@
node :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO (Id SCNNode)
node scnHitTestResult  =
  sendMsg scnHitTestResult (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Index of the hit geometry element.
--
-- ObjC selector: @- geometryIndex@
geometryIndex :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO CLong
geometryIndex scnHitTestResult  =
  sendMsg scnHitTestResult (mkSelector "geometryIndex") retCLong []

-- | Index of the hit primitive of the geometry element.
--
-- ObjC selector: @- faceIndex@
faceIndex :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO CLong
faceIndex scnHitTestResult  =
  sendMsg scnHitTestResult (mkSelector "faceIndex") retCLong []

-- | Intersection point in the node's local coordinate system.
--
-- ObjC selector: @- localCoordinates@
localCoordinates :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
localCoordinates scnHitTestResult  =
  sendMsgStret scnHitTestResult (mkSelector "localCoordinates") retSCNVector3 []

-- | Intersection point in the world coordinate system.
--
-- ObjC selector: @- worldCoordinates@
worldCoordinates :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
worldCoordinates scnHitTestResult  =
  sendMsgStret scnHitTestResult (mkSelector "worldCoordinates") retSCNVector3 []

-- | Intersection normal in the node's local coordinate system.
--
-- ObjC selector: @- localNormal@
localNormal :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
localNormal scnHitTestResult  =
  sendMsgStret scnHitTestResult (mkSelector "localNormal") retSCNVector3 []

-- | Intersection normal in the world coordinate system.
--
-- ObjC selector: @- worldNormal@
worldNormal :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNVector3
worldNormal scnHitTestResult  =
  sendMsgStret scnHitTestResult (mkSelector "worldNormal") retSCNVector3 []

-- | World transform of the hit node.
--
-- ObjC selector: @- modelTransform@
modelTransform :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO SCNMatrix4
modelTransform scnHitTestResult  =
  sendMsgStret scnHitTestResult (mkSelector "modelTransform") retSCNMatrix4 []

-- | The hit bone. Only available if the node hit has a SCNSkinner attached.
--
-- ObjC selector: @- boneNode@
boneNode :: IsSCNHitTestResult scnHitTestResult => scnHitTestResult -> IO (Id SCNNode)
boneNode scnHitTestResult  =
  sendMsg scnHitTestResult (mkSelector "boneNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @geometryIndex@
geometryIndexSelector :: Selector
geometryIndexSelector = mkSelector "geometryIndex"

-- | @Selector@ for @faceIndex@
faceIndexSelector :: Selector
faceIndexSelector = mkSelector "faceIndex"

-- | @Selector@ for @localCoordinates@
localCoordinatesSelector :: Selector
localCoordinatesSelector = mkSelector "localCoordinates"

-- | @Selector@ for @worldCoordinates@
worldCoordinatesSelector :: Selector
worldCoordinatesSelector = mkSelector "worldCoordinates"

-- | @Selector@ for @localNormal@
localNormalSelector :: Selector
localNormalSelector = mkSelector "localNormal"

-- | @Selector@ for @worldNormal@
worldNormalSelector :: Selector
worldNormalSelector = mkSelector "worldNormal"

-- | @Selector@ for @modelTransform@
modelTransformSelector :: Selector
modelTransformSelector = mkSelector "modelTransform"

-- | @Selector@ for @boneNode@
boneNodeSelector :: Selector
boneNodeSelector = mkSelector "boneNode"

