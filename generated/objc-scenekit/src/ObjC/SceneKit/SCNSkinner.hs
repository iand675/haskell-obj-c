{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNSkinner
--
-- SCNSkinner controls the deformation of skinned geometries
--
-- Generated bindings for @SCNSkinner@.
module ObjC.SceneKit.SCNSkinner
  ( SCNSkinner
  , IsSCNSkinner(..)
  , skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndices
  , skeleton
  , setSkeleton
  , baseGeometryBindTransform
  , setBaseGeometryBindTransform
  , skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndicesSelector
  , skeletonSelector
  , setSkeletonSelector
  , baseGeometryBindTransformSelector
  , setBaseGeometryBindTransformSelector


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

-- | skinnerWithBaseGeometry:bones:boneInverseBindTransforms:boneWeights:boneIndices:
--
-- Creates and initialize a skinner instance with the specified parameters.
--
-- @baseGeometry@ — Specifies the base geometry used by the skinner
--
-- @bones@ — Specifies the array of bones.
--
-- @boneInverseBindTransforms@ — The inverse of the bone’s bind-space transformation matrix at the time the bind shape was bound to this bone.
--
-- @boneWeights@ — A buffer of weights. This contains the weights of every influence of every vertex. The number of influence per vertex is controlled by the number of component in the geometry source.
--
-- @boneIndices@ — A buffer of bone indexes. This buffer contains the corresponding index in the bones array for every weight in the weights buffer.
--
-- ObjC selector: @+ skinnerWithBaseGeometry:bones:boneInverseBindTransforms:boneWeights:boneIndices:@
skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndices :: (IsSCNGeometry baseGeometry, IsNSArray bones, IsNSArray boneInverseBindTransforms, IsSCNGeometrySource boneWeights, IsSCNGeometrySource boneIndices) => baseGeometry -> bones -> boneInverseBindTransforms -> boneWeights -> boneIndices -> IO (Id SCNSkinner)
skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndices baseGeometry bones boneInverseBindTransforms boneWeights boneIndices =
  do
    cls' <- getRequiredClass "SCNSkinner"
    withObjCPtr baseGeometry $ \raw_baseGeometry ->
      withObjCPtr bones $ \raw_bones ->
        withObjCPtr boneInverseBindTransforms $ \raw_boneInverseBindTransforms ->
          withObjCPtr boneWeights $ \raw_boneWeights ->
            withObjCPtr boneIndices $ \raw_boneIndices ->
              sendClassMsg cls' (mkSelector "skinnerWithBaseGeometry:bones:boneInverseBindTransforms:boneWeights:boneIndices:") (retPtr retVoid) [argPtr (castPtr raw_baseGeometry :: Ptr ()), argPtr (castPtr raw_bones :: Ptr ()), argPtr (castPtr raw_boneInverseBindTransforms :: Ptr ()), argPtr (castPtr raw_boneWeights :: Ptr ()), argPtr (castPtr raw_boneIndices :: Ptr ())] >>= retainedObject . castPtr

-- | skeleton
--
-- Specifies the skeleton of the receiver.
--
-- When setting a new skeleton, the new skeleton must have the same hierarchy of joints.
--
-- ObjC selector: @- skeleton@
skeleton :: IsSCNSkinner scnSkinner => scnSkinner -> IO (Id SCNNode)
skeleton scnSkinner  =
  sendMsg scnSkinner (mkSelector "skeleton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | skeleton
--
-- Specifies the skeleton of the receiver.
--
-- When setting a new skeleton, the new skeleton must have the same hierarchy of joints.
--
-- ObjC selector: @- setSkeleton:@
setSkeleton :: (IsSCNSkinner scnSkinner, IsSCNNode value) => scnSkinner -> value -> IO ()
setSkeleton scnSkinner  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnSkinner (mkSelector "setSkeleton:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | baseGeometryBindTransform
--
-- Specifies the transform of the baseGeometry at the time when the mesh was bound to a skeleton. This transforms the baseGeometry from object space to a space on which the skinning then applies.
--
-- ObjC selector: @- baseGeometryBindTransform@
baseGeometryBindTransform :: IsSCNSkinner scnSkinner => scnSkinner -> IO SCNMatrix4
baseGeometryBindTransform scnSkinner  =
  sendMsgStret scnSkinner (mkSelector "baseGeometryBindTransform") retSCNMatrix4 []

-- | baseGeometryBindTransform
--
-- Specifies the transform of the baseGeometry at the time when the mesh was bound to a skeleton. This transforms the baseGeometry from object space to a space on which the skinning then applies.
--
-- ObjC selector: @- setBaseGeometryBindTransform:@
setBaseGeometryBindTransform :: IsSCNSkinner scnSkinner => scnSkinner -> SCNMatrix4 -> IO ()
setBaseGeometryBindTransform scnSkinner  value =
  sendMsg scnSkinner (mkSelector "setBaseGeometryBindTransform:") retVoid [argSCNMatrix4 value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skinnerWithBaseGeometry:bones:boneInverseBindTransforms:boneWeights:boneIndices:@
skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndicesSelector :: Selector
skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndicesSelector = mkSelector "skinnerWithBaseGeometry:bones:boneInverseBindTransforms:boneWeights:boneIndices:"

-- | @Selector@ for @skeleton@
skeletonSelector :: Selector
skeletonSelector = mkSelector "skeleton"

-- | @Selector@ for @setSkeleton:@
setSkeletonSelector :: Selector
setSkeletonSelector = mkSelector "setSkeleton:"

-- | @Selector@ for @baseGeometryBindTransform@
baseGeometryBindTransformSelector :: Selector
baseGeometryBindTransformSelector = mkSelector "baseGeometryBindTransform"

-- | @Selector@ for @setBaseGeometryBindTransform:@
setBaseGeometryBindTransformSelector :: Selector
setBaseGeometryBindTransformSelector = mkSelector "setBaseGeometryBindTransform:"

