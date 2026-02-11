{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsShape
--
-- SCNPhysicsShape represents the shape of a physics body.
--
-- Generated bindings for @SCNPhysicsShape@.
module ObjC.SceneKit.SCNPhysicsShape
  ( SCNPhysicsShape
  , IsSCNPhysicsShape(..)
  , shapeWithGeometry_options
  , shapeWithNode_options
  , shapeWithShapes_transforms
  , options
  , sourceObject
  , transforms
  , shapeWithGeometry_optionsSelector
  , shapeWithNode_optionsSelector
  , shapeWithShapes_transformsSelector
  , optionsSelector
  , sourceObjectSelector
  , transformsSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ shapeWithGeometry:options:@
shapeWithGeometry_options :: (IsSCNGeometry geometry, IsNSDictionary options) => geometry -> options -> IO (Id SCNPhysicsShape)
shapeWithGeometry_options geometry options =
  do
    cls' <- getRequiredClass "SCNPhysicsShape"
    withObjCPtr geometry $ \raw_geometry ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "shapeWithGeometry:options:") (retPtr retVoid) [argPtr (castPtr raw_geometry :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ shapeWithNode:options:@
shapeWithNode_options :: (IsSCNNode node, IsNSDictionary options) => node -> options -> IO (Id SCNPhysicsShape)
shapeWithNode_options node options =
  do
    cls' <- getRequiredClass "SCNPhysicsShape"
    withObjCPtr node $ \raw_node ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "shapeWithNode:options:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ shapeWithShapes:transforms:@
shapeWithShapes_transforms :: (IsNSArray shapes, IsNSArray transforms) => shapes -> transforms -> IO (Id SCNPhysicsShape)
shapeWithShapes_transforms shapes transforms =
  do
    cls' <- getRequiredClass "SCNPhysicsShape"
    withObjCPtr shapes $ \raw_shapes ->
      withObjCPtr transforms $ \raw_transforms ->
        sendClassMsg cls' (mkSelector "shapeWithShapes:transforms:") (retPtr retVoid) [argPtr (castPtr raw_shapes :: Ptr ()), argPtr (castPtr raw_transforms :: Ptr ())] >>= retainedObject . castPtr

-- | @- options@
options :: IsSCNPhysicsShape scnPhysicsShape => scnPhysicsShape -> IO (Id NSDictionary)
options scnPhysicsShape  =
  sendMsg scnPhysicsShape (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceObject@
sourceObject :: IsSCNPhysicsShape scnPhysicsShape => scnPhysicsShape -> IO RawId
sourceObject scnPhysicsShape  =
  fmap (RawId . castPtr) $ sendMsg scnPhysicsShape (mkSelector "sourceObject") (retPtr retVoid) []

-- | @- transforms@
transforms :: IsSCNPhysicsShape scnPhysicsShape => scnPhysicsShape -> IO (Id NSArray)
transforms scnPhysicsShape  =
  sendMsg scnPhysicsShape (mkSelector "transforms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shapeWithGeometry:options:@
shapeWithGeometry_optionsSelector :: Selector
shapeWithGeometry_optionsSelector = mkSelector "shapeWithGeometry:options:"

-- | @Selector@ for @shapeWithNode:options:@
shapeWithNode_optionsSelector :: Selector
shapeWithNode_optionsSelector = mkSelector "shapeWithNode:options:"

-- | @Selector@ for @shapeWithShapes:transforms:@
shapeWithShapes_transformsSelector :: Selector
shapeWithShapes_transformsSelector = mkSelector "shapeWithShapes:transforms:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @sourceObject@
sourceObjectSelector :: Selector
sourceObjectSelector = mkSelector "sourceObject"

-- | @Selector@ for @transforms@
transformsSelector :: Selector
transformsSelector = mkSelector "transforms"

