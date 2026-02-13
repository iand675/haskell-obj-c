{-# LANGUAGE DataKinds #-}
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
  , optionsSelector
  , shapeWithGeometry_optionsSelector
  , shapeWithNode_optionsSelector
  , shapeWithShapes_transformsSelector
  , sourceObjectSelector
  , transformsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ shapeWithGeometry:options:@
shapeWithGeometry_options :: (IsSCNGeometry geometry, IsNSDictionary options) => geometry -> options -> IO (Id SCNPhysicsShape)
shapeWithGeometry_options geometry options =
  do
    cls' <- getRequiredClass "SCNPhysicsShape"
    sendClassMessage cls' shapeWithGeometry_optionsSelector (toSCNGeometry geometry) (toNSDictionary options)

-- | @+ shapeWithNode:options:@
shapeWithNode_options :: (IsSCNNode node, IsNSDictionary options) => node -> options -> IO (Id SCNPhysicsShape)
shapeWithNode_options node options =
  do
    cls' <- getRequiredClass "SCNPhysicsShape"
    sendClassMessage cls' shapeWithNode_optionsSelector (toSCNNode node) (toNSDictionary options)

-- | @+ shapeWithShapes:transforms:@
shapeWithShapes_transforms :: (IsNSArray shapes, IsNSArray transforms) => shapes -> transforms -> IO (Id SCNPhysicsShape)
shapeWithShapes_transforms shapes transforms =
  do
    cls' <- getRequiredClass "SCNPhysicsShape"
    sendClassMessage cls' shapeWithShapes_transformsSelector (toNSArray shapes) (toNSArray transforms)

-- | @- options@
options :: IsSCNPhysicsShape scnPhysicsShape => scnPhysicsShape -> IO (Id NSDictionary)
options scnPhysicsShape =
  sendMessage scnPhysicsShape optionsSelector

-- | @- sourceObject@
sourceObject :: IsSCNPhysicsShape scnPhysicsShape => scnPhysicsShape -> IO RawId
sourceObject scnPhysicsShape =
  sendMessage scnPhysicsShape sourceObjectSelector

-- | @- transforms@
transforms :: IsSCNPhysicsShape scnPhysicsShape => scnPhysicsShape -> IO (Id NSArray)
transforms scnPhysicsShape =
  sendMessage scnPhysicsShape transformsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shapeWithGeometry:options:@
shapeWithGeometry_optionsSelector :: Selector '[Id SCNGeometry, Id NSDictionary] (Id SCNPhysicsShape)
shapeWithGeometry_optionsSelector = mkSelector "shapeWithGeometry:options:"

-- | @Selector@ for @shapeWithNode:options:@
shapeWithNode_optionsSelector :: Selector '[Id SCNNode, Id NSDictionary] (Id SCNPhysicsShape)
shapeWithNode_optionsSelector = mkSelector "shapeWithNode:options:"

-- | @Selector@ for @shapeWithShapes:transforms:@
shapeWithShapes_transformsSelector :: Selector '[Id NSArray, Id NSArray] (Id SCNPhysicsShape)
shapeWithShapes_transformsSelector = mkSelector "shapeWithShapes:transforms:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSDictionary)
optionsSelector = mkSelector "options"

-- | @Selector@ for @sourceObject@
sourceObjectSelector :: Selector '[] RawId
sourceObjectSelector = mkSelector "sourceObject"

-- | @Selector@ for @transforms@
transformsSelector :: Selector '[] (Id NSArray)
transformsSelector = mkSelector "transforms"

