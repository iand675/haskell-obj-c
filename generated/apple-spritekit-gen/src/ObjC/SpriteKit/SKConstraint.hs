{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SKConstraints are evaluated each frame after actions and physics. The node's transform will be changed to staisfy the constarint
--
-- Generated bindings for @SKConstraint@.
module ObjC.SpriteKit.SKConstraint
  ( SKConstraint
  , IsSKConstraint(..)
  , positionX
  , positionY
  , positionX_Y
  , distance_toNode
  , zRotation
  , orientToNode_offset
  , enabled
  , setEnabled
  , referenceNode
  , setReferenceNode
  , distance_toNodeSelector
  , enabledSelector
  , orientToNode_offsetSelector
  , positionXSelector
  , positionX_YSelector
  , positionYSelector
  , referenceNodeSelector
  , setEnabledSelector
  , setReferenceNodeSelector
  , zRotationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Constrain the node's position to a range
--
-- ObjC selector: @+ positionX:@
positionX :: IsSKRange range => range -> IO (Id SKConstraint)
positionX range =
  do
    cls' <- getRequiredClass "SKConstraint"
    sendClassMessage cls' positionXSelector (toSKRange range)

-- | @+ positionY:@
positionY :: IsSKRange range => range -> IO (Id SKConstraint)
positionY range =
  do
    cls' <- getRequiredClass "SKConstraint"
    sendClassMessage cls' positionYSelector (toSKRange range)

-- | @+ positionX:Y:@
positionX_Y :: (IsSKRange xRange, IsSKRange yRange) => xRange -> yRange -> IO (Id SKConstraint)
positionX_Y xRange yRange =
  do
    cls' <- getRequiredClass "SKConstraint"
    sendClassMessage cls' positionX_YSelector (toSKRange xRange) (toSKRange yRange)

-- | Constrain the node's position to be within a distance of a point or node
--
-- ObjC selector: @+ distance:toNode:@
distance_toNode :: (IsSKRange range, IsSKNode node) => range -> node -> IO (Id SKConstraint)
distance_toNode range node =
  do
    cls' <- getRequiredClass "SKConstraint"
    sendClassMessage cls' distance_toNodeSelector (toSKRange range) (toSKNode node)

-- | Constrain the node's rotation to a range
--
-- ObjC selector: @+ zRotation:@
zRotation :: IsSKRange zRange => zRange -> IO (Id SKConstraint)
zRotation zRange =
  do
    cls' <- getRequiredClass "SKConstraint"
    sendClassMessage cls' zRotationSelector (toSKRange zRange)

-- | Constrain the node's rotation to orient to a point or node
--
-- ObjC selector: @+ orientToNode:offset:@
orientToNode_offset :: (IsSKNode node, IsSKRange radians) => node -> radians -> IO (Id SKConstraint)
orientToNode_offset node radians =
  do
    cls' <- getRequiredClass "SKConstraint"
    sendClassMessage cls' orientToNode_offsetSelector (toSKNode node) (toSKRange radians)

-- | @- enabled@
enabled :: IsSKConstraint skConstraint => skConstraint -> IO Bool
enabled skConstraint =
  sendMessage skConstraint enabledSelector

-- | @- setEnabled:@
setEnabled :: IsSKConstraint skConstraint => skConstraint -> Bool -> IO ()
setEnabled skConstraint value =
  sendMessage skConstraint setEnabledSelector value

-- | @- referenceNode@
referenceNode :: IsSKConstraint skConstraint => skConstraint -> IO (Id SKNode)
referenceNode skConstraint =
  sendMessage skConstraint referenceNodeSelector

-- | @- setReferenceNode:@
setReferenceNode :: (IsSKConstraint skConstraint, IsSKNode value) => skConstraint -> value -> IO ()
setReferenceNode skConstraint value =
  sendMessage skConstraint setReferenceNodeSelector (toSKNode value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @positionX:@
positionXSelector :: Selector '[Id SKRange] (Id SKConstraint)
positionXSelector = mkSelector "positionX:"

-- | @Selector@ for @positionY:@
positionYSelector :: Selector '[Id SKRange] (Id SKConstraint)
positionYSelector = mkSelector "positionY:"

-- | @Selector@ for @positionX:Y:@
positionX_YSelector :: Selector '[Id SKRange, Id SKRange] (Id SKConstraint)
positionX_YSelector = mkSelector "positionX:Y:"

-- | @Selector@ for @distance:toNode:@
distance_toNodeSelector :: Selector '[Id SKRange, Id SKNode] (Id SKConstraint)
distance_toNodeSelector = mkSelector "distance:toNode:"

-- | @Selector@ for @zRotation:@
zRotationSelector :: Selector '[Id SKRange] (Id SKConstraint)
zRotationSelector = mkSelector "zRotation:"

-- | @Selector@ for @orientToNode:offset:@
orientToNode_offsetSelector :: Selector '[Id SKNode, Id SKRange] (Id SKConstraint)
orientToNode_offsetSelector = mkSelector "orientToNode:offset:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @referenceNode@
referenceNodeSelector :: Selector '[] (Id SKNode)
referenceNodeSelector = mkSelector "referenceNode"

-- | @Selector@ for @setReferenceNode:@
setReferenceNodeSelector :: Selector '[Id SKNode] ()
setReferenceNodeSelector = mkSelector "setReferenceNode:"

