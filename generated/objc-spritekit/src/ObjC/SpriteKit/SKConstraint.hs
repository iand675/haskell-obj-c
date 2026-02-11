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
  , positionXSelector
  , positionYSelector
  , positionX_YSelector
  , distance_toNodeSelector
  , zRotationSelector
  , orientToNode_offsetSelector
  , enabledSelector
  , setEnabledSelector
  , referenceNodeSelector
  , setReferenceNodeSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Constrain the node's position to a range
--
-- ObjC selector: @+ positionX:@
positionX :: IsSKRange range => range -> IO (Id SKConstraint)
positionX range =
  do
    cls' <- getRequiredClass "SKConstraint"
    withObjCPtr range $ \raw_range ->
      sendClassMsg cls' (mkSelector "positionX:") (retPtr retVoid) [argPtr (castPtr raw_range :: Ptr ())] >>= retainedObject . castPtr

-- | @+ positionY:@
positionY :: IsSKRange range => range -> IO (Id SKConstraint)
positionY range =
  do
    cls' <- getRequiredClass "SKConstraint"
    withObjCPtr range $ \raw_range ->
      sendClassMsg cls' (mkSelector "positionY:") (retPtr retVoid) [argPtr (castPtr raw_range :: Ptr ())] >>= retainedObject . castPtr

-- | @+ positionX:Y:@
positionX_Y :: (IsSKRange xRange, IsSKRange yRange) => xRange -> yRange -> IO (Id SKConstraint)
positionX_Y xRange yRange =
  do
    cls' <- getRequiredClass "SKConstraint"
    withObjCPtr xRange $ \raw_xRange ->
      withObjCPtr yRange $ \raw_yRange ->
        sendClassMsg cls' (mkSelector "positionX:Y:") (retPtr retVoid) [argPtr (castPtr raw_xRange :: Ptr ()), argPtr (castPtr raw_yRange :: Ptr ())] >>= retainedObject . castPtr

-- | Constrain the node's position to be within a distance of a point or node
--
-- ObjC selector: @+ distance:toNode:@
distance_toNode :: (IsSKRange range, IsSKNode node) => range -> node -> IO (Id SKConstraint)
distance_toNode range node =
  do
    cls' <- getRequiredClass "SKConstraint"
    withObjCPtr range $ \raw_range ->
      withObjCPtr node $ \raw_node ->
        sendClassMsg cls' (mkSelector "distance:toNode:") (retPtr retVoid) [argPtr (castPtr raw_range :: Ptr ()), argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | Constrain the node's rotation to a range
--
-- ObjC selector: @+ zRotation:@
zRotation :: IsSKRange zRange => zRange -> IO (Id SKConstraint)
zRotation zRange =
  do
    cls' <- getRequiredClass "SKConstraint"
    withObjCPtr zRange $ \raw_zRange ->
      sendClassMsg cls' (mkSelector "zRotation:") (retPtr retVoid) [argPtr (castPtr raw_zRange :: Ptr ())] >>= retainedObject . castPtr

-- | Constrain the node's rotation to orient to a point or node
--
-- ObjC selector: @+ orientToNode:offset:@
orientToNode_offset :: (IsSKNode node, IsSKRange radians) => node -> radians -> IO (Id SKConstraint)
orientToNode_offset node radians =
  do
    cls' <- getRequiredClass "SKConstraint"
    withObjCPtr node $ \raw_node ->
      withObjCPtr radians $ \raw_radians ->
        sendClassMsg cls' (mkSelector "orientToNode:offset:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ()), argPtr (castPtr raw_radians :: Ptr ())] >>= retainedObject . castPtr

-- | @- enabled@
enabled :: IsSKConstraint skConstraint => skConstraint -> IO Bool
enabled skConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skConstraint (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsSKConstraint skConstraint => skConstraint -> Bool -> IO ()
setEnabled skConstraint  value =
  sendMsg skConstraint (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- referenceNode@
referenceNode :: IsSKConstraint skConstraint => skConstraint -> IO (Id SKNode)
referenceNode skConstraint  =
  sendMsg skConstraint (mkSelector "referenceNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferenceNode:@
setReferenceNode :: (IsSKConstraint skConstraint, IsSKNode value) => skConstraint -> value -> IO ()
setReferenceNode skConstraint  value =
withObjCPtr value $ \raw_value ->
    sendMsg skConstraint (mkSelector "setReferenceNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @positionX:@
positionXSelector :: Selector
positionXSelector = mkSelector "positionX:"

-- | @Selector@ for @positionY:@
positionYSelector :: Selector
positionYSelector = mkSelector "positionY:"

-- | @Selector@ for @positionX:Y:@
positionX_YSelector :: Selector
positionX_YSelector = mkSelector "positionX:Y:"

-- | @Selector@ for @distance:toNode:@
distance_toNodeSelector :: Selector
distance_toNodeSelector = mkSelector "distance:toNode:"

-- | @Selector@ for @zRotation:@
zRotationSelector :: Selector
zRotationSelector = mkSelector "zRotation:"

-- | @Selector@ for @orientToNode:offset:@
orientToNode_offsetSelector :: Selector
orientToNode_offsetSelector = mkSelector "orientToNode:offset:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @referenceNode@
referenceNodeSelector :: Selector
referenceNodeSelector = mkSelector "referenceNode"

-- | @Selector@ for @setReferenceNode:@
setReferenceNodeSelector :: Selector
setReferenceNodeSelector = mkSelector "setReferenceNode:"

