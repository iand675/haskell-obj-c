{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKNode@.
module ObjC.GameplayKit.SKNode
  ( SKNode
  , IsSKNode(..)
  , obstaclesFromSpriteTextures_accuracy
  , obstaclesFromNodeBounds
  , obstaclesFromNodePhysicsBodies
  , entity
  , setEntity
  , obstaclesFromSpriteTextures_accuracySelector
  , obstaclesFromNodeBoundsSelector
  , obstaclesFromNodePhysicsBodiesSelector
  , entitySelector
  , setEntitySelector


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

-- | Returns an array of GKPolygonObstacles from a group of SKSpriteNode's textures in scene space.
--
-- See: GKObstacleGraph
--
-- ObjC selector: @+ obstaclesFromSpriteTextures:accuracy:@
obstaclesFromSpriteTextures_accuracy :: IsNSArray sprites => sprites -> CFloat -> IO (Id NSArray)
obstaclesFromSpriteTextures_accuracy sprites accuracy =
  do
    cls' <- getRequiredClass "SKNode"
    withObjCPtr sprites $ \raw_sprites ->
      sendClassMsg cls' (mkSelector "obstaclesFromSpriteTextures:accuracy:") (retPtr retVoid) [argPtr (castPtr raw_sprites :: Ptr ()), argCFloat accuracy] >>= retainedObject . castPtr

-- | Returns an array of GKPolygonObstacles from a group of SKNode's transformed bounds in scene space.
--
-- See: GKObstacleGraph
--
-- ObjC selector: @+ obstaclesFromNodeBounds:@
obstaclesFromNodeBounds :: IsNSArray nodes => nodes -> IO (Id NSArray)
obstaclesFromNodeBounds nodes =
  do
    cls' <- getRequiredClass "SKNode"
    withObjCPtr nodes $ \raw_nodes ->
      sendClassMsg cls' (mkSelector "obstaclesFromNodeBounds:") (retPtr retVoid) [argPtr (castPtr raw_nodes :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an array of GKPolygonObstacles from a group of SKNode's physics bodies in scene space.
--
-- See: GKObstacleGraph
--
-- ObjC selector: @+ obstaclesFromNodePhysicsBodies:@
obstaclesFromNodePhysicsBodies :: IsNSArray nodes => nodes -> IO (Id NSArray)
obstaclesFromNodePhysicsBodies nodes =
  do
    cls' <- getRequiredClass "SKNode"
    withObjCPtr nodes $ \raw_nodes ->
      sendClassMsg cls' (mkSelector "obstaclesFromNodePhysicsBodies:") (retPtr retVoid) [argPtr (castPtr raw_nodes :: Ptr ())] >>= retainedObject . castPtr

-- | The GKEntity associated with the node via a GKSKNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- entity@
entity :: IsSKNode skNode => skNode -> IO RawId
entity skNode  =
    fmap (RawId . castPtr) $ sendMsg skNode (mkSelector "entity") (retPtr retVoid) []

-- | The GKEntity associated with the node via a GKSKNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- setEntity:@
setEntity :: IsSKNode skNode => skNode -> RawId -> IO ()
setEntity skNode  value =
    sendMsg skNode (mkSelector "setEntity:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @obstaclesFromSpriteTextures:accuracy:@
obstaclesFromSpriteTextures_accuracySelector :: Selector
obstaclesFromSpriteTextures_accuracySelector = mkSelector "obstaclesFromSpriteTextures:accuracy:"

-- | @Selector@ for @obstaclesFromNodeBounds:@
obstaclesFromNodeBoundsSelector :: Selector
obstaclesFromNodeBoundsSelector = mkSelector "obstaclesFromNodeBounds:"

-- | @Selector@ for @obstaclesFromNodePhysicsBodies:@
obstaclesFromNodePhysicsBodiesSelector :: Selector
obstaclesFromNodePhysicsBodiesSelector = mkSelector "obstaclesFromNodePhysicsBodies:"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @setEntity:@
setEntitySelector :: Selector
setEntitySelector = mkSelector "setEntity:"

