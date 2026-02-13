{-# LANGUAGE DataKinds #-}
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
  , entitySelector
  , obstaclesFromNodeBoundsSelector
  , obstaclesFromNodePhysicsBodiesSelector
  , obstaclesFromSpriteTextures_accuracySelector
  , setEntitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SpriteKit.Internal.Classes

-- | Returns an array of GKPolygonObstacles from a group of SKSpriteNode's textures in scene space.
--
-- See: GKObstacleGraph
--
-- ObjC selector: @+ obstaclesFromSpriteTextures:accuracy:@
obstaclesFromSpriteTextures_accuracy :: IsNSArray sprites => sprites -> CFloat -> IO (Id NSArray)
obstaclesFromSpriteTextures_accuracy sprites accuracy =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMessage cls' obstaclesFromSpriteTextures_accuracySelector (toNSArray sprites) accuracy

-- | Returns an array of GKPolygonObstacles from a group of SKNode's transformed bounds in scene space.
--
-- See: GKObstacleGraph
--
-- ObjC selector: @+ obstaclesFromNodeBounds:@
obstaclesFromNodeBounds :: IsNSArray nodes => nodes -> IO (Id NSArray)
obstaclesFromNodeBounds nodes =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMessage cls' obstaclesFromNodeBoundsSelector (toNSArray nodes)

-- | Returns an array of GKPolygonObstacles from a group of SKNode's physics bodies in scene space.
--
-- See: GKObstacleGraph
--
-- ObjC selector: @+ obstaclesFromNodePhysicsBodies:@
obstaclesFromNodePhysicsBodies :: IsNSArray nodes => nodes -> IO (Id NSArray)
obstaclesFromNodePhysicsBodies nodes =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMessage cls' obstaclesFromNodePhysicsBodiesSelector (toNSArray nodes)

-- | The GKEntity associated with the node via a GKSKNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- entity@
entity :: IsSKNode skNode => skNode -> IO RawId
entity skNode =
  sendMessage skNode entitySelector

-- | The GKEntity associated with the node via a GKSKNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- setEntity:@
setEntity :: IsSKNode skNode => skNode -> RawId -> IO ()
setEntity skNode value =
  sendMessage skNode setEntitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @obstaclesFromSpriteTextures:accuracy:@
obstaclesFromSpriteTextures_accuracySelector :: Selector '[Id NSArray, CFloat] (Id NSArray)
obstaclesFromSpriteTextures_accuracySelector = mkSelector "obstaclesFromSpriteTextures:accuracy:"

-- | @Selector@ for @obstaclesFromNodeBounds:@
obstaclesFromNodeBoundsSelector :: Selector '[Id NSArray] (Id NSArray)
obstaclesFromNodeBoundsSelector = mkSelector "obstaclesFromNodeBounds:"

-- | @Selector@ for @obstaclesFromNodePhysicsBodies:@
obstaclesFromNodePhysicsBodiesSelector :: Selector '[Id NSArray] (Id NSArray)
obstaclesFromNodePhysicsBodiesSelector = mkSelector "obstaclesFromNodePhysicsBodies:"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] RawId
entitySelector = mkSelector "entity"

-- | @Selector@ for @setEntity:@
setEntitySelector :: Selector '[RawId] ()
setEntitySelector = mkSelector "setEntity:"

