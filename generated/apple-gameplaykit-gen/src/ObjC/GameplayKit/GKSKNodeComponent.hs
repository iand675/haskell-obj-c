{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A component that encapsulates a SpriteKit node.
--
-- Generated bindings for @GKSKNodeComponent@.
module ObjC.GameplayKit.GKSKNodeComponent
  ( GKSKNodeComponent
  , IsGKSKNodeComponent(..)
  , componentWithNode
  , initWithNode
  , node
  , setNode
  , componentWithNodeSelector
  , initWithNodeSelector
  , nodeSelector
  , setNodeSelector


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

-- | Creates a component that encapsulate the given SpriteKit node. When the component is  added to an entity, the SKNode's entity property will be set.
--
-- @node@ — Node to associate with the component.
--
-- See: SKNode.entity
--
-- ObjC selector: @+ componentWithNode:@
componentWithNode :: IsSKNode node => node -> IO (Id GKSKNodeComponent)
componentWithNode node =
  do
    cls' <- getRequiredClass "GKSKNodeComponent"
    sendClassMessage cls' componentWithNodeSelector (toSKNode node)

-- | Initializes component to encapsulate the given SpriteKit node. When the component is added to an entity, the SKNode's entity property will be set.
--
-- @node@ — Node to associate with the component.
--
-- See: SKNode.entity
--
-- ObjC selector: @- initWithNode:@
initWithNode :: (IsGKSKNodeComponent gkskNodeComponent, IsSKNode node) => gkskNodeComponent -> node -> IO (Id GKSKNodeComponent)
initWithNode gkskNodeComponent node =
  sendOwnedMessage gkskNodeComponent initWithNodeSelector (toSKNode node)

-- | The SpriteKit node this component encapsulates.
--
-- ObjC selector: @- node@
node :: IsGKSKNodeComponent gkskNodeComponent => gkskNodeComponent -> IO (Id SKNode)
node gkskNodeComponent =
  sendMessage gkskNodeComponent nodeSelector

-- | The SpriteKit node this component encapsulates.
--
-- ObjC selector: @- setNode:@
setNode :: (IsGKSKNodeComponent gkskNodeComponent, IsSKNode value) => gkskNodeComponent -> value -> IO ()
setNode gkskNodeComponent value =
  sendMessage gkskNodeComponent setNodeSelector (toSKNode value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @componentWithNode:@
componentWithNodeSelector :: Selector '[Id SKNode] (Id GKSKNodeComponent)
componentWithNodeSelector = mkSelector "componentWithNode:"

-- | @Selector@ for @initWithNode:@
initWithNodeSelector :: Selector '[Id SKNode] (Id GKSKNodeComponent)
initWithNodeSelector = mkSelector "initWithNode:"

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id SKNode)
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector '[Id SKNode] ()
setNodeSelector = mkSelector "setNode:"

