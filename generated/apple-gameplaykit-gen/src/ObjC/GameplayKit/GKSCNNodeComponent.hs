{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A component that encapsulates a SceneKit node.
--
-- Generated bindings for @GKSCNNodeComponent@.
module ObjC.GameplayKit.GKSCNNodeComponent
  ( GKSCNNodeComponent
  , IsGKSCNNodeComponent(..)
  , componentWithNode
  , initWithNode
  , node
  , componentWithNodeSelector
  , initWithNodeSelector
  , nodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SceneKit.Internal.Classes

-- | Creates a component that encapsulate the given SceneKit node. When the component is added to an entity, the SCNNode's entity property will be set.
--
-- @node@ — Node to associate with the component.
--
-- See: SCNNode.entity
--
-- ObjC selector: @+ componentWithNode:@
componentWithNode :: IsSCNNode node => node -> IO (Id GKSCNNodeComponent)
componentWithNode node =
  do
    cls' <- getRequiredClass "GKSCNNodeComponent"
    sendClassMessage cls' componentWithNodeSelector (toSCNNode node)

-- | Initializes component to encapsulate the given SceneKit node. When the component is added to an entity, the SCNNode's entity property will be set.
--
-- @node@ — Node to associate with the component.
--
-- See: SCNNode.entity
--
-- ObjC selector: @- initWithNode:@
initWithNode :: (IsGKSCNNodeComponent gkscnNodeComponent, IsSCNNode node) => gkscnNodeComponent -> node -> IO (Id GKSCNNodeComponent)
initWithNode gkscnNodeComponent node =
  sendOwnedMessage gkscnNodeComponent initWithNodeSelector (toSCNNode node)

-- | The SceneKit node this component encapsulates.
--
-- ObjC selector: @- node@
node :: IsGKSCNNodeComponent gkscnNodeComponent => gkscnNodeComponent -> IO (Id SCNNode)
node gkscnNodeComponent =
  sendMessage gkscnNodeComponent nodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @componentWithNode:@
componentWithNodeSelector :: Selector '[Id SCNNode] (Id GKSCNNodeComponent)
componentWithNodeSelector = mkSelector "componentWithNode:"

-- | @Selector@ for @initWithNode:@
initWithNodeSelector :: Selector '[Id SCNNode] (Id GKSCNNodeComponent)
initWithNodeSelector = mkSelector "initWithNode:"

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id SCNNode)
nodeSelector = mkSelector "node"

