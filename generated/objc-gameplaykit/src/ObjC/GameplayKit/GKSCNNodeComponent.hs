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
    withObjCPtr node $ \raw_node ->
      sendClassMsg cls' (mkSelector "componentWithNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes component to encapsulate the given SceneKit node. When the component is added to an entity, the SCNNode's entity property will be set.
--
-- @node@ — Node to associate with the component.
--
-- See: SCNNode.entity
--
-- ObjC selector: @- initWithNode:@
initWithNode :: (IsGKSCNNodeComponent gkscnNodeComponent, IsSCNNode node) => gkscnNodeComponent -> node -> IO (Id GKSCNNodeComponent)
initWithNode gkscnNodeComponent  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkscnNodeComponent (mkSelector "initWithNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= ownedObject . castPtr

-- | The SceneKit node this component encapsulates.
--
-- ObjC selector: @- node@
node :: IsGKSCNNodeComponent gkscnNodeComponent => gkscnNodeComponent -> IO (Id SCNNode)
node gkscnNodeComponent  =
  sendMsg gkscnNodeComponent (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @componentWithNode:@
componentWithNodeSelector :: Selector
componentWithNodeSelector = mkSelector "componentWithNode:"

-- | @Selector@ for @initWithNode:@
initWithNodeSelector :: Selector
initWithNodeSelector = mkSelector "initWithNode:"

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

