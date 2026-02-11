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
    withObjCPtr node $ \raw_node ->
      sendClassMsg cls' (mkSelector "componentWithNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes component to encapsulate the given SpriteKit node. When the component is added to an entity, the SKNode's entity property will be set.
--
-- @node@ — Node to associate with the component.
--
-- See: SKNode.entity
--
-- ObjC selector: @- initWithNode:@
initWithNode :: (IsGKSKNodeComponent gkskNodeComponent, IsSKNode node) => gkskNodeComponent -> node -> IO (Id GKSKNodeComponent)
initWithNode gkskNodeComponent  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkskNodeComponent (mkSelector "initWithNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= ownedObject . castPtr

-- | The SpriteKit node this component encapsulates.
--
-- ObjC selector: @- node@
node :: IsGKSKNodeComponent gkskNodeComponent => gkskNodeComponent -> IO (Id SKNode)
node gkskNodeComponent  =
  sendMsg gkskNodeComponent (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The SpriteKit node this component encapsulates.
--
-- ObjC selector: @- setNode:@
setNode :: (IsGKSKNodeComponent gkskNodeComponent, IsSKNode value) => gkskNodeComponent -> value -> IO ()
setNode gkskNodeComponent  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkskNodeComponent (mkSelector "setNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector
setNodeSelector = mkSelector "setNode:"

