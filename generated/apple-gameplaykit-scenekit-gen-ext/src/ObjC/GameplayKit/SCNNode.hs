{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNNode
--
-- SCNNode is the model class for node-tree objects.
--
-- It encapsulates the position, rotations, and other transforms of a node, which define a coordinate system.		     The coordinate systems of all the sub-nodes are relative to the one of their parent node.
--
-- Generated bindings for @SCNNode@.
module ObjC.GameplayKit.SCNNode
  ( SCNNode
  , IsSCNNode(..)
  , entity
  , setEntity
  , entitySelector
  , setEntitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.SceneKit.Internal.Classes

-- | The GKEntity associated with the node via a GKSCNNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- entity@
entity :: IsSCNNode scnNode => scnNode -> IO RawId
entity scnNode =
  sendMessage scnNode entitySelector

-- | The GKEntity associated with the node via a GKSCNNodeComponent.
--
-- See: GKEntity
--
-- ObjC selector: @- setEntity:@
setEntity :: IsSCNNode scnNode => scnNode -> RawId -> IO ()
setEntity scnNode value =
  sendMessage scnNode setEntitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entity@
entitySelector :: Selector '[] RawId
entitySelector = mkSelector "entity"

-- | @Selector@ for @setEntity:@
setEntitySelector :: Selector '[RawId] ()
setEntitySelector = mkSelector "setEntity:"

