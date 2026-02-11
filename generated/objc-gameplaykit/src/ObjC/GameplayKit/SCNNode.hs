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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

