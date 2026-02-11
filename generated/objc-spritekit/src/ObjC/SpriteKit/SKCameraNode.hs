{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Camera node is a full fledged SKNode that can have actions and physics applied to it. It also uses the standard SKNode transform system so modifying the camera node's position is how you translate the camera's viewport. Applying a scale to the node would zoom the viewport in or out etc. As an added benefit you can now rotate the viewport by applying a zRotation to the camera node, just as you would with any other SKNode.
--
-- The camera viewport is centered on the camera's position. It uses the scene's frame and scale mode along with the node transforms to determine the size, origin and rotation of the viewport.
--
-- There are some convenience functions included for testing if nodes are contained within the camera viewport. It can be used to determine if objects are no longer visible on the display.
--
-- In order to use a camera; set it on the scene that contains the camera.
--
-- See: SKScene.camera
--
-- Generated bindings for @SKCameraNode@.
module ObjC.SpriteKit.SKCameraNode
  ( SKCameraNode
  , IsSKCameraNode(..)
  , containsNode
  , containedNodeSet
  , containsNodeSelector
  , containedNodeSetSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Checks if the node is contained inside the viewport of the camera. The camera and node must both be in the same scene and presented on a view in order to determine if the node is inside the camera viewport rectangle.
--
-- Returns: YES if the node is inside the viewport. NO if node is nil or the node is outside the viewport.
--
-- ObjC selector: @- containsNode:@
containsNode :: (IsSKCameraNode skCameraNode, IsSKNode node) => skCameraNode -> node -> IO Bool
containsNode skCameraNode  node =
withObjCPtr node $ \raw_node ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skCameraNode (mkSelector "containsNode:") retCULong [argPtr (castPtr raw_node :: Ptr ())]

-- | Returns the set of nodes in the same scene as the camera that are contained within its viewport.
--
-- Returns: the set of nodes contained
--
-- ObjC selector: @- containedNodeSet@
containedNodeSet :: IsSKCameraNode skCameraNode => skCameraNode -> IO (Id NSSet)
containedNodeSet skCameraNode  =
  sendMsg skCameraNode (mkSelector "containedNodeSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @containsNode:@
containsNodeSelector :: Selector
containsNodeSelector = mkSelector "containsNode:"

-- | @Selector@ for @containedNodeSet@
containedNodeSetSelector :: Selector
containedNodeSetSelector = mkSelector "containedNodeSet"

