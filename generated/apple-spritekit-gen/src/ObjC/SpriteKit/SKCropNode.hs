{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A SpriteKit node that masks child nodes using another node's alpha component
--
-- Generated bindings for @SKCropNode@.
module ObjC.SpriteKit.SKCropNode
  ( SKCropNode
  , IsSKCropNode(..)
  , maskNode
  , setMaskNode
  , maskNodeSelector
  , setMaskNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | SKNode to be used as the mask.
--
-- The SKNode supplied as the mask must not be a child of another node, but it may have children. Anywhere the mask's output alpha component is less than 0.05 masks out that area for the SKCropNode's children. If the mask is nil, nothing is masked out.
--
-- ObjC selector: @- maskNode@
maskNode :: IsSKCropNode skCropNode => skCropNode -> IO (Id SKNode)
maskNode skCropNode =
  sendMessage skCropNode maskNodeSelector

-- | SKNode to be used as the mask.
--
-- The SKNode supplied as the mask must not be a child of another node, but it may have children. Anywhere the mask's output alpha component is less than 0.05 masks out that area for the SKCropNode's children. If the mask is nil, nothing is masked out.
--
-- ObjC selector: @- setMaskNode:@
setMaskNode :: (IsSKCropNode skCropNode, IsSKNode value) => skCropNode -> value -> IO ()
setMaskNode skCropNode value =
  sendMessage skCropNode setMaskNodeSelector (toSKNode value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maskNode@
maskNodeSelector :: Selector '[] (Id SKNode)
maskNodeSelector = mkSelector "maskNode"

-- | @Selector@ for @setMaskNode:@
setMaskNodeSelector :: Selector '[Id SKNode] ()
setMaskNodeSelector = mkSelector "setMaskNode:"

