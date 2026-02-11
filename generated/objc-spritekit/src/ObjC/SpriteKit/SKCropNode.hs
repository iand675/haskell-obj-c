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

-- | SKNode to be used as the mask.
--
-- The SKNode supplied as the mask must not be a child of another node, but it may have children. Anywhere the mask's output alpha component is less than 0.05 masks out that area for the SKCropNode's children. If the mask is nil, nothing is masked out.
--
-- ObjC selector: @- maskNode@
maskNode :: IsSKCropNode skCropNode => skCropNode -> IO (Id SKNode)
maskNode skCropNode  =
  sendMsg skCropNode (mkSelector "maskNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SKNode to be used as the mask.
--
-- The SKNode supplied as the mask must not be a child of another node, but it may have children. Anywhere the mask's output alpha component is less than 0.05 masks out that area for the SKCropNode's children. If the mask is nil, nothing is masked out.
--
-- ObjC selector: @- setMaskNode:@
setMaskNode :: (IsSKCropNode skCropNode, IsSKNode value) => skCropNode -> value -> IO ()
setMaskNode skCropNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skCropNode (mkSelector "setMaskNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maskNode@
maskNodeSelector :: Selector
maskNodeSelector = mkSelector "maskNode"

-- | @Selector@ for @setMaskNode:@
setMaskNodeSelector :: Selector
setMaskNodeSelector = mkSelector "setMaskNode:"

