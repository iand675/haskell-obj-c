{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An SKTransformNode can be applied a 3D rotation that will affect the visual aspect of its children. The physics and constraints of the children will behave as if none of them were transformed.
--
-- Generated bindings for @SKTransformNode@.
module ObjC.SpriteKit.SKTransformNode
  ( SKTransformNode
  , IsSKTransformNode(..)
  , xRotation
  , setXRotation
  , yRotation
  , setYRotation
  , xRotationSelector
  , setXRotationSelector
  , yRotationSelector
  , setYRotationSelector


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

-- | @- xRotation@
xRotation :: IsSKTransformNode skTransformNode => skTransformNode -> IO CDouble
xRotation skTransformNode  =
  sendMsg skTransformNode (mkSelector "xRotation") retCDouble []

-- | @- setXRotation:@
setXRotation :: IsSKTransformNode skTransformNode => skTransformNode -> CDouble -> IO ()
setXRotation skTransformNode  value =
  sendMsg skTransformNode (mkSelector "setXRotation:") retVoid [argCDouble (fromIntegral value)]

-- | @- yRotation@
yRotation :: IsSKTransformNode skTransformNode => skTransformNode -> IO CDouble
yRotation skTransformNode  =
  sendMsg skTransformNode (mkSelector "yRotation") retCDouble []

-- | @- setYRotation:@
setYRotation :: IsSKTransformNode skTransformNode => skTransformNode -> CDouble -> IO ()
setYRotation skTransformNode  value =
  sendMsg skTransformNode (mkSelector "setYRotation:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @xRotation@
xRotationSelector :: Selector
xRotationSelector = mkSelector "xRotation"

-- | @Selector@ for @setXRotation:@
setXRotationSelector :: Selector
setXRotationSelector = mkSelector "setXRotation:"

-- | @Selector@ for @yRotation@
yRotationSelector :: Selector
yRotationSelector = mkSelector "yRotation"

-- | @Selector@ for @setYRotation:@
setYRotationSelector :: Selector
setYRotationSelector = mkSelector "setYRotation:"

