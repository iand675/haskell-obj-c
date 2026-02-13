{-# LANGUAGE DataKinds #-}
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
  , setXRotationSelector
  , setYRotationSelector
  , xRotationSelector
  , yRotationSelector


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

-- | @- xRotation@
xRotation :: IsSKTransformNode skTransformNode => skTransformNode -> IO CDouble
xRotation skTransformNode =
  sendMessage skTransformNode xRotationSelector

-- | @- setXRotation:@
setXRotation :: IsSKTransformNode skTransformNode => skTransformNode -> CDouble -> IO ()
setXRotation skTransformNode value =
  sendMessage skTransformNode setXRotationSelector value

-- | @- yRotation@
yRotation :: IsSKTransformNode skTransformNode => skTransformNode -> IO CDouble
yRotation skTransformNode =
  sendMessage skTransformNode yRotationSelector

-- | @- setYRotation:@
setYRotation :: IsSKTransformNode skTransformNode => skTransformNode -> CDouble -> IO ()
setYRotation skTransformNode value =
  sendMessage skTransformNode setYRotationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @xRotation@
xRotationSelector :: Selector '[] CDouble
xRotationSelector = mkSelector "xRotation"

-- | @Selector@ for @setXRotation:@
setXRotationSelector :: Selector '[CDouble] ()
setXRotationSelector = mkSelector "setXRotation:"

-- | @Selector@ for @yRotation@
yRotationSelector :: Selector '[] CDouble
yRotationSelector = mkSelector "yRotation"

-- | @Selector@ for @setYRotation:@
setYRotationSelector :: Selector '[CDouble] ()
setYRotationSelector = mkSelector "setYRotation:"

