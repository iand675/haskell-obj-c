{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRotationGestureRecognizer@.
module ObjC.AppKit.NSRotationGestureRecognizer
  ( NSRotationGestureRecognizer
  , IsNSRotationGestureRecognizer(..)
  , rotation
  , setRotation
  , rotationInDegrees
  , setRotationInDegrees
  , rotationInDegreesSelector
  , rotationSelector
  , setRotationInDegreesSelector
  , setRotationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rotation@
rotation :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> IO CDouble
rotation nsRotationGestureRecognizer =
  sendMessage nsRotationGestureRecognizer rotationSelector

-- | @- setRotation:@
setRotation :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> CDouble -> IO ()
setRotation nsRotationGestureRecognizer value =
  sendMessage nsRotationGestureRecognizer setRotationSelector value

-- | @- rotationInDegrees@
rotationInDegrees :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> IO CDouble
rotationInDegrees nsRotationGestureRecognizer =
  sendMessage nsRotationGestureRecognizer rotationInDegreesSelector

-- | @- setRotationInDegrees:@
setRotationInDegrees :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> CDouble -> IO ()
setRotationInDegrees nsRotationGestureRecognizer value =
  sendMessage nsRotationGestureRecognizer setRotationInDegreesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rotation@
rotationSelector :: Selector '[] CDouble
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector '[CDouble] ()
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @rotationInDegrees@
rotationInDegreesSelector :: Selector '[] CDouble
rotationInDegreesSelector = mkSelector "rotationInDegrees"

-- | @Selector@ for @setRotationInDegrees:@
setRotationInDegreesSelector :: Selector '[CDouble] ()
setRotationInDegreesSelector = mkSelector "setRotationInDegrees:"

