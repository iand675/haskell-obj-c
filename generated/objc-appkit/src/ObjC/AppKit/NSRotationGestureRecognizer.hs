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
  , rotationSelector
  , setRotationSelector
  , rotationInDegreesSelector
  , setRotationInDegreesSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rotation@
rotation :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> IO CDouble
rotation nsRotationGestureRecognizer  =
  sendMsg nsRotationGestureRecognizer (mkSelector "rotation") retCDouble []

-- | @- setRotation:@
setRotation :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> CDouble -> IO ()
setRotation nsRotationGestureRecognizer  value =
  sendMsg nsRotationGestureRecognizer (mkSelector "setRotation:") retVoid [argCDouble (fromIntegral value)]

-- | @- rotationInDegrees@
rotationInDegrees :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> IO CDouble
rotationInDegrees nsRotationGestureRecognizer  =
  sendMsg nsRotationGestureRecognizer (mkSelector "rotationInDegrees") retCDouble []

-- | @- setRotationInDegrees:@
setRotationInDegrees :: IsNSRotationGestureRecognizer nsRotationGestureRecognizer => nsRotationGestureRecognizer -> CDouble -> IO ()
setRotationInDegrees nsRotationGestureRecognizer  value =
  sendMsg nsRotationGestureRecognizer (mkSelector "setRotationInDegrees:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @rotationInDegrees@
rotationInDegreesSelector :: Selector
rotationInDegreesSelector = mkSelector "rotationInDegrees"

-- | @Selector@ for @setRotationInDegrees:@
setRotationInDegreesSelector :: Selector
setRotationInDegreesSelector = mkSelector "setRotationInDegrees:"

