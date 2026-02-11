{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAffineTransform@.
module ObjC.Foundation.NSAffineTransform
  ( NSAffineTransform
  , IsNSAffineTransform(..)
  , transform
  , initWithTransform
  , init_
  , translateXBy_yBy
  , rotateByDegrees
  , rotateByRadians
  , scaleBy
  , scaleXBy_yBy
  , invert
  , appendTransform
  , prependTransform
  , transformPoint
  , transformSize
  , transformSelector
  , initWithTransformSelector
  , initSelector
  , translateXBy_yBySelector
  , rotateByDegreesSelector
  , rotateByRadiansSelector
  , scaleBySelector
  , scaleXBy_yBySelector
  , invertSelector
  , appendTransformSelector
  , prependTransformSelector
  , transformPointSelector
  , transformSizeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @+ transform@
transform :: IO (Id NSAffineTransform)
transform  =
  do
    cls' <- getRequiredClass "NSAffineTransform"
    sendClassMsg cls' (mkSelector "transform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithTransform:@
initWithTransform :: (IsNSAffineTransform nsAffineTransform, IsNSAffineTransform transform) => nsAffineTransform -> transform -> IO (Id NSAffineTransform)
initWithTransform nsAffineTransform  transform =
withObjCPtr transform $ \raw_transform ->
    sendMsg nsAffineTransform (mkSelector "initWithTransform:") (retPtr retVoid) [argPtr (castPtr raw_transform :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO (Id NSAffineTransform)
init_ nsAffineTransform  =
  sendMsg nsAffineTransform (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- translateXBy:yBy:@
translateXBy_yBy :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> CDouble -> IO ()
translateXBy_yBy nsAffineTransform  deltaX deltaY =
  sendMsg nsAffineTransform (mkSelector "translateXBy:yBy:") retVoid [argCDouble (fromIntegral deltaX), argCDouble (fromIntegral deltaY)]

-- | @- rotateByDegrees:@
rotateByDegrees :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> IO ()
rotateByDegrees nsAffineTransform  angle =
  sendMsg nsAffineTransform (mkSelector "rotateByDegrees:") retVoid [argCDouble (fromIntegral angle)]

-- | @- rotateByRadians:@
rotateByRadians :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> IO ()
rotateByRadians nsAffineTransform  angle =
  sendMsg nsAffineTransform (mkSelector "rotateByRadians:") retVoid [argCDouble (fromIntegral angle)]

-- | @- scaleBy:@
scaleBy :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> IO ()
scaleBy nsAffineTransform  scale =
  sendMsg nsAffineTransform (mkSelector "scaleBy:") retVoid [argCDouble (fromIntegral scale)]

-- | @- scaleXBy:yBy:@
scaleXBy_yBy :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> CDouble -> IO ()
scaleXBy_yBy nsAffineTransform  scaleX scaleY =
  sendMsg nsAffineTransform (mkSelector "scaleXBy:yBy:") retVoid [argCDouble (fromIntegral scaleX), argCDouble (fromIntegral scaleY)]

-- | @- invert@
invert :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO ()
invert nsAffineTransform  =
  sendMsg nsAffineTransform (mkSelector "invert") retVoid []

-- | @- appendTransform:@
appendTransform :: (IsNSAffineTransform nsAffineTransform, IsNSAffineTransform transform) => nsAffineTransform -> transform -> IO ()
appendTransform nsAffineTransform  transform =
withObjCPtr transform $ \raw_transform ->
    sendMsg nsAffineTransform (mkSelector "appendTransform:") retVoid [argPtr (castPtr raw_transform :: Ptr ())]

-- | @- prependTransform:@
prependTransform :: (IsNSAffineTransform nsAffineTransform, IsNSAffineTransform transform) => nsAffineTransform -> transform -> IO ()
prependTransform nsAffineTransform  transform =
withObjCPtr transform $ \raw_transform ->
    sendMsg nsAffineTransform (mkSelector "prependTransform:") retVoid [argPtr (castPtr raw_transform :: Ptr ())]

-- | @- transformPoint:@
transformPoint :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> NSPoint -> IO NSPoint
transformPoint nsAffineTransform  aPoint =
  sendMsgStret nsAffineTransform (mkSelector "transformPoint:") retNSPoint [argNSPoint aPoint]

-- | @- transformSize:@
transformSize :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> NSSize -> IO NSSize
transformSize nsAffineTransform  aSize =
  sendMsgStret nsAffineTransform (mkSelector "transformSize:") retNSSize [argNSSize aSize]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @initWithTransform:@
initWithTransformSelector :: Selector
initWithTransformSelector = mkSelector "initWithTransform:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @translateXBy:yBy:@
translateXBy_yBySelector :: Selector
translateXBy_yBySelector = mkSelector "translateXBy:yBy:"

-- | @Selector@ for @rotateByDegrees:@
rotateByDegreesSelector :: Selector
rotateByDegreesSelector = mkSelector "rotateByDegrees:"

-- | @Selector@ for @rotateByRadians:@
rotateByRadiansSelector :: Selector
rotateByRadiansSelector = mkSelector "rotateByRadians:"

-- | @Selector@ for @scaleBy:@
scaleBySelector :: Selector
scaleBySelector = mkSelector "scaleBy:"

-- | @Selector@ for @scaleXBy:yBy:@
scaleXBy_yBySelector :: Selector
scaleXBy_yBySelector = mkSelector "scaleXBy:yBy:"

-- | @Selector@ for @invert@
invertSelector :: Selector
invertSelector = mkSelector "invert"

-- | @Selector@ for @appendTransform:@
appendTransformSelector :: Selector
appendTransformSelector = mkSelector "appendTransform:"

-- | @Selector@ for @prependTransform:@
prependTransformSelector :: Selector
prependTransformSelector = mkSelector "prependTransform:"

-- | @Selector@ for @transformPoint:@
transformPointSelector :: Selector
transformPointSelector = mkSelector "transformPoint:"

-- | @Selector@ for @transformSize:@
transformSizeSelector :: Selector
transformSizeSelector = mkSelector "transformSize:"

