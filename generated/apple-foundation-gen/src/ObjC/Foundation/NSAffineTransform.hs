{-# LANGUAGE DataKinds #-}
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
  , appendTransformSelector
  , initSelector
  , initWithTransformSelector
  , invertSelector
  , prependTransformSelector
  , rotateByDegreesSelector
  , rotateByRadiansSelector
  , scaleBySelector
  , scaleXBy_yBySelector
  , transformPointSelector
  , transformSelector
  , transformSizeSelector
  , translateXBy_yBySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @+ transform@
transform :: IO (Id NSAffineTransform)
transform  =
  do
    cls' <- getRequiredClass "NSAffineTransform"
    sendClassMessage cls' transformSelector

-- | @- initWithTransform:@
initWithTransform :: (IsNSAffineTransform nsAffineTransform, IsNSAffineTransform transform) => nsAffineTransform -> transform -> IO (Id NSAffineTransform)
initWithTransform nsAffineTransform transform =
  sendOwnedMessage nsAffineTransform initWithTransformSelector (toNSAffineTransform transform)

-- | @- init@
init_ :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO (Id NSAffineTransform)
init_ nsAffineTransform =
  sendOwnedMessage nsAffineTransform initSelector

-- | @- translateXBy:yBy:@
translateXBy_yBy :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> CDouble -> IO ()
translateXBy_yBy nsAffineTransform deltaX deltaY =
  sendMessage nsAffineTransform translateXBy_yBySelector deltaX deltaY

-- | @- rotateByDegrees:@
rotateByDegrees :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> IO ()
rotateByDegrees nsAffineTransform angle =
  sendMessage nsAffineTransform rotateByDegreesSelector angle

-- | @- rotateByRadians:@
rotateByRadians :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> IO ()
rotateByRadians nsAffineTransform angle =
  sendMessage nsAffineTransform rotateByRadiansSelector angle

-- | @- scaleBy:@
scaleBy :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> IO ()
scaleBy nsAffineTransform scale =
  sendMessage nsAffineTransform scaleBySelector scale

-- | @- scaleXBy:yBy:@
scaleXBy_yBy :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> CDouble -> CDouble -> IO ()
scaleXBy_yBy nsAffineTransform scaleX scaleY =
  sendMessage nsAffineTransform scaleXBy_yBySelector scaleX scaleY

-- | @- invert@
invert :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO ()
invert nsAffineTransform =
  sendMessage nsAffineTransform invertSelector

-- | @- appendTransform:@
appendTransform :: (IsNSAffineTransform nsAffineTransform, IsNSAffineTransform transform) => nsAffineTransform -> transform -> IO ()
appendTransform nsAffineTransform transform =
  sendMessage nsAffineTransform appendTransformSelector (toNSAffineTransform transform)

-- | @- prependTransform:@
prependTransform :: (IsNSAffineTransform nsAffineTransform, IsNSAffineTransform transform) => nsAffineTransform -> transform -> IO ()
prependTransform nsAffineTransform transform =
  sendMessage nsAffineTransform prependTransformSelector (toNSAffineTransform transform)

-- | @- transformPoint:@
transformPoint :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> NSPoint -> IO NSPoint
transformPoint nsAffineTransform aPoint =
  sendMessage nsAffineTransform transformPointSelector aPoint

-- | @- transformSize:@
transformSize :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> NSSize -> IO NSSize
transformSize nsAffineTransform aSize =
  sendMessage nsAffineTransform transformSizeSelector aSize

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Id NSAffineTransform)
transformSelector = mkSelector "transform"

-- | @Selector@ for @initWithTransform:@
initWithTransformSelector :: Selector '[Id NSAffineTransform] (Id NSAffineTransform)
initWithTransformSelector = mkSelector "initWithTransform:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSAffineTransform)
initSelector = mkSelector "init"

-- | @Selector@ for @translateXBy:yBy:@
translateXBy_yBySelector :: Selector '[CDouble, CDouble] ()
translateXBy_yBySelector = mkSelector "translateXBy:yBy:"

-- | @Selector@ for @rotateByDegrees:@
rotateByDegreesSelector :: Selector '[CDouble] ()
rotateByDegreesSelector = mkSelector "rotateByDegrees:"

-- | @Selector@ for @rotateByRadians:@
rotateByRadiansSelector :: Selector '[CDouble] ()
rotateByRadiansSelector = mkSelector "rotateByRadians:"

-- | @Selector@ for @scaleBy:@
scaleBySelector :: Selector '[CDouble] ()
scaleBySelector = mkSelector "scaleBy:"

-- | @Selector@ for @scaleXBy:yBy:@
scaleXBy_yBySelector :: Selector '[CDouble, CDouble] ()
scaleXBy_yBySelector = mkSelector "scaleXBy:yBy:"

-- | @Selector@ for @invert@
invertSelector :: Selector '[] ()
invertSelector = mkSelector "invert"

-- | @Selector@ for @appendTransform:@
appendTransformSelector :: Selector '[Id NSAffineTransform] ()
appendTransformSelector = mkSelector "appendTransform:"

-- | @Selector@ for @prependTransform:@
prependTransformSelector :: Selector '[Id NSAffineTransform] ()
prependTransformSelector = mkSelector "prependTransform:"

-- | @Selector@ for @transformPoint:@
transformPointSelector :: Selector '[NSPoint] NSPoint
transformPointSelector = mkSelector "transformPoint:"

-- | @Selector@ for @transformSize:@
transformSizeSelector :: Selector '[NSSize] NSSize
transformSizeSelector = mkSelector "transformSize:"

