{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLLight@.
module ObjC.ModelIO.MDLLight
  ( MDLLight
  , IsMDLLight(..)
  , lightType
  , setLightType
  , colorSpace
  , setColorSpace
  , lightTypeSelector
  , setLightTypeSelector
  , colorSpaceSelector
  , setColorSpaceSelector

  -- * Enum types
  , MDLLightType(MDLLightType)
  , pattern MDLLightTypeUnknown
  , pattern MDLLightTypeAmbient
  , pattern MDLLightTypeDirectional
  , pattern MDLLightTypeSpot
  , pattern MDLLightTypePoint
  , pattern MDLLightTypeLinear
  , pattern MDLLightTypeDiscArea
  , pattern MDLLightTypeRectangularArea
  , pattern MDLLightTypeSuperElliptical
  , pattern MDLLightTypePhotometric
  , pattern MDLLightTypeProbe
  , pattern MDLLightTypeEnvironment

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- lightType@
lightType :: IsMDLLight mdlLight => mdlLight -> IO MDLLightType
lightType mdlLight  =
  fmap (coerce :: CULong -> MDLLightType) $ sendMsg mdlLight (mkSelector "lightType") retCULong []

-- | @- setLightType:@
setLightType :: IsMDLLight mdlLight => mdlLight -> MDLLightType -> IO ()
setLightType mdlLight  value =
  sendMsg mdlLight (mkSelector "setLightType:") retVoid [argCULong (coerce value)]

-- | @- colorSpace@
colorSpace :: IsMDLLight mdlLight => mdlLight -> IO (Id NSString)
colorSpace mdlLight  =
  sendMsg mdlLight (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorSpace:@
setColorSpace :: (IsMDLLight mdlLight, IsNSString value) => mdlLight -> value -> IO ()
setColorSpace mdlLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlLight (mkSelector "setColorSpace:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lightType@
lightTypeSelector :: Selector
lightTypeSelector = mkSelector "lightType"

-- | @Selector@ for @setLightType:@
setLightTypeSelector :: Selector
setLightTypeSelector = mkSelector "setLightType:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector
setColorSpaceSelector = mkSelector "setColorSpace:"

