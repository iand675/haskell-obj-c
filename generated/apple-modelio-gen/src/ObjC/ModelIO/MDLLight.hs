{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , colorSpaceSelector
  , lightTypeSelector
  , setColorSpaceSelector
  , setLightTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- lightType@
lightType :: IsMDLLight mdlLight => mdlLight -> IO MDLLightType
lightType mdlLight =
  sendMessage mdlLight lightTypeSelector

-- | @- setLightType:@
setLightType :: IsMDLLight mdlLight => mdlLight -> MDLLightType -> IO ()
setLightType mdlLight value =
  sendMessage mdlLight setLightTypeSelector value

-- | @- colorSpace@
colorSpace :: IsMDLLight mdlLight => mdlLight -> IO (Id NSString)
colorSpace mdlLight =
  sendMessage mdlLight colorSpaceSelector

-- | @- setColorSpace:@
setColorSpace :: (IsMDLLight mdlLight, IsNSString value) => mdlLight -> value -> IO ()
setColorSpace mdlLight value =
  sendMessage mdlLight setColorSpaceSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lightType@
lightTypeSelector :: Selector '[] MDLLightType
lightTypeSelector = mkSelector "lightType"

-- | @Selector@ for @setLightType:@
setLightTypeSelector :: Selector '[MDLLightType] ()
setLightTypeSelector = mkSelector "setLightType:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSString)
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector '[Id NSString] ()
setColorSpaceSelector = mkSelector "setColorSpace:"

