{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAreaLight@.
module ObjC.ModelIO.MDLAreaLight
  ( MDLAreaLight
  , IsMDLAreaLight(..)
  , areaRadius
  , setAreaRadius
  , aspect
  , setAspect
  , areaRadiusSelector
  , aspectSelector
  , setAreaRadiusSelector
  , setAspectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- areaRadius@
areaRadius :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> IO CFloat
areaRadius mdlAreaLight =
  sendMessage mdlAreaLight areaRadiusSelector

-- | @- setAreaRadius:@
setAreaRadius :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> CFloat -> IO ()
setAreaRadius mdlAreaLight value =
  sendMessage mdlAreaLight setAreaRadiusSelector value

-- | @- aspect@
aspect :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> IO CFloat
aspect mdlAreaLight =
  sendMessage mdlAreaLight aspectSelector

-- | @- setAspect:@
setAspect :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> CFloat -> IO ()
setAspect mdlAreaLight value =
  sendMessage mdlAreaLight setAspectSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areaRadius@
areaRadiusSelector :: Selector '[] CFloat
areaRadiusSelector = mkSelector "areaRadius"

-- | @Selector@ for @setAreaRadius:@
setAreaRadiusSelector :: Selector '[CFloat] ()
setAreaRadiusSelector = mkSelector "setAreaRadius:"

-- | @Selector@ for @aspect@
aspectSelector :: Selector '[] CFloat
aspectSelector = mkSelector "aspect"

-- | @Selector@ for @setAspect:@
setAspectSelector :: Selector '[CFloat] ()
setAspectSelector = mkSelector "setAspect:"

