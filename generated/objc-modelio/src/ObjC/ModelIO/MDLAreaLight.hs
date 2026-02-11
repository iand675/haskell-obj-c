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
  , setAreaRadiusSelector
  , aspectSelector
  , setAspectSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- areaRadius@
areaRadius :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> IO CFloat
areaRadius mdlAreaLight  =
  sendMsg mdlAreaLight (mkSelector "areaRadius") retCFloat []

-- | @- setAreaRadius:@
setAreaRadius :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> CFloat -> IO ()
setAreaRadius mdlAreaLight  value =
  sendMsg mdlAreaLight (mkSelector "setAreaRadius:") retVoid [argCFloat (fromIntegral value)]

-- | @- aspect@
aspect :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> IO CFloat
aspect mdlAreaLight  =
  sendMsg mdlAreaLight (mkSelector "aspect") retCFloat []

-- | @- setAspect:@
setAspect :: IsMDLAreaLight mdlAreaLight => mdlAreaLight -> CFloat -> IO ()
setAspect mdlAreaLight  value =
  sendMsg mdlAreaLight (mkSelector "setAspect:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areaRadius@
areaRadiusSelector :: Selector
areaRadiusSelector = mkSelector "areaRadius"

-- | @Selector@ for @setAreaRadius:@
setAreaRadiusSelector :: Selector
setAreaRadiusSelector = mkSelector "setAreaRadius:"

-- | @Selector@ for @aspect@
aspectSelector :: Selector
aspectSelector = mkSelector "aspect"

-- | @Selector@ for @setAspect:@
setAspectSelector :: Selector
setAspectSelector = mkSelector "setAspect:"

