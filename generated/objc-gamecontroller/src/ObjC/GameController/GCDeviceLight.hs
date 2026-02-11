{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A controller light is an abstract representation of the light-emitting capabilities of a GCController instance.
--
-- Generated bindings for @GCDeviceLight@.
module ObjC.GameController.GCDeviceLight
  ( GCDeviceLight
  , IsGCDeviceLight(..)
  , init_
  , color
  , setColor
  , initSelector
  , colorSelector
  , setColorSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCDeviceLight gcDeviceLight => gcDeviceLight -> IO (Id GCDeviceLight)
init_ gcDeviceLight  =
  sendMsg gcDeviceLight (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- color@
color :: IsGCDeviceLight gcDeviceLight => gcDeviceLight -> IO (Id GCColor)
color gcDeviceLight  =
  sendMsg gcDeviceLight (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsGCDeviceLight gcDeviceLight, IsGCColor value) => gcDeviceLight -> value -> IO ()
setColor gcDeviceLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcDeviceLight (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

