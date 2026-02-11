{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSShadow@.
module ObjC.AppKit.NSShadow
  ( NSShadow
  , IsNSShadow(..)
  , init_
  , set
  , shadowOffset
  , setShadowOffset
  , shadowBlurRadius
  , setShadowBlurRadius
  , shadowColor
  , setShadowColor
  , initSelector
  , setSelector
  , shadowOffsetSelector
  , setShadowOffsetSelector
  , shadowBlurRadiusSelector
  , setShadowBlurRadiusSelector
  , shadowColorSelector
  , setShadowColorSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSShadow nsShadow => nsShadow -> IO (Id NSShadow)
init_ nsShadow  =
  sendMsg nsShadow (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- set@
set :: IsNSShadow nsShadow => nsShadow -> IO ()
set nsShadow  =
  sendMsg nsShadow (mkSelector "set") retVoid []

-- | @- shadowOffset@
shadowOffset :: IsNSShadow nsShadow => nsShadow -> IO NSSize
shadowOffset nsShadow  =
  sendMsgStret nsShadow (mkSelector "shadowOffset") retNSSize []

-- | @- setShadowOffset:@
setShadowOffset :: IsNSShadow nsShadow => nsShadow -> NSSize -> IO ()
setShadowOffset nsShadow  value =
  sendMsg nsShadow (mkSelector "setShadowOffset:") retVoid [argNSSize value]

-- | @- shadowBlurRadius@
shadowBlurRadius :: IsNSShadow nsShadow => nsShadow -> IO CDouble
shadowBlurRadius nsShadow  =
  sendMsg nsShadow (mkSelector "shadowBlurRadius") retCDouble []

-- | @- setShadowBlurRadius:@
setShadowBlurRadius :: IsNSShadow nsShadow => nsShadow -> CDouble -> IO ()
setShadowBlurRadius nsShadow  value =
  sendMsg nsShadow (mkSelector "setShadowBlurRadius:") retVoid [argCDouble (fromIntegral value)]

-- | @- shadowColor@
shadowColor :: IsNSShadow nsShadow => nsShadow -> IO (Id NSColor)
shadowColor nsShadow  =
  sendMsg nsShadow (mkSelector "shadowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShadowColor:@
setShadowColor :: (IsNSShadow nsShadow, IsNSColor value) => nsShadow -> value -> IO ()
setShadowColor nsShadow  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsShadow (mkSelector "setShadowColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @shadowOffset@
shadowOffsetSelector :: Selector
shadowOffsetSelector = mkSelector "shadowOffset"

-- | @Selector@ for @setShadowOffset:@
setShadowOffsetSelector :: Selector
setShadowOffsetSelector = mkSelector "setShadowOffset:"

-- | @Selector@ for @shadowBlurRadius@
shadowBlurRadiusSelector :: Selector
shadowBlurRadiusSelector = mkSelector "shadowBlurRadius"

-- | @Selector@ for @setShadowBlurRadius:@
setShadowBlurRadiusSelector :: Selector
setShadowBlurRadiusSelector = mkSelector "setShadowBlurRadius:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector
setShadowColorSelector = mkSelector "setShadowColor:"

