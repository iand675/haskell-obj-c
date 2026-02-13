{-# LANGUAGE DataKinds #-}
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
  , setShadowBlurRadiusSelector
  , setShadowColorSelector
  , setShadowOffsetSelector
  , shadowBlurRadiusSelector
  , shadowColorSelector
  , shadowOffsetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSShadow nsShadow => nsShadow -> IO (Id NSShadow)
init_ nsShadow =
  sendOwnedMessage nsShadow initSelector

-- | @- set@
set :: IsNSShadow nsShadow => nsShadow -> IO ()
set nsShadow =
  sendMessage nsShadow setSelector

-- | @- shadowOffset@
shadowOffset :: IsNSShadow nsShadow => nsShadow -> IO NSSize
shadowOffset nsShadow =
  sendMessage nsShadow shadowOffsetSelector

-- | @- setShadowOffset:@
setShadowOffset :: IsNSShadow nsShadow => nsShadow -> NSSize -> IO ()
setShadowOffset nsShadow value =
  sendMessage nsShadow setShadowOffsetSelector value

-- | @- shadowBlurRadius@
shadowBlurRadius :: IsNSShadow nsShadow => nsShadow -> IO CDouble
shadowBlurRadius nsShadow =
  sendMessage nsShadow shadowBlurRadiusSelector

-- | @- setShadowBlurRadius:@
setShadowBlurRadius :: IsNSShadow nsShadow => nsShadow -> CDouble -> IO ()
setShadowBlurRadius nsShadow value =
  sendMessage nsShadow setShadowBlurRadiusSelector value

-- | @- shadowColor@
shadowColor :: IsNSShadow nsShadow => nsShadow -> IO (Id NSColor)
shadowColor nsShadow =
  sendMessage nsShadow shadowColorSelector

-- | @- setShadowColor:@
setShadowColor :: (IsNSShadow nsShadow, IsNSColor value) => nsShadow -> value -> IO ()
setShadowColor nsShadow value =
  sendMessage nsShadow setShadowColorSelector (toNSColor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSShadow)
initSelector = mkSelector "init"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @shadowOffset@
shadowOffsetSelector :: Selector '[] NSSize
shadowOffsetSelector = mkSelector "shadowOffset"

-- | @Selector@ for @setShadowOffset:@
setShadowOffsetSelector :: Selector '[NSSize] ()
setShadowOffsetSelector = mkSelector "setShadowOffset:"

-- | @Selector@ for @shadowBlurRadius@
shadowBlurRadiusSelector :: Selector '[] CDouble
shadowBlurRadiusSelector = mkSelector "shadowBlurRadius"

-- | @Selector@ for @setShadowBlurRadius:@
setShadowBlurRadiusSelector :: Selector '[CDouble] ()
setShadowBlurRadiusSelector = mkSelector "setShadowBlurRadius:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector '[] (Id NSColor)
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector '[Id NSColor] ()
setShadowColorSelector = mkSelector "setShadowColor:"

