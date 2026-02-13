{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMRGBColor@.
module ObjC.WebKit.DOMRGBColor
  ( DOMRGBColor
  , IsDOMRGBColor(..)
  , red
  , green
  , blue
  , alpha
  , color
  , alphaSelector
  , blueSelector
  , colorSelector
  , greenSelector
  , redSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- red@
red :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
red domrgbColor =
  sendMessage domrgbColor redSelector

-- | @- green@
green :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
green domrgbColor =
  sendMessage domrgbColor greenSelector

-- | @- blue@
blue :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
blue domrgbColor =
  sendMessage domrgbColor blueSelector

-- | @- alpha@
alpha :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
alpha domrgbColor =
  sendMessage domrgbColor alphaSelector

-- | @- color@
color :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id NSColor)
color domrgbColor =
  sendMessage domrgbColor colorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @red@
redSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
redSelector = mkSelector "red"

-- | @Selector@ for @green@
greenSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
greenSelector = mkSelector "green"

-- | @Selector@ for @blue@
blueSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
blueSelector = mkSelector "blue"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

