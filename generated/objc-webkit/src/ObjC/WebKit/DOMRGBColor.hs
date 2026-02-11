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
  , redSelector
  , greenSelector
  , blueSelector
  , alphaSelector
  , colorSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- red@
red :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
red domrgbColor  =
  sendMsg domrgbColor (mkSelector "red") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- green@
green :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
green domrgbColor  =
  sendMsg domrgbColor (mkSelector "green") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- blue@
blue :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
blue domrgbColor  =
  sendMsg domrgbColor (mkSelector "blue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alpha@
alpha :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id DOMCSSPrimitiveValue)
alpha domrgbColor  =
  sendMsg domrgbColor (mkSelector "alpha") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- color@
color :: IsDOMRGBColor domrgbColor => domrgbColor -> IO (Id NSColor)
color domrgbColor  =
  sendMsg domrgbColor (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @red@
redSelector :: Selector
redSelector = mkSelector "red"

-- | @Selector@ for @green@
greenSelector :: Selector
greenSelector = mkSelector "green"

-- | @Selector@ for @blue@
blueSelector :: Selector
blueSelector = mkSelector "blue"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

