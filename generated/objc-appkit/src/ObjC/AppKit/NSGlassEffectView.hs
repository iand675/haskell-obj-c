{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view that embeds its content view in a dynamic glass effect.
--
-- Generated bindings for @NSGlassEffectView@.
module ObjC.AppKit.NSGlassEffectView
  ( NSGlassEffectView
  , IsNSGlassEffectView(..)
  , contentView
  , setContentView
  , cornerRadius
  , setCornerRadius
  , tintColor
  , setTintColor
  , style
  , setStyle
  , contentViewSelector
  , setContentViewSelector
  , cornerRadiusSelector
  , setCornerRadiusSelector
  , tintColorSelector
  , setTintColorSelector
  , styleSelector
  , setStyleSelector

  -- * Enum types
  , NSGlassEffectViewStyle(NSGlassEffectViewStyle)
  , pattern NSGlassEffectViewStyleRegular
  , pattern NSGlassEffectViewStyleClear

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The view to embed in glass.
--
-- - Important: @NSGlassEffectView@ only guarantees the @contentView@ will be placed inside the glass effect; arbitrary subviews aren't guaranteed specific behavior with regard to z-order in relation to the content view or glass effect.
--
-- ObjC selector: @- contentView@
contentView :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO (Id NSView)
contentView nsGlassEffectView  =
  sendMsg nsGlassEffectView (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view to embed in glass.
--
-- - Important: @NSGlassEffectView@ only guarantees the @contentView@ will be placed inside the glass effect; arbitrary subviews aren't guaranteed specific behavior with regard to z-order in relation to the content view or glass effect.
--
-- ObjC selector: @- setContentView:@
setContentView :: (IsNSGlassEffectView nsGlassEffectView, IsNSView value) => nsGlassEffectView -> value -> IO ()
setContentView nsGlassEffectView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGlassEffectView (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The amount of curvature for all corners of the glass.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO CDouble
cornerRadius nsGlassEffectView  =
  sendMsg nsGlassEffectView (mkSelector "cornerRadius") retCDouble []

-- | The amount of curvature for all corners of the glass.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> CDouble -> IO ()
setCornerRadius nsGlassEffectView  value =
  sendMsg nsGlassEffectView (mkSelector "setCornerRadius:") retVoid [argCDouble (fromIntegral value)]

-- | The color the glass effect view uses to tint the background and glass effect toward.
--
-- ObjC selector: @- tintColor@
tintColor :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO (Id NSColor)
tintColor nsGlassEffectView  =
  sendMsg nsGlassEffectView (mkSelector "tintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The color the glass effect view uses to tint the background and glass effect toward.
--
-- ObjC selector: @- setTintColor:@
setTintColor :: (IsNSGlassEffectView nsGlassEffectView, IsNSColor value) => nsGlassEffectView -> value -> IO ()
setTintColor nsGlassEffectView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGlassEffectView (mkSelector "setTintColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The style of glass this view uses.
--
-- ObjC selector: @- style@
style :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO NSGlassEffectViewStyle
style nsGlassEffectView  =
  fmap (coerce :: CLong -> NSGlassEffectViewStyle) $ sendMsg nsGlassEffectView (mkSelector "style") retCLong []

-- | The style of glass this view uses.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> NSGlassEffectViewStyle -> IO ()
setStyle nsGlassEffectView  value =
  sendMsg nsGlassEffectView (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @tintColor@
tintColorSelector :: Selector
tintColorSelector = mkSelector "tintColor"

-- | @Selector@ for @setTintColor:@
setTintColorSelector :: Selector
setTintColorSelector = mkSelector "setTintColor:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

