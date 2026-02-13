{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cornerRadiusSelector
  , setContentViewSelector
  , setCornerRadiusSelector
  , setStyleSelector
  , setTintColorSelector
  , styleSelector
  , tintColorSelector

  -- * Enum types
  , NSGlassEffectViewStyle(NSGlassEffectViewStyle)
  , pattern NSGlassEffectViewStyleRegular
  , pattern NSGlassEffectViewStyleClear

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
contentView nsGlassEffectView =
  sendMessage nsGlassEffectView contentViewSelector

-- | The view to embed in glass.
--
-- - Important: @NSGlassEffectView@ only guarantees the @contentView@ will be placed inside the glass effect; arbitrary subviews aren't guaranteed specific behavior with regard to z-order in relation to the content view or glass effect.
--
-- ObjC selector: @- setContentView:@
setContentView :: (IsNSGlassEffectView nsGlassEffectView, IsNSView value) => nsGlassEffectView -> value -> IO ()
setContentView nsGlassEffectView value =
  sendMessage nsGlassEffectView setContentViewSelector (toNSView value)

-- | The amount of curvature for all corners of the glass.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO CDouble
cornerRadius nsGlassEffectView =
  sendMessage nsGlassEffectView cornerRadiusSelector

-- | The amount of curvature for all corners of the glass.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> CDouble -> IO ()
setCornerRadius nsGlassEffectView value =
  sendMessage nsGlassEffectView setCornerRadiusSelector value

-- | The color the glass effect view uses to tint the background and glass effect toward.
--
-- ObjC selector: @- tintColor@
tintColor :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO (Id NSColor)
tintColor nsGlassEffectView =
  sendMessage nsGlassEffectView tintColorSelector

-- | The color the glass effect view uses to tint the background and glass effect toward.
--
-- ObjC selector: @- setTintColor:@
setTintColor :: (IsNSGlassEffectView nsGlassEffectView, IsNSColor value) => nsGlassEffectView -> value -> IO ()
setTintColor nsGlassEffectView value =
  sendMessage nsGlassEffectView setTintColorSelector (toNSColor value)

-- | The style of glass this view uses.
--
-- ObjC selector: @- style@
style :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> IO NSGlassEffectViewStyle
style nsGlassEffectView =
  sendMessage nsGlassEffectView styleSelector

-- | The style of glass this view uses.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsNSGlassEffectView nsGlassEffectView => nsGlassEffectView -> NSGlassEffectViewStyle -> IO ()
setStyle nsGlassEffectView value =
  sendMessage nsGlassEffectView setStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @tintColor@
tintColorSelector :: Selector '[] (Id NSColor)
tintColorSelector = mkSelector "tintColor"

-- | @Selector@ for @setTintColor:@
setTintColorSelector :: Selector '[Id NSColor] ()
setTintColorSelector = mkSelector "setTintColor:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSGlassEffectViewStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[NSGlassEffectViewStyle] ()
setStyleSelector = mkSelector "setStyle:"

