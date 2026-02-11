{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view that efficiently merges descendant glass effect views together when they are within a specified proximity to each other.
--
-- - Tip: Using a glass effect container view can improve performance by reducing the number of passes required to render similar glass effect views.
--
-- Generated bindings for @NSGlassEffectContainerView@.
module ObjC.AppKit.NSGlassEffectContainerView
  ( NSGlassEffectContainerView
  , IsNSGlassEffectContainerView(..)
  , contentView
  , setContentView
  , spacing
  , setSpacing
  , contentViewSelector
  , setContentViewSelector
  , spacingSelector
  , setSpacingSelector


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
import ObjC.Foundation.Internal.Classes

-- | The view that contains descendant views to merge together when in proximity to each other.
--
-- The glass effect container view does the following: 1. Elevates the z-order of descendants of @contentView@ to position them above the @contentView@. 2. Merges descendants together if the views are sufficiently similar and within the proximity specified in ``spacing``. 3. Processes similar glass effect views as a batch to improve performance.
--
-- ObjC selector: @- contentView@
contentView :: IsNSGlassEffectContainerView nsGlassEffectContainerView => nsGlassEffectContainerView -> IO (Id NSView)
contentView nsGlassEffectContainerView  =
  sendMsg nsGlassEffectContainerView (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view that contains descendant views to merge together when in proximity to each other.
--
-- The glass effect container view does the following: 1. Elevates the z-order of descendants of @contentView@ to position them above the @contentView@. 2. Merges descendants together if the views are sufficiently similar and within the proximity specified in ``spacing``. 3. Processes similar glass effect views as a batch to improve performance.
--
-- ObjC selector: @- setContentView:@
setContentView :: (IsNSGlassEffectContainerView nsGlassEffectContainerView, IsNSView value) => nsGlassEffectContainerView -> value -> IO ()
setContentView nsGlassEffectContainerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGlassEffectContainerView (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The proximity at which the glass effect container view begins merging eligible descendent glass effect views.
--
-- The default value, zero, is sufficient for batch processing eligible glass effect views, while avoiding distortion and merging effects for other views in close proximity.
--
-- ObjC selector: @- spacing@
spacing :: IsNSGlassEffectContainerView nsGlassEffectContainerView => nsGlassEffectContainerView -> IO CDouble
spacing nsGlassEffectContainerView  =
  sendMsg nsGlassEffectContainerView (mkSelector "spacing") retCDouble []

-- | The proximity at which the glass effect container view begins merging eligible descendent glass effect views.
--
-- The default value, zero, is sufficient for batch processing eligible glass effect views, while avoiding distortion and merging effects for other views in close proximity.
--
-- ObjC selector: @- setSpacing:@
setSpacing :: IsNSGlassEffectContainerView nsGlassEffectContainerView => nsGlassEffectContainerView -> CDouble -> IO ()
setSpacing nsGlassEffectContainerView  value =
  sendMsg nsGlassEffectContainerView (mkSelector "setSpacing:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @spacing@
spacingSelector :: Selector
spacingSelector = mkSelector "spacing"

-- | @Selector@ for @setSpacing:@
setSpacingSelector :: Selector
setSpacingSelector = mkSelector "setSpacing:"

