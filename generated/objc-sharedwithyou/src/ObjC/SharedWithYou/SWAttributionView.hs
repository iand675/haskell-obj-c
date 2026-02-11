{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWAttributionView@.
module ObjC.SharedWithYou.SWAttributionView
  ( SWAttributionView
  , IsSWAttributionView(..)
  , highlight
  , setHighlight
  , displayContext
  , setDisplayContext
  , horizontalAlignment
  , setHorizontalAlignment
  , backgroundStyle
  , setBackgroundStyle
  , preferredMaxLayoutWidth
  , setPreferredMaxLayoutWidth
  , highlightMenu
  , menuTitleForHideAction
  , setMenuTitleForHideAction
  , supplementalMenu
  , setSupplementalMenu
  , enablesMarquee
  , setEnablesMarquee
  , highlightSelector
  , setHighlightSelector
  , displayContextSelector
  , setDisplayContextSelector
  , horizontalAlignmentSelector
  , setHorizontalAlignmentSelector
  , backgroundStyleSelector
  , setBackgroundStyleSelector
  , preferredMaxLayoutWidthSelector
  , setPreferredMaxLayoutWidthSelector
  , highlightMenuSelector
  , menuTitleForHideActionSelector
  , setMenuTitleForHideActionSelector
  , supplementalMenuSelector
  , setSupplementalMenuSelector
  , enablesMarqueeSelector
  , setEnablesMarqueeSelector

  -- * Enum types
  , SWAttributionViewBackgroundStyle(SWAttributionViewBackgroundStyle)
  , pattern SWAttributionViewBackgroundStyleDefault
  , pattern SWAttributionViewBackgroundStyleColor
  , pattern SWAttributionViewBackgroundStyleMaterial
  , SWAttributionViewDisplayContext(SWAttributionViewDisplayContext)
  , pattern SWAttributionViewDisplayContextSummary
  , pattern SWAttributionViewDisplayContextDetail
  , SWAttributionViewHorizontalAlignment(SWAttributionViewHorizontalAlignment)
  , pattern SWAttributionViewHorizontalAlignmentDefault
  , pattern SWAttributionViewHorizontalAlignmentLeading
  , pattern SWAttributionViewHorizontalAlignmentCenter
  , pattern SWAttributionViewHorizontalAlignmentTrailing

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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.SharedWithYou.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The SWHighlight to use for displaying this attribution. When this property is set to a new highlight, the contents of the view will be reloaded.
--
-- ObjC selector: @- highlight@
highlight :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id SWHighlight)
highlight swAttributionView  =
  sendMsg swAttributionView (mkSelector "highlight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The SWHighlight to use for displaying this attribution. When this property is set to a new highlight, the contents of the view will be reloaded.
--
-- ObjC selector: @- setHighlight:@
setHighlight :: (IsSWAttributionView swAttributionView, IsSWHighlight value) => swAttributionView -> value -> IO ()
setHighlight swAttributionView  value =
withObjCPtr value $ \raw_value ->
    sendMsg swAttributionView (mkSelector "setHighlight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The context for the content being displayed with this view. Set this prior to adding this view to your view hierarchy.
--
-- ObjC selector: @- displayContext@
displayContext :: IsSWAttributionView swAttributionView => swAttributionView -> IO SWAttributionViewDisplayContext
displayContext swAttributionView  =
  fmap (coerce :: CLong -> SWAttributionViewDisplayContext) $ sendMsg swAttributionView (mkSelector "displayContext") retCLong []

-- | The context for the content being displayed with this view. Set this prior to adding this view to your view hierarchy.
--
-- ObjC selector: @- setDisplayContext:@
setDisplayContext :: IsSWAttributionView swAttributionView => swAttributionView -> SWAttributionViewDisplayContext -> IO ()
setDisplayContext swAttributionView  value =
  sendMsg swAttributionView (mkSelector "setDisplayContext:") retVoid [argCLong (coerce value)]

-- | The horizontal alignment of the view. You should specify a value, in case the internal default ever changes.
--
-- This value specifies the horizontal anchor for the view's contents. This only has an effect when the width of the contents are less than the available width.
--
-- ObjC selector: @- horizontalAlignment@
horizontalAlignment :: IsSWAttributionView swAttributionView => swAttributionView -> IO SWAttributionViewHorizontalAlignment
horizontalAlignment swAttributionView  =
  fmap (coerce :: CLong -> SWAttributionViewHorizontalAlignment) $ sendMsg swAttributionView (mkSelector "horizontalAlignment") retCLong []

-- | The horizontal alignment of the view. You should specify a value, in case the internal default ever changes.
--
-- This value specifies the horizontal anchor for the view's contents. This only has an effect when the width of the contents are less than the available width.
--
-- ObjC selector: @- setHorizontalAlignment:@
setHorizontalAlignment :: IsSWAttributionView swAttributionView => swAttributionView -> SWAttributionViewHorizontalAlignment -> IO ()
setHorizontalAlignment swAttributionView  value =
  sendMsg swAttributionView (mkSelector "setHorizontalAlignment:") retVoid [argCLong (coerce value)]

-- | The background style of the inner view containing names and avatars.
--
-- If you do not specify a background style, one will be chosen automatically. In general, .color looks best on monochrome backgrounds, while .material looks better on colored backgrounds.
--
-- ObjC selector: @- backgroundStyle@
backgroundStyle :: IsSWAttributionView swAttributionView => swAttributionView -> IO SWAttributionViewBackgroundStyle
backgroundStyle swAttributionView  =
  fmap (coerce :: CLong -> SWAttributionViewBackgroundStyle) $ sendMsg swAttributionView (mkSelector "backgroundStyle") retCLong []

-- | The background style of the inner view containing names and avatars.
--
-- If you do not specify a background style, one will be chosen automatically. In general, .color looks best on monochrome backgrounds, while .material looks better on colored backgrounds.
--
-- ObjC selector: @- setBackgroundStyle:@
setBackgroundStyle :: IsSWAttributionView swAttributionView => swAttributionView -> SWAttributionViewBackgroundStyle -> IO ()
setBackgroundStyle swAttributionView  value =
  sendMsg swAttributionView (mkSelector "setBackgroundStyle:") retVoid [argCLong (coerce value)]

-- | For use when embedding this view in a SwiftUI view representable.
--
-- When using this view in SwiftUI, this view will constrain its contents to this width. If you are not using SwiftUI this property should not be necessary, as SWAttributionView otherwise derives the maximum width from the frame or constraints you set.
--
-- ObjC selector: @- preferredMaxLayoutWidth@
preferredMaxLayoutWidth :: IsSWAttributionView swAttributionView => swAttributionView -> IO CDouble
preferredMaxLayoutWidth swAttributionView  =
  sendMsg swAttributionView (mkSelector "preferredMaxLayoutWidth") retCDouble []

-- | For use when embedding this view in a SwiftUI view representable.
--
-- When using this view in SwiftUI, this view will constrain its contents to this width. If you are not using SwiftUI this property should not be necessary, as SWAttributionView otherwise derives the maximum width from the frame or constraints you set.
--
-- ObjC selector: @- setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidth :: IsSWAttributionView swAttributionView => swAttributionView -> CDouble -> IO ()
setPreferredMaxLayoutWidth swAttributionView  value =
  sendMsg swAttributionView (mkSelector "setPreferredMaxLayoutWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- highlightMenu@
highlightMenu :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id NSMenu)
highlightMenu swAttributionView  =
  sendMsg swAttributionView (mkSelector "highlightMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A custom localized string to be used as the title for the "Hide" menu item title. A nil value will result in the default title.
--
-- SWAttributionView manages a context menu which includes the option for the user to hide the content for the SWHighlight represented by this view. Set a title to be used as the title for that context menu. An app that displays articles, for example, might set "Hide Article", localized to the current language. The string should include the word "Hide", localized correctly with the custom content type.
--
-- ObjC selector: @- menuTitleForHideAction@
menuTitleForHideAction :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id NSString)
menuTitleForHideAction swAttributionView  =
  sendMsg swAttributionView (mkSelector "menuTitleForHideAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A custom localized string to be used as the title for the "Hide" menu item title. A nil value will result in the default title.
--
-- SWAttributionView manages a context menu which includes the option for the user to hide the content for the SWHighlight represented by this view. Set a title to be used as the title for that context menu. An app that displays articles, for example, might set "Hide Article", localized to the current language. The string should include the word "Hide", localized correctly with the custom content type.
--
-- ObjC selector: @- setMenuTitleForHideAction:@
setMenuTitleForHideAction :: (IsSWAttributionView swAttributionView, IsNSString value) => swAttributionView -> value -> IO ()
setMenuTitleForHideAction swAttributionView  value =
withObjCPtr value $ \raw_value ->
    sendMsg swAttributionView (mkSelector "setMenuTitleForHideAction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supplementalMenu@
supplementalMenu :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id NSMenuItem)
supplementalMenu swAttributionView  =
  sendMsg swAttributionView (mkSelector "supplementalMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupplementalMenu:@
setSupplementalMenu :: (IsSWAttributionView swAttributionView, IsNSMenuItem value) => swAttributionView -> value -> IO ()
setSupplementalMenu swAttributionView  value =
withObjCPtr value $ \raw_value ->
    sendMsg swAttributionView (mkSelector "setSupplementalMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Automatically enables a marquee effect if the text contents extend past the bounds of the view (tvOS only)
--
-- ObjC selector: @- enablesMarquee@
enablesMarquee :: IsSWAttributionView swAttributionView => swAttributionView -> IO Bool
enablesMarquee swAttributionView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg swAttributionView (mkSelector "enablesMarquee") retCULong []

-- | Automatically enables a marquee effect if the text contents extend past the bounds of the view (tvOS only)
--
-- ObjC selector: @- setEnablesMarquee:@
setEnablesMarquee :: IsSWAttributionView swAttributionView => swAttributionView -> Bool -> IO ()
setEnablesMarquee swAttributionView  value =
  sendMsg swAttributionView (mkSelector "setEnablesMarquee:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @highlight@
highlightSelector :: Selector
highlightSelector = mkSelector "highlight"

-- | @Selector@ for @setHighlight:@
setHighlightSelector :: Selector
setHighlightSelector = mkSelector "setHighlight:"

-- | @Selector@ for @displayContext@
displayContextSelector :: Selector
displayContextSelector = mkSelector "displayContext"

-- | @Selector@ for @setDisplayContext:@
setDisplayContextSelector :: Selector
setDisplayContextSelector = mkSelector "setDisplayContext:"

-- | @Selector@ for @horizontalAlignment@
horizontalAlignmentSelector :: Selector
horizontalAlignmentSelector = mkSelector "horizontalAlignment"

-- | @Selector@ for @setHorizontalAlignment:@
setHorizontalAlignmentSelector :: Selector
setHorizontalAlignmentSelector = mkSelector "setHorizontalAlignment:"

-- | @Selector@ for @backgroundStyle@
backgroundStyleSelector :: Selector
backgroundStyleSelector = mkSelector "backgroundStyle"

-- | @Selector@ for @setBackgroundStyle:@
setBackgroundStyleSelector :: Selector
setBackgroundStyleSelector = mkSelector "setBackgroundStyle:"

-- | @Selector@ for @preferredMaxLayoutWidth@
preferredMaxLayoutWidthSelector :: Selector
preferredMaxLayoutWidthSelector = mkSelector "preferredMaxLayoutWidth"

-- | @Selector@ for @setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidthSelector :: Selector
setPreferredMaxLayoutWidthSelector = mkSelector "setPreferredMaxLayoutWidth:"

-- | @Selector@ for @highlightMenu@
highlightMenuSelector :: Selector
highlightMenuSelector = mkSelector "highlightMenu"

-- | @Selector@ for @menuTitleForHideAction@
menuTitleForHideActionSelector :: Selector
menuTitleForHideActionSelector = mkSelector "menuTitleForHideAction"

-- | @Selector@ for @setMenuTitleForHideAction:@
setMenuTitleForHideActionSelector :: Selector
setMenuTitleForHideActionSelector = mkSelector "setMenuTitleForHideAction:"

-- | @Selector@ for @supplementalMenu@
supplementalMenuSelector :: Selector
supplementalMenuSelector = mkSelector "supplementalMenu"

-- | @Selector@ for @setSupplementalMenu:@
setSupplementalMenuSelector :: Selector
setSupplementalMenuSelector = mkSelector "setSupplementalMenu:"

-- | @Selector@ for @enablesMarquee@
enablesMarqueeSelector :: Selector
enablesMarqueeSelector = mkSelector "enablesMarquee"

-- | @Selector@ for @setEnablesMarquee:@
setEnablesMarqueeSelector :: Selector
setEnablesMarqueeSelector = mkSelector "setEnablesMarquee:"

