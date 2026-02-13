{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , backgroundStyleSelector
  , displayContextSelector
  , enablesMarqueeSelector
  , highlightMenuSelector
  , highlightSelector
  , horizontalAlignmentSelector
  , menuTitleForHideActionSelector
  , preferredMaxLayoutWidthSelector
  , setBackgroundStyleSelector
  , setDisplayContextSelector
  , setEnablesMarqueeSelector
  , setHighlightSelector
  , setHorizontalAlignmentSelector
  , setMenuTitleForHideActionSelector
  , setPreferredMaxLayoutWidthSelector
  , setSupplementalMenuSelector
  , supplementalMenuSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
highlight swAttributionView =
  sendMessage swAttributionView highlightSelector

-- | The SWHighlight to use for displaying this attribution. When this property is set to a new highlight, the contents of the view will be reloaded.
--
-- ObjC selector: @- setHighlight:@
setHighlight :: (IsSWAttributionView swAttributionView, IsSWHighlight value) => swAttributionView -> value -> IO ()
setHighlight swAttributionView value =
  sendMessage swAttributionView setHighlightSelector (toSWHighlight value)

-- | The context for the content being displayed with this view. Set this prior to adding this view to your view hierarchy.
--
-- ObjC selector: @- displayContext@
displayContext :: IsSWAttributionView swAttributionView => swAttributionView -> IO SWAttributionViewDisplayContext
displayContext swAttributionView =
  sendMessage swAttributionView displayContextSelector

-- | The context for the content being displayed with this view. Set this prior to adding this view to your view hierarchy.
--
-- ObjC selector: @- setDisplayContext:@
setDisplayContext :: IsSWAttributionView swAttributionView => swAttributionView -> SWAttributionViewDisplayContext -> IO ()
setDisplayContext swAttributionView value =
  sendMessage swAttributionView setDisplayContextSelector value

-- | The horizontal alignment of the view. You should specify a value, in case the internal default ever changes.
--
-- This value specifies the horizontal anchor for the view's contents. This only has an effect when the width of the contents are less than the available width.
--
-- ObjC selector: @- horizontalAlignment@
horizontalAlignment :: IsSWAttributionView swAttributionView => swAttributionView -> IO SWAttributionViewHorizontalAlignment
horizontalAlignment swAttributionView =
  sendMessage swAttributionView horizontalAlignmentSelector

-- | The horizontal alignment of the view. You should specify a value, in case the internal default ever changes.
--
-- This value specifies the horizontal anchor for the view's contents. This only has an effect when the width of the contents are less than the available width.
--
-- ObjC selector: @- setHorizontalAlignment:@
setHorizontalAlignment :: IsSWAttributionView swAttributionView => swAttributionView -> SWAttributionViewHorizontalAlignment -> IO ()
setHorizontalAlignment swAttributionView value =
  sendMessage swAttributionView setHorizontalAlignmentSelector value

-- | The background style of the inner view containing names and avatars.
--
-- If you do not specify a background style, one will be chosen automatically. In general, .color looks best on monochrome backgrounds, while .material looks better on colored backgrounds.
--
-- ObjC selector: @- backgroundStyle@
backgroundStyle :: IsSWAttributionView swAttributionView => swAttributionView -> IO SWAttributionViewBackgroundStyle
backgroundStyle swAttributionView =
  sendMessage swAttributionView backgroundStyleSelector

-- | The background style of the inner view containing names and avatars.
--
-- If you do not specify a background style, one will be chosen automatically. In general, .color looks best on monochrome backgrounds, while .material looks better on colored backgrounds.
--
-- ObjC selector: @- setBackgroundStyle:@
setBackgroundStyle :: IsSWAttributionView swAttributionView => swAttributionView -> SWAttributionViewBackgroundStyle -> IO ()
setBackgroundStyle swAttributionView value =
  sendMessage swAttributionView setBackgroundStyleSelector value

-- | For use when embedding this view in a SwiftUI view representable.
--
-- When using this view in SwiftUI, this view will constrain its contents to this width. If you are not using SwiftUI this property should not be necessary, as SWAttributionView otherwise derives the maximum width from the frame or constraints you set.
--
-- ObjC selector: @- preferredMaxLayoutWidth@
preferredMaxLayoutWidth :: IsSWAttributionView swAttributionView => swAttributionView -> IO CDouble
preferredMaxLayoutWidth swAttributionView =
  sendMessage swAttributionView preferredMaxLayoutWidthSelector

-- | For use when embedding this view in a SwiftUI view representable.
--
-- When using this view in SwiftUI, this view will constrain its contents to this width. If you are not using SwiftUI this property should not be necessary, as SWAttributionView otherwise derives the maximum width from the frame or constraints you set.
--
-- ObjC selector: @- setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidth :: IsSWAttributionView swAttributionView => swAttributionView -> CDouble -> IO ()
setPreferredMaxLayoutWidth swAttributionView value =
  sendMessage swAttributionView setPreferredMaxLayoutWidthSelector value

-- | @- highlightMenu@
highlightMenu :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id NSMenu)
highlightMenu swAttributionView =
  sendMessage swAttributionView highlightMenuSelector

-- | A custom localized string to be used as the title for the "Hide" menu item title. A nil value will result in the default title.
--
-- SWAttributionView manages a context menu which includes the option for the user to hide the content for the SWHighlight represented by this view. Set a title to be used as the title for that context menu. An app that displays articles, for example, might set "Hide Article", localized to the current language. The string should include the word "Hide", localized correctly with the custom content type.
--
-- ObjC selector: @- menuTitleForHideAction@
menuTitleForHideAction :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id NSString)
menuTitleForHideAction swAttributionView =
  sendMessage swAttributionView menuTitleForHideActionSelector

-- | A custom localized string to be used as the title for the "Hide" menu item title. A nil value will result in the default title.
--
-- SWAttributionView manages a context menu which includes the option for the user to hide the content for the SWHighlight represented by this view. Set a title to be used as the title for that context menu. An app that displays articles, for example, might set "Hide Article", localized to the current language. The string should include the word "Hide", localized correctly with the custom content type.
--
-- ObjC selector: @- setMenuTitleForHideAction:@
setMenuTitleForHideAction :: (IsSWAttributionView swAttributionView, IsNSString value) => swAttributionView -> value -> IO ()
setMenuTitleForHideAction swAttributionView value =
  sendMessage swAttributionView setMenuTitleForHideActionSelector (toNSString value)

-- | @- supplementalMenu@
supplementalMenu :: IsSWAttributionView swAttributionView => swAttributionView -> IO (Id NSMenuItem)
supplementalMenu swAttributionView =
  sendMessage swAttributionView supplementalMenuSelector

-- | @- setSupplementalMenu:@
setSupplementalMenu :: (IsSWAttributionView swAttributionView, IsNSMenuItem value) => swAttributionView -> value -> IO ()
setSupplementalMenu swAttributionView value =
  sendMessage swAttributionView setSupplementalMenuSelector (toNSMenuItem value)

-- | Automatically enables a marquee effect if the text contents extend past the bounds of the view (tvOS only)
--
-- ObjC selector: @- enablesMarquee@
enablesMarquee :: IsSWAttributionView swAttributionView => swAttributionView -> IO Bool
enablesMarquee swAttributionView =
  sendMessage swAttributionView enablesMarqueeSelector

-- | Automatically enables a marquee effect if the text contents extend past the bounds of the view (tvOS only)
--
-- ObjC selector: @- setEnablesMarquee:@
setEnablesMarquee :: IsSWAttributionView swAttributionView => swAttributionView -> Bool -> IO ()
setEnablesMarquee swAttributionView value =
  sendMessage swAttributionView setEnablesMarqueeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @highlight@
highlightSelector :: Selector '[] (Id SWHighlight)
highlightSelector = mkSelector "highlight"

-- | @Selector@ for @setHighlight:@
setHighlightSelector :: Selector '[Id SWHighlight] ()
setHighlightSelector = mkSelector "setHighlight:"

-- | @Selector@ for @displayContext@
displayContextSelector :: Selector '[] SWAttributionViewDisplayContext
displayContextSelector = mkSelector "displayContext"

-- | @Selector@ for @setDisplayContext:@
setDisplayContextSelector :: Selector '[SWAttributionViewDisplayContext] ()
setDisplayContextSelector = mkSelector "setDisplayContext:"

-- | @Selector@ for @horizontalAlignment@
horizontalAlignmentSelector :: Selector '[] SWAttributionViewHorizontalAlignment
horizontalAlignmentSelector = mkSelector "horizontalAlignment"

-- | @Selector@ for @setHorizontalAlignment:@
setHorizontalAlignmentSelector :: Selector '[SWAttributionViewHorizontalAlignment] ()
setHorizontalAlignmentSelector = mkSelector "setHorizontalAlignment:"

-- | @Selector@ for @backgroundStyle@
backgroundStyleSelector :: Selector '[] SWAttributionViewBackgroundStyle
backgroundStyleSelector = mkSelector "backgroundStyle"

-- | @Selector@ for @setBackgroundStyle:@
setBackgroundStyleSelector :: Selector '[SWAttributionViewBackgroundStyle] ()
setBackgroundStyleSelector = mkSelector "setBackgroundStyle:"

-- | @Selector@ for @preferredMaxLayoutWidth@
preferredMaxLayoutWidthSelector :: Selector '[] CDouble
preferredMaxLayoutWidthSelector = mkSelector "preferredMaxLayoutWidth"

-- | @Selector@ for @setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidthSelector :: Selector '[CDouble] ()
setPreferredMaxLayoutWidthSelector = mkSelector "setPreferredMaxLayoutWidth:"

-- | @Selector@ for @highlightMenu@
highlightMenuSelector :: Selector '[] (Id NSMenu)
highlightMenuSelector = mkSelector "highlightMenu"

-- | @Selector@ for @menuTitleForHideAction@
menuTitleForHideActionSelector :: Selector '[] (Id NSString)
menuTitleForHideActionSelector = mkSelector "menuTitleForHideAction"

-- | @Selector@ for @setMenuTitleForHideAction:@
setMenuTitleForHideActionSelector :: Selector '[Id NSString] ()
setMenuTitleForHideActionSelector = mkSelector "setMenuTitleForHideAction:"

-- | @Selector@ for @supplementalMenu@
supplementalMenuSelector :: Selector '[] (Id NSMenuItem)
supplementalMenuSelector = mkSelector "supplementalMenu"

-- | @Selector@ for @setSupplementalMenu:@
setSupplementalMenuSelector :: Selector '[Id NSMenuItem] ()
setSupplementalMenuSelector = mkSelector "setSupplementalMenu:"

-- | @Selector@ for @enablesMarquee@
enablesMarqueeSelector :: Selector '[] Bool
enablesMarqueeSelector = mkSelector "enablesMarquee"

-- | @Selector@ for @setEnablesMarquee:@
setEnablesMarqueeSelector :: Selector '[Bool] ()
setEnablesMarqueeSelector = mkSelector "setEnablesMarquee:"

