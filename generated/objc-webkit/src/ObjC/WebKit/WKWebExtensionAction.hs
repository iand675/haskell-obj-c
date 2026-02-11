{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionAction`` object encapsulates the properties for an individual web extension action.
--
-- Provides access to action properties such as popup, icon, and title, with tab-specific values.
--
-- Generated bindings for @WKWebExtensionAction@.
module ObjC.WebKit.WKWebExtensionAction
  ( WKWebExtensionAction
  , IsWKWebExtensionAction(..)
  , init_
  , new
  , closePopup
  , webExtensionContext
  , label
  , badgeText
  , hasUnreadBadgeText
  , setHasUnreadBadgeText
  , inspectionName
  , setInspectionName
  , enabled
  , menuItems
  , presentsPopup
  , popupPopover
  , popupWebView
  , initSelector
  , newSelector
  , closePopupSelector
  , webExtensionContextSelector
  , labelSelector
  , badgeTextSelector
  , hasUnreadBadgeTextSelector
  , setHasUnreadBadgeTextSelector
  , inspectionNameSelector
  , setInspectionNameSelector
  , enabledSelector
  , menuItemsSelector
  , presentsPopupSelector
  , popupPopoverSelector
  , popupWebViewSelector


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

-- | @- init@
init_ :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebExtensionAction)
init_ wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebExtensionAction)
new wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Triggers the dismissal process of the popup.
--
-- Invoke this method to manage the popup's lifecycle, ensuring the web view is unloaded and resources are released once the popup closes. This method is automatically called upon the dismissal of the action's ``UIViewController`` or ``NSPopover``.  For custom scenarios where the popup's lifecycle is manually managed, it must be explicitly invoked to ensure proper closure.
--
-- ObjC selector: @- closePopup@
closePopup :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO ()
closePopup wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "closePopup") retVoid []

-- | The extension context to which this action is related.
--
-- ObjC selector: @- webExtensionContext@
webExtensionContext :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebExtensionContext)
webExtensionContext wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "webExtensionContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized display label for the action.
--
-- ObjC selector: @- label@
label :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSString)
label wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The badge text for the action.
--
-- Provides the text that appears on the badge for the action. An empty string signifies that no badge should be shown.
--
-- ObjC selector: @- badgeText@
badgeText :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSString)
badgeText wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "badgeText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value indicating whether the badge text is unread.
--
-- This property is automatically set to @YES@ when ``badgeText`` changes and is not empty. If ``badgeText`` becomes empty or the popup associated with the action is presented, this property is automatically set to @NO@. Additionally, it should be set to @NO@ by the app when the badge has been presented to the user. This property is useful for higher-level notification badges when extensions might be hidden behind an action sheet.
--
-- ObjC selector: @- hasUnreadBadgeText@
hasUnreadBadgeText :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO Bool
hasUnreadBadgeText wkWebExtensionAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionAction (mkSelector "hasUnreadBadgeText") retCULong []

-- | A Boolean value indicating whether the badge text is unread.
--
-- This property is automatically set to @YES@ when ``badgeText`` changes and is not empty. If ``badgeText`` becomes empty or the popup associated with the action is presented, this property is automatically set to @NO@. Additionally, it should be set to @NO@ by the app when the badge has been presented to the user. This property is useful for higher-level notification badges when extensions might be hidden behind an action sheet.
--
-- ObjC selector: @- setHasUnreadBadgeText:@
setHasUnreadBadgeText :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> Bool -> IO ()
setHasUnreadBadgeText wkWebExtensionAction  value =
  sendMsg wkWebExtensionAction (mkSelector "setHasUnreadBadgeText:") retVoid [argCULong (if value then 1 else 0)]

-- | The name shown when inspecting the popup web view.
--
-- This is the text that will appear when inspecting the popup web view.
--
-- ObjC selector: @- inspectionName@
inspectionName :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSString)
inspectionName wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "inspectionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name shown when inspecting the popup web view.
--
-- This is the text that will appear when inspecting the popup web view.
--
-- ObjC selector: @- setInspectionName:@
setInspectionName :: (IsWKWebExtensionAction wkWebExtensionAction, IsNSString value) => wkWebExtensionAction -> value -> IO ()
setInspectionName wkWebExtensionAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionAction (mkSelector "setInspectionName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating whether the action is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO Bool
enabled wkWebExtensionAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionAction (mkSelector "enabled") retCULong []

-- | @- menuItems@
menuItems :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSArray)
menuItems wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "menuItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value indicating whether the action has a popup.
--
-- Use this property to check if the action has a popup before attempting to show any popup views.
--
-- ObjC selector: @- presentsPopup@
presentsPopup :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO Bool
presentsPopup wkWebExtensionAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionAction (mkSelector "presentsPopup") retCULong []

-- | A popover that presents a web view loaded with the popup page for this action, or @nil@ if no popup is specified.
--
-- This popover contains a view controller with a web view preloaded with the popup page. It automatically adjusts its size to fit the web view's content size. The ``presentsPopup`` property should be checked to determine the availability of a popup before using this property.  Dismissing the popover will close the popup and unload the web view.
--
-- presentsPopup
--
-- ObjC selector: @- popupPopover@
popupPopover :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSPopover)
popupPopover wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "popupPopover") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A web view loaded with the popup page for this action, or @nil@ if no popup is specified.
--
-- The web view will be preloaded with the popup page upon first access or after it has been unloaded. Use the ``presentsPopup`` property to determine whether a popup should be displayed before using this property.
--
-- presentsPopup
--
-- ObjC selector: @- popupWebView@
popupWebView :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebView)
popupWebView wkWebExtensionAction  =
  sendMsg wkWebExtensionAction (mkSelector "popupWebView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @closePopup@
closePopupSelector :: Selector
closePopupSelector = mkSelector "closePopup"

-- | @Selector@ for @webExtensionContext@
webExtensionContextSelector :: Selector
webExtensionContextSelector = mkSelector "webExtensionContext"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @badgeText@
badgeTextSelector :: Selector
badgeTextSelector = mkSelector "badgeText"

-- | @Selector@ for @hasUnreadBadgeText@
hasUnreadBadgeTextSelector :: Selector
hasUnreadBadgeTextSelector = mkSelector "hasUnreadBadgeText"

-- | @Selector@ for @setHasUnreadBadgeText:@
setHasUnreadBadgeTextSelector :: Selector
setHasUnreadBadgeTextSelector = mkSelector "setHasUnreadBadgeText:"

-- | @Selector@ for @inspectionName@
inspectionNameSelector :: Selector
inspectionNameSelector = mkSelector "inspectionName"

-- | @Selector@ for @setInspectionName:@
setInspectionNameSelector :: Selector
setInspectionNameSelector = mkSelector "setInspectionName:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @menuItems@
menuItemsSelector :: Selector
menuItemsSelector = mkSelector "menuItems"

-- | @Selector@ for @presentsPopup@
presentsPopupSelector :: Selector
presentsPopupSelector = mkSelector "presentsPopup"

-- | @Selector@ for @popupPopover@
popupPopoverSelector :: Selector
popupPopoverSelector = mkSelector "popupPopover"

-- | @Selector@ for @popupWebView@
popupWebViewSelector :: Selector
popupWebViewSelector = mkSelector "popupWebView"

