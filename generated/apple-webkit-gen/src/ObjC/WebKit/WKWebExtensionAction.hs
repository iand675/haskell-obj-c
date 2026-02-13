{-# LANGUAGE DataKinds #-}
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
  , associatedTab
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
  , associatedTabSelector
  , badgeTextSelector
  , closePopupSelector
  , enabledSelector
  , hasUnreadBadgeTextSelector
  , initSelector
  , inspectionNameSelector
  , labelSelector
  , menuItemsSelector
  , newSelector
  , popupPopoverSelector
  , popupWebViewSelector
  , presentsPopupSelector
  , setHasUnreadBadgeTextSelector
  , setInspectionNameSelector
  , webExtensionContextSelector


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

-- | @- init@
init_ :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebExtensionAction)
init_ wkWebExtensionAction =
  sendOwnedMessage wkWebExtensionAction initSelector

-- | @- new@
new :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebExtensionAction)
new wkWebExtensionAction =
  sendOwnedMessage wkWebExtensionAction newSelector

-- | Triggers the dismissal process of the popup.
--
-- Invoke this method to manage the popup's lifecycle, ensuring the web view is unloaded and resources are released once the popup closes. This method is automatically called upon the dismissal of the action's ``UIViewController`` or ``NSPopover``.  For custom scenarios where the popup's lifecycle is manually managed, it must be explicitly invoked to ensure proper closure.
--
-- ObjC selector: @- closePopup@
closePopup :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO ()
closePopup wkWebExtensionAction =
  sendMessage wkWebExtensionAction closePopupSelector

-- | The extension context to which this action is related.
--
-- ObjC selector: @- webExtensionContext@
webExtensionContext :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebExtensionContext)
webExtensionContext wkWebExtensionAction =
  sendMessage wkWebExtensionAction webExtensionContextSelector

-- | The tab that this action is associated with, or @nil@ if it is the default action.
--
-- When this property is @nil@, it indicates that the action is the default action and not associated with a specific tab.
--
-- ObjC selector: @- associatedTab@
associatedTab :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO RawId
associatedTab wkWebExtensionAction =
  sendMessage wkWebExtensionAction associatedTabSelector

-- | The localized display label for the action.
--
-- ObjC selector: @- label@
label :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSString)
label wkWebExtensionAction =
  sendMessage wkWebExtensionAction labelSelector

-- | The badge text for the action.
--
-- Provides the text that appears on the badge for the action. An empty string signifies that no badge should be shown.
--
-- ObjC selector: @- badgeText@
badgeText :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSString)
badgeText wkWebExtensionAction =
  sendMessage wkWebExtensionAction badgeTextSelector

-- | A Boolean value indicating whether the badge text is unread.
--
-- This property is automatically set to @YES@ when ``badgeText`` changes and is not empty. If ``badgeText`` becomes empty or the popup associated with the action is presented, this property is automatically set to @NO@. Additionally, it should be set to @NO@ by the app when the badge has been presented to the user. This property is useful for higher-level notification badges when extensions might be hidden behind an action sheet.
--
-- ObjC selector: @- hasUnreadBadgeText@
hasUnreadBadgeText :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO Bool
hasUnreadBadgeText wkWebExtensionAction =
  sendMessage wkWebExtensionAction hasUnreadBadgeTextSelector

-- | A Boolean value indicating whether the badge text is unread.
--
-- This property is automatically set to @YES@ when ``badgeText`` changes and is not empty. If ``badgeText`` becomes empty or the popup associated with the action is presented, this property is automatically set to @NO@. Additionally, it should be set to @NO@ by the app when the badge has been presented to the user. This property is useful for higher-level notification badges when extensions might be hidden behind an action sheet.
--
-- ObjC selector: @- setHasUnreadBadgeText:@
setHasUnreadBadgeText :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> Bool -> IO ()
setHasUnreadBadgeText wkWebExtensionAction value =
  sendMessage wkWebExtensionAction setHasUnreadBadgeTextSelector value

-- | The name shown when inspecting the popup web view.
--
-- This is the text that will appear when inspecting the popup web view.
--
-- ObjC selector: @- inspectionName@
inspectionName :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSString)
inspectionName wkWebExtensionAction =
  sendMessage wkWebExtensionAction inspectionNameSelector

-- | The name shown when inspecting the popup web view.
--
-- This is the text that will appear when inspecting the popup web view.
--
-- ObjC selector: @- setInspectionName:@
setInspectionName :: (IsWKWebExtensionAction wkWebExtensionAction, IsNSString value) => wkWebExtensionAction -> value -> IO ()
setInspectionName wkWebExtensionAction value =
  sendMessage wkWebExtensionAction setInspectionNameSelector (toNSString value)

-- | A Boolean value indicating whether the action is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO Bool
enabled wkWebExtensionAction =
  sendMessage wkWebExtensionAction enabledSelector

-- | @- menuItems@
menuItems :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSArray)
menuItems wkWebExtensionAction =
  sendMessage wkWebExtensionAction menuItemsSelector

-- | A Boolean value indicating whether the action has a popup.
--
-- Use this property to check if the action has a popup before attempting to show any popup views.
--
-- ObjC selector: @- presentsPopup@
presentsPopup :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO Bool
presentsPopup wkWebExtensionAction =
  sendMessage wkWebExtensionAction presentsPopupSelector

-- | A popover that presents a web view loaded with the popup page for this action, or @nil@ if no popup is specified.
--
-- This popover contains a view controller with a web view preloaded with the popup page. It automatically adjusts its size to fit the web view's content size. The ``presentsPopup`` property should be checked to determine the availability of a popup before using this property.  Dismissing the popover will close the popup and unload the web view.
--
-- presentsPopup
--
-- ObjC selector: @- popupPopover@
popupPopover :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id NSPopover)
popupPopover wkWebExtensionAction =
  sendMessage wkWebExtensionAction popupPopoverSelector

-- | A web view loaded with the popup page for this action, or @nil@ if no popup is specified.
--
-- The web view will be preloaded with the popup page upon first access or after it has been unloaded. Use the ``presentsPopup`` property to determine whether a popup should be displayed before using this property.
--
-- presentsPopup
--
-- ObjC selector: @- popupWebView@
popupWebView :: IsWKWebExtensionAction wkWebExtensionAction => wkWebExtensionAction -> IO (Id WKWebView)
popupWebView wkWebExtensionAction =
  sendMessage wkWebExtensionAction popupWebViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionAction)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionAction)
newSelector = mkSelector "new"

-- | @Selector@ for @closePopup@
closePopupSelector :: Selector '[] ()
closePopupSelector = mkSelector "closePopup"

-- | @Selector@ for @webExtensionContext@
webExtensionContextSelector :: Selector '[] (Id WKWebExtensionContext)
webExtensionContextSelector = mkSelector "webExtensionContext"

-- | @Selector@ for @associatedTab@
associatedTabSelector :: Selector '[] RawId
associatedTabSelector = mkSelector "associatedTab"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @badgeText@
badgeTextSelector :: Selector '[] (Id NSString)
badgeTextSelector = mkSelector "badgeText"

-- | @Selector@ for @hasUnreadBadgeText@
hasUnreadBadgeTextSelector :: Selector '[] Bool
hasUnreadBadgeTextSelector = mkSelector "hasUnreadBadgeText"

-- | @Selector@ for @setHasUnreadBadgeText:@
setHasUnreadBadgeTextSelector :: Selector '[Bool] ()
setHasUnreadBadgeTextSelector = mkSelector "setHasUnreadBadgeText:"

-- | @Selector@ for @inspectionName@
inspectionNameSelector :: Selector '[] (Id NSString)
inspectionNameSelector = mkSelector "inspectionName"

-- | @Selector@ for @setInspectionName:@
setInspectionNameSelector :: Selector '[Id NSString] ()
setInspectionNameSelector = mkSelector "setInspectionName:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @menuItems@
menuItemsSelector :: Selector '[] (Id NSArray)
menuItemsSelector = mkSelector "menuItems"

-- | @Selector@ for @presentsPopup@
presentsPopupSelector :: Selector '[] Bool
presentsPopupSelector = mkSelector "presentsPopup"

-- | @Selector@ for @popupPopover@
popupPopoverSelector :: Selector '[] (Id NSPopover)
popupPopoverSelector = mkSelector "popupPopover"

-- | @Selector@ for @popupWebView@
popupWebViewSelector :: Selector '[] (Id WKWebView)
popupWebViewSelector = mkSelector "popupWebView"

