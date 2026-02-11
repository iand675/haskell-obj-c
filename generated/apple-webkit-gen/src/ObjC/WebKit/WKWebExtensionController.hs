{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionController`` object manages a set of loaded extension contexts.
--
-- You can have one or more extension controller instances, allowing different parts of the app to use different sets of extensions. A controller is associated with ``WKWebView`` via the ``webExtensionController`` property on ``WKWebViewConfiguration``.
--
-- Generated bindings for @WKWebExtensionController@.
module ObjC.WebKit.WKWebExtensionController
  ( WKWebExtensionController
  , IsWKWebExtensionController(..)
  , init_
  , initWithConfiguration
  , loadExtensionContext_error
  , unloadExtensionContext_error
  , extensionContextForExtension
  , extensionContextForURL
  , fetchDataRecordOfTypes_forExtensionContext_completionHandler
  , removeDataOfTypes_fromDataRecords_completionHandler
  , didOpenWindow
  , didCloseWindow
  , didFocusWindow
  , didOpenTab
  , didCloseTab_windowIsClosing
  , didActivateTab_previousActiveTab
  , didSelectTabs
  , didDeselectTabs
  , didMoveTab_fromIndex_inWindow
  , didReplaceTab_withTab
  , didChangeTabProperties_forTab
  , delegate
  , setDelegate
  , configuration
  , extensions
  , extensionContexts
  , allExtensionDataTypes
  , initSelector
  , initWithConfigurationSelector
  , loadExtensionContext_errorSelector
  , unloadExtensionContext_errorSelector
  , extensionContextForExtensionSelector
  , extensionContextForURLSelector
  , fetchDataRecordOfTypes_forExtensionContext_completionHandlerSelector
  , removeDataOfTypes_fromDataRecords_completionHandlerSelector
  , didOpenWindowSelector
  , didCloseWindowSelector
  , didFocusWindowSelector
  , didOpenTabSelector
  , didCloseTab_windowIsClosingSelector
  , didActivateTab_previousActiveTabSelector
  , didSelectTabsSelector
  , didDeselectTabsSelector
  , didMoveTab_fromIndex_inWindowSelector
  , didReplaceTab_withTabSelector
  , didChangeTabProperties_forTabSelector
  , delegateSelector
  , setDelegateSelector
  , configurationSelector
  , extensionsSelector
  , extensionContextsSelector
  , allExtensionDataTypesSelector

  -- * Enum types
  , WKWebExtensionTabChangedProperties(WKWebExtensionTabChangedProperties)
  , pattern WKWebExtensionTabChangedPropertiesNone
  , pattern WKWebExtensionTabChangedPropertiesLoading
  , pattern WKWebExtensionTabChangedPropertiesMuted
  , pattern WKWebExtensionTabChangedPropertiesPinned
  , pattern WKWebExtensionTabChangedPropertiesPlayingAudio
  , pattern WKWebExtensionTabChangedPropertiesReaderMode
  , pattern WKWebExtensionTabChangedPropertiesSize
  , pattern WKWebExtensionTabChangedPropertiesTitle
  , pattern WKWebExtensionTabChangedPropertiesURL
  , pattern WKWebExtensionTabChangedPropertiesZoomFactor

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
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns a web extension controller initialized with the default configuration.
--
-- Returns: An initialized web extension controller, or nil if the object could not be initialized.
--
-- This is a designated initializer. You can use ``initWithConfiguration:`` to initialize an instance with a configuration.
--
-- initWithConfiguration:
--
-- ObjC selector: @- init@
init_ :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> IO (Id WKWebExtensionController)
init_ wkWebExtensionController  =
    sendMsg wkWebExtensionController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns a web extension controller initialized with the specified configuration.
--
-- @configuration@ — The configuration for the new web extension controller.
--
-- Returns: An initialized web extension controller, or nil if the object could not be initialized.
--
-- This is a designated initializer. You can use ``init:`` to initialize an instance with the default configuration. The initializer copies the specified configuration, so mutating the configuration after invoking the initializer has no effect on the web extension controller.
--
-- init
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsWKWebExtensionController wkWebExtensionController, IsWKWebExtensionControllerConfiguration configuration) => wkWebExtensionController -> configuration -> IO (Id WKWebExtensionController)
initWithConfiguration wkWebExtensionController  configuration =
  withObjCPtr configuration $ \raw_configuration ->
      sendMsg wkWebExtensionController (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | Loads the specified extension context.
--
-- Causes the context to start, loading any background content, and injecting any content into relevant tabs.
--
-- @error@ — Set to @nil@ or an @NSError@ instance if an error occurred.
--
-- Returns: A Boolean value indicating if the context was successfully loaded.
--
-- loadExtensionContext:
--
-- ObjC selector: @- loadExtensionContext:error:@
loadExtensionContext_error :: (IsWKWebExtensionController wkWebExtensionController, IsWKWebExtensionContext extensionContext, IsNSError error_) => wkWebExtensionController -> extensionContext -> error_ -> IO Bool
loadExtensionContext_error wkWebExtensionController  extensionContext error_ =
  withObjCPtr extensionContext $ \raw_extensionContext ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionController (mkSelector "loadExtensionContext:error:") retCULong [argPtr (castPtr raw_extensionContext :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Unloads the specified extension context.
--
-- Causes the context to stop running.
--
-- @error@ — Set to @nil@ or an @NSError@ instance if an error occurred.
--
-- Returns: A Boolean value indicating if the context was successfully unloaded.
--
-- unloadExtensionContext:
--
-- ObjC selector: @- unloadExtensionContext:error:@
unloadExtensionContext_error :: (IsWKWebExtensionController wkWebExtensionController, IsWKWebExtensionContext extensionContext, IsNSError error_) => wkWebExtensionController -> extensionContext -> error_ -> IO Bool
unloadExtensionContext_error wkWebExtensionController  extensionContext error_ =
  withObjCPtr extensionContext $ \raw_extensionContext ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionController (mkSelector "unloadExtensionContext:error:") retCULong [argPtr (castPtr raw_extensionContext :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Returns a loaded extension context for the specified extension.
--
-- @extension@ — An extension to lookup.
--
-- Returns: An extension context or @nil@ if no match was found.
--
-- extensions
--
-- ObjC selector: @- extensionContextForExtension:@
extensionContextForExtension :: (IsWKWebExtensionController wkWebExtensionController, IsWKWebExtension extension) => wkWebExtensionController -> extension -> IO (Id WKWebExtensionContext)
extensionContextForExtension wkWebExtensionController  extension =
  withObjCPtr extension $ \raw_extension ->
      sendMsg wkWebExtensionController (mkSelector "extensionContextForExtension:") (retPtr retVoid) [argPtr (castPtr raw_extension :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a loaded extension context matching the specified URL.
--
-- @URL@ — The URL to lookup.
--
-- Returns: An extension context or @nil@ if no match was found.
--
-- This method is useful for determining the extension context to use when about to navigate to an extension URL. For example, you could use this method to retrieve the appropriate extension context and then use its ``webViewConfiguration`` property to configure a web view for loading that URL.
--
-- ObjC selector: @- extensionContextForURL:@
extensionContextForURL :: (IsWKWebExtensionController wkWebExtensionController, IsNSURL url) => wkWebExtensionController -> url -> IO (Id WKWebExtensionContext)
extensionContextForURL wkWebExtensionController  url =
  withObjCPtr url $ \raw_url ->
      sendMsg wkWebExtensionController (mkSelector "extensionContextForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | Fetches a data record containing the given extension data types for a specific known web extension context.
--
-- @dataTypes@ — The extension data types to fetch records for.
--
-- @extensionContext@ — The specific web extension context to fetch records for.
--
-- @completionHandler@ — A block to invoke when the data record has been fetched.
--
-- Note: The extension does not need to be loaded to be included in the result.
--
-- ObjC selector: @- fetchDataRecordOfTypes:forExtensionContext:completionHandler:@
fetchDataRecordOfTypes_forExtensionContext_completionHandler :: (IsWKWebExtensionController wkWebExtensionController, IsNSSet dataTypes, IsWKWebExtensionContext extensionContext) => wkWebExtensionController -> dataTypes -> extensionContext -> Ptr () -> IO ()
fetchDataRecordOfTypes_forExtensionContext_completionHandler wkWebExtensionController  dataTypes extensionContext completionHandler =
  withObjCPtr dataTypes $ \raw_dataTypes ->
    withObjCPtr extensionContext $ \raw_extensionContext ->
        sendMsg wkWebExtensionController (mkSelector "fetchDataRecordOfTypes:forExtensionContext:completionHandler:") retVoid [argPtr (castPtr raw_dataTypes :: Ptr ()), argPtr (castPtr raw_extensionContext :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Removes extension data of the given types for the given data records.
--
-- @dataTypes@ — The extension data types that should be removed.
--
-- @dataRecords@ — The extension data records to delete data from.
--
-- @completionHandler@ — A block to invoke when the data has been removed.
--
-- ObjC selector: @- removeDataOfTypes:fromDataRecords:completionHandler:@
removeDataOfTypes_fromDataRecords_completionHandler :: (IsWKWebExtensionController wkWebExtensionController, IsNSSet dataTypes, IsNSArray dataRecords) => wkWebExtensionController -> dataTypes -> dataRecords -> Ptr () -> IO ()
removeDataOfTypes_fromDataRecords_completionHandler wkWebExtensionController  dataTypes dataRecords completionHandler =
  withObjCPtr dataTypes $ \raw_dataTypes ->
    withObjCPtr dataRecords $ \raw_dataRecords ->
        sendMsg wkWebExtensionController (mkSelector "removeDataOfTypes:fromDataRecords:completionHandler:") retVoid [argPtr (castPtr raw_dataTypes :: Ptr ()), argPtr (castPtr raw_dataRecords :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Should be called by the app when a new window is opened to fire appropriate events with all loaded web extensions.
--
-- @newWindow@ — The newly opened window.
--
-- This method informs all loaded extensions of the opening of a new window, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- didCloseWindow:
--
-- ObjC selector: @- didOpenWindow:@
didOpenWindow :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> IO ()
didOpenWindow wkWebExtensionController  newWindow =
    sendMsg wkWebExtensionController (mkSelector "didOpenWindow:") retVoid [argPtr (castPtr (unRawId newWindow) :: Ptr ())]

-- | Should be called by the app when a window is closed to fire appropriate events with all loaded web extensions.
--
-- @newWindow@ — The window that was closed.
--
-- This method informs all loaded extensions of the closure of a window, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- didOpenWindow:
--
-- ObjC selector: @- didCloseWindow:@
didCloseWindow :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> IO ()
didCloseWindow wkWebExtensionController  closedWindow =
    sendMsg wkWebExtensionController (mkSelector "didCloseWindow:") retVoid [argPtr (castPtr (unRawId closedWindow) :: Ptr ())]

-- | Should be called by the app when a window gains focus to fire appropriate events with all loaded web extensions.
--
-- @focusedWindow@ — The window that gained focus, or @nil@ if no window has focus or a window has focus that is not visible to extensions.
--
-- This method informs all loaded extensions of the focused window, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- ObjC selector: @- didFocusWindow:@
didFocusWindow :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> IO ()
didFocusWindow wkWebExtensionController  focusedWindow =
    sendMsg wkWebExtensionController (mkSelector "didFocusWindow:") retVoid [argPtr (castPtr (unRawId focusedWindow) :: Ptr ())]

-- | Should be called by the app when a new tab is opened to fire appropriate events with all loaded web extensions.
--
-- @newTab@ — The newly opened tab.
--
-- This method informs all loaded extensions of the opening of a new tab, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- didCloseTab:
--
-- ObjC selector: @- didOpenTab:@
didOpenTab :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> IO ()
didOpenTab wkWebExtensionController  newTab =
    sendMsg wkWebExtensionController (mkSelector "didOpenTab:") retVoid [argPtr (castPtr (unRawId newTab) :: Ptr ())]

-- | Should be called by the app when a tab is closed to fire appropriate events with all loaded web extensions.
--
-- @closedTab@ — The tab that was closed.
--
-- @windowIsClosing@ — A boolean value indicating whether the window containing the tab is also closing.
--
-- This method informs all loaded extensions of the closing of a tab, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- didOpenTab:
--
-- ObjC selector: @- didCloseTab:windowIsClosing:@
didCloseTab_windowIsClosing :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> Bool -> IO ()
didCloseTab_windowIsClosing wkWebExtensionController  closedTab windowIsClosing =
    sendMsg wkWebExtensionController (mkSelector "didCloseTab:windowIsClosing:") retVoid [argPtr (castPtr (unRawId closedTab) :: Ptr ()), argCULong (if windowIsClosing then 1 else 0)]

-- | Should be called by the app when a tab is activated to notify all loaded web extensions.
--
-- @activatedTab@ — The tab that has become active.
--
-- @previousTab@ — The tab that was active before. This parameter can be @nil@ if there was no previously active tab.
--
-- This method informs all loaded extensions of the tab activation, ensuring consistent state awareness across extensions. If the intention is to inform only a specific extension, use the respective method on that extension's context instead.
--
-- ObjC selector: @- didActivateTab:previousActiveTab:@
didActivateTab_previousActiveTab :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> RawId -> IO ()
didActivateTab_previousActiveTab wkWebExtensionController  activatedTab previousTab =
    sendMsg wkWebExtensionController (mkSelector "didActivateTab:previousActiveTab:") retVoid [argPtr (castPtr (unRawId activatedTab) :: Ptr ()), argPtr (castPtr (unRawId previousTab) :: Ptr ())]

-- | Should be called by the app when tabs are selected to fire appropriate events with all loaded web extensions.
--
-- @selectedTabs@ — The set of tabs that were selected.
--
-- This method informs all loaded extensions that tabs have been selected, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- ObjC selector: @- didSelectTabs:@
didSelectTabs :: (IsWKWebExtensionController wkWebExtensionController, IsNSArray selectedTabs) => wkWebExtensionController -> selectedTabs -> IO ()
didSelectTabs wkWebExtensionController  selectedTabs =
  withObjCPtr selectedTabs $ \raw_selectedTabs ->
      sendMsg wkWebExtensionController (mkSelector "didSelectTabs:") retVoid [argPtr (castPtr raw_selectedTabs :: Ptr ())]

-- | Should be called by the app when tabs are deselected to fire appropriate events with all loaded web extensions.
--
-- @deselectedTabs@ — The set of tabs that were deselected.
--
-- This method informs all loaded extensions that tabs have been deselected, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- ObjC selector: @- didDeselectTabs:@
didDeselectTabs :: (IsWKWebExtensionController wkWebExtensionController, IsNSArray deselectedTabs) => wkWebExtensionController -> deselectedTabs -> IO ()
didDeselectTabs wkWebExtensionController  deselectedTabs =
  withObjCPtr deselectedTabs $ \raw_deselectedTabs ->
      sendMsg wkWebExtensionController (mkSelector "didDeselectTabs:") retVoid [argPtr (castPtr raw_deselectedTabs :: Ptr ())]

-- | Should be called by the app when a tab is moved to fire appropriate events with all loaded web extensions.
--
-- @movedTab@ — The tab that was moved.
--
-- @index@ — The old index of the tab within the window.
--
-- @oldWindow@ — The window that the tab was moved from, or @nil@ if the tab is moving from no open window.
--
-- This method informs all loaded extensions of the movement of a tab, ensuring consistent understanding across extensions. If the window is staying the same, the current window should be specified. If the intention is to inform only a specific extension, use the respective method on that extension's context instead.
--
-- ObjC selector: @- didMoveTab:fromIndex:inWindow:@
didMoveTab_fromIndex_inWindow :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> CULong -> RawId -> IO ()
didMoveTab_fromIndex_inWindow wkWebExtensionController  movedTab index oldWindow =
    sendMsg wkWebExtensionController (mkSelector "didMoveTab:fromIndex:inWindow:") retVoid [argPtr (castPtr (unRawId movedTab) :: Ptr ()), argCULong index, argPtr (castPtr (unRawId oldWindow) :: Ptr ())]

-- | Should be called by the app when a tab is replaced by another tab to fire appropriate events with all loaded web extensions.
--
-- @oldTab@ — The tab that was replaced.
--
-- @newTab@ — The tab that replaced the old tab.
--
-- This method informs all loaded extensions of the replacement of a tab, ensuring consistent understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- ObjC selector: @- didReplaceTab:withTab:@
didReplaceTab_withTab :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> RawId -> IO ()
didReplaceTab_withTab wkWebExtensionController  oldTab newTab =
    sendMsg wkWebExtensionController (mkSelector "didReplaceTab:withTab:") retVoid [argPtr (castPtr (unRawId oldTab) :: Ptr ()), argPtr (castPtr (unRawId newTab) :: Ptr ())]

-- | Should be called by the app when the properties of a tab are changed to fire appropriate events with all loaded web extensions.
--
-- @properties@ — The properties of the tab that were changed.
--
-- @changedTab@ — The tab whose properties were changed.
--
-- This method informs all loaded extensions of changes to tab properties, ensuring a unified understanding across extensions. If the intention is to inform only a specific extension, you should use the respective method on that extension's context instead.
--
-- ObjC selector: @- didChangeTabProperties:forTab:@
didChangeTabProperties_forTab :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> WKWebExtensionTabChangedProperties -> RawId -> IO ()
didChangeTabProperties_forTab wkWebExtensionController  properties changedTab =
    sendMsg wkWebExtensionController (mkSelector "didChangeTabProperties:forTab:") retVoid [argCULong (coerce properties), argPtr (castPtr (unRawId changedTab) :: Ptr ())]

-- | The extension controller delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> IO RawId
delegate wkWebExtensionController  =
    fmap (RawId . castPtr) $ sendMsg wkWebExtensionController (mkSelector "delegate") (retPtr retVoid) []

-- | The extension controller delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> RawId -> IO ()
setDelegate wkWebExtensionController  value =
    sendMsg wkWebExtensionController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A copy of the configuration with which the web extension controller was initialized.
--
-- Mutating the configuration has no effect on the web extension controller.
--
-- ObjC selector: @- configuration@
configuration :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> IO (Id WKWebExtensionControllerConfiguration)
configuration wkWebExtensionController  =
    sendMsg wkWebExtensionController (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A set of all the currently loaded extensions.
--
-- extensionContexts
--
-- ObjC selector: @- extensions@
extensions :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> IO (Id NSSet)
extensions wkWebExtensionController  =
    sendMsg wkWebExtensionController (mkSelector "extensions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A set of all the currently loaded extension contexts.
--
-- extensions
--
-- ObjC selector: @- extensionContexts@
extensionContexts :: IsWKWebExtensionController wkWebExtensionController => wkWebExtensionController -> IO (Id NSSet)
extensionContexts wkWebExtensionController  =
    sendMsg wkWebExtensionController (mkSelector "extensionContexts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a set of all available extension data types.
--
-- ObjC selector: @+ allExtensionDataTypes@
allExtensionDataTypes :: IO (Id NSSet)
allExtensionDataTypes  =
  do
    cls' <- getRequiredClass "WKWebExtensionController"
    sendClassMsg cls' (mkSelector "allExtensionDataTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @loadExtensionContext:error:@
loadExtensionContext_errorSelector :: Selector
loadExtensionContext_errorSelector = mkSelector "loadExtensionContext:error:"

-- | @Selector@ for @unloadExtensionContext:error:@
unloadExtensionContext_errorSelector :: Selector
unloadExtensionContext_errorSelector = mkSelector "unloadExtensionContext:error:"

-- | @Selector@ for @extensionContextForExtension:@
extensionContextForExtensionSelector :: Selector
extensionContextForExtensionSelector = mkSelector "extensionContextForExtension:"

-- | @Selector@ for @extensionContextForURL:@
extensionContextForURLSelector :: Selector
extensionContextForURLSelector = mkSelector "extensionContextForURL:"

-- | @Selector@ for @fetchDataRecordOfTypes:forExtensionContext:completionHandler:@
fetchDataRecordOfTypes_forExtensionContext_completionHandlerSelector :: Selector
fetchDataRecordOfTypes_forExtensionContext_completionHandlerSelector = mkSelector "fetchDataRecordOfTypes:forExtensionContext:completionHandler:"

-- | @Selector@ for @removeDataOfTypes:fromDataRecords:completionHandler:@
removeDataOfTypes_fromDataRecords_completionHandlerSelector :: Selector
removeDataOfTypes_fromDataRecords_completionHandlerSelector = mkSelector "removeDataOfTypes:fromDataRecords:completionHandler:"

-- | @Selector@ for @didOpenWindow:@
didOpenWindowSelector :: Selector
didOpenWindowSelector = mkSelector "didOpenWindow:"

-- | @Selector@ for @didCloseWindow:@
didCloseWindowSelector :: Selector
didCloseWindowSelector = mkSelector "didCloseWindow:"

-- | @Selector@ for @didFocusWindow:@
didFocusWindowSelector :: Selector
didFocusWindowSelector = mkSelector "didFocusWindow:"

-- | @Selector@ for @didOpenTab:@
didOpenTabSelector :: Selector
didOpenTabSelector = mkSelector "didOpenTab:"

-- | @Selector@ for @didCloseTab:windowIsClosing:@
didCloseTab_windowIsClosingSelector :: Selector
didCloseTab_windowIsClosingSelector = mkSelector "didCloseTab:windowIsClosing:"

-- | @Selector@ for @didActivateTab:previousActiveTab:@
didActivateTab_previousActiveTabSelector :: Selector
didActivateTab_previousActiveTabSelector = mkSelector "didActivateTab:previousActiveTab:"

-- | @Selector@ for @didSelectTabs:@
didSelectTabsSelector :: Selector
didSelectTabsSelector = mkSelector "didSelectTabs:"

-- | @Selector@ for @didDeselectTabs:@
didDeselectTabsSelector :: Selector
didDeselectTabsSelector = mkSelector "didDeselectTabs:"

-- | @Selector@ for @didMoveTab:fromIndex:inWindow:@
didMoveTab_fromIndex_inWindowSelector :: Selector
didMoveTab_fromIndex_inWindowSelector = mkSelector "didMoveTab:fromIndex:inWindow:"

-- | @Selector@ for @didReplaceTab:withTab:@
didReplaceTab_withTabSelector :: Selector
didReplaceTab_withTabSelector = mkSelector "didReplaceTab:withTab:"

-- | @Selector@ for @didChangeTabProperties:forTab:@
didChangeTabProperties_forTabSelector :: Selector
didChangeTabProperties_forTabSelector = mkSelector "didChangeTabProperties:forTab:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @extensions@
extensionsSelector :: Selector
extensionsSelector = mkSelector "extensions"

-- | @Selector@ for @extensionContexts@
extensionContextsSelector :: Selector
extensionContextsSelector = mkSelector "extensionContexts"

-- | @Selector@ for @allExtensionDataTypes@
allExtensionDataTypesSelector :: Selector
allExtensionDataTypesSelector = mkSelector "allExtensionDataTypes"

