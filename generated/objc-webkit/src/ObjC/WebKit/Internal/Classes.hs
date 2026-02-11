{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.WebKit.Internal.Classes (
    module ObjC.WebKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.JavaScriptCore.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.JavaScriptCore.Internal.Classes

-- ---------- WKBackForwardList ----------

-- | A WKBackForwardList object is a list of webpages previously visited in a web view that can be reached by going back or forward.
-- 
-- Phantom type for @WKBackForwardList@.
data WKBackForwardList

instance IsObjCObject (Id WKBackForwardList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKBackForwardList"

class IsNSObject a => IsWKBackForwardList a where
  toWKBackForwardList :: a -> Id WKBackForwardList

instance IsWKBackForwardList (Id WKBackForwardList) where
  toWKBackForwardList = unsafeCastId

instance IsNSObject (Id WKBackForwardList) where
  toNSObject = unsafeCastId

-- ---------- WKBackForwardListItem ----------

-- | A WKBackForwardListItem object represents a webpage in the back-forward list of a web view.
-- 
-- Phantom type for @WKBackForwardListItem@.
data WKBackForwardListItem

instance IsObjCObject (Id WKBackForwardListItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKBackForwardListItem"

class IsNSObject a => IsWKBackForwardListItem a where
  toWKBackForwardListItem :: a -> Id WKBackForwardListItem

instance IsWKBackForwardListItem (Id WKBackForwardListItem) where
  toWKBackForwardListItem = unsafeCastId

instance IsNSObject (Id WKBackForwardListItem) where
  toNSObject = unsafeCastId

-- ---------- WKContentRuleList ----------

-- | Phantom type for @WKContentRuleList@.
data WKContentRuleList

instance IsObjCObject (Id WKContentRuleList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKContentRuleList"

class IsNSObject a => IsWKContentRuleList a where
  toWKContentRuleList :: a -> Id WKContentRuleList

instance IsWKContentRuleList (Id WKContentRuleList) where
  toWKContentRuleList = unsafeCastId

instance IsNSObject (Id WKContentRuleList) where
  toNSObject = unsafeCastId

-- ---------- WKContentRuleListStore ----------

-- | Phantom type for @WKContentRuleListStore@.
data WKContentRuleListStore

instance IsObjCObject (Id WKContentRuleListStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKContentRuleListStore"

class IsNSObject a => IsWKContentRuleListStore a where
  toWKContentRuleListStore :: a -> Id WKContentRuleListStore

instance IsWKContentRuleListStore (Id WKContentRuleListStore) where
  toWKContentRuleListStore = unsafeCastId

instance IsNSObject (Id WKContentRuleListStore) where
  toNSObject = unsafeCastId

-- ---------- WKContentWorld ----------

-- | A WKContentWorld object allows you to separate your application's interaction with content displayed in a WKWebView into different roles that cannot interfere with one another.
--
-- WKContentWorld objects should be treated as namespaces. This is useful for keeping your application's web content environment separate from the environment of the web page content itself,as well as managing multiple different environments within your own application.For example:- If you have complex scripting logic to bridge your web content to your application but your web content also has complex scripting libraries of its own,  you avoid possible conflicts by using a client WKContentWorld.- If you are writing a general purpose web browser that supports JavaScript extensions, you would use a different client WKContentWorld for each extension.
--
-- Since a WKContentWorld object is a namespace it does not contain any data itself.For example:- If you store a variable in JavaScript in the scope of a particular WKContentWorld while viewing a particular web page document, after navigating to a new document that variable will be gone.- If you store a variable in JavaScript in the scope of a particular WKContentWorld in one WKWebView, that variable will not exist in the same world in another WKWebView.
-- 
-- Phantom type for @WKContentWorld@.
data WKContentWorld

instance IsObjCObject (Id WKContentWorld) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKContentWorld"

class IsNSObject a => IsWKContentWorld a where
  toWKContentWorld :: a -> Id WKContentWorld

instance IsWKContentWorld (Id WKContentWorld) where
  toWKContentWorld = unsafeCastId

instance IsNSObject (Id WKContentWorld) where
  toNSObject = unsafeCastId

-- ---------- WKDownload ----------

-- | Phantom type for @WKDownload@.
data WKDownload

instance IsObjCObject (Id WKDownload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKDownload"

class IsNSObject a => IsWKDownload a where
  toWKDownload :: a -> Id WKDownload

instance IsWKDownload (Id WKDownload) where
  toWKDownload = unsafeCastId

instance IsNSObject (Id WKDownload) where
  toNSObject = unsafeCastId

-- ---------- WKFindConfiguration ----------

-- | Phantom type for @WKFindConfiguration@.
data WKFindConfiguration

instance IsObjCObject (Id WKFindConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKFindConfiguration"

class IsNSObject a => IsWKFindConfiguration a where
  toWKFindConfiguration :: a -> Id WKFindConfiguration

instance IsWKFindConfiguration (Id WKFindConfiguration) where
  toWKFindConfiguration = unsafeCastId

instance IsNSObject (Id WKFindConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKFindResult ----------

-- | Phantom type for @WKFindResult@.
data WKFindResult

instance IsObjCObject (Id WKFindResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKFindResult"

class IsNSObject a => IsWKFindResult a where
  toWKFindResult :: a -> Id WKFindResult

instance IsWKFindResult (Id WKFindResult) where
  toWKFindResult = unsafeCastId

instance IsNSObject (Id WKFindResult) where
  toNSObject = unsafeCastId

-- ---------- WKFrameInfo ----------

-- | A WKFrameInfo object contains information about a frame on a webpage.
--
-- An instance of this class is a transient, data-only object; it does not uniquely identify a frame across multiple delegate method calls.
-- 
-- Phantom type for @WKFrameInfo@.
data WKFrameInfo

instance IsObjCObject (Id WKFrameInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKFrameInfo"

class IsNSObject a => IsWKFrameInfo a where
  toWKFrameInfo :: a -> Id WKFrameInfo

instance IsWKFrameInfo (Id WKFrameInfo) where
  toWKFrameInfo = unsafeCastId

instance IsNSObject (Id WKFrameInfo) where
  toNSObject = unsafeCastId

-- ---------- WKHTTPCookieStore ----------

-- | A WKHTTPCookieStore object allows managing the HTTP cookies associated with a particular WKWebsiteDataStore.
-- 
-- Phantom type for @WKHTTPCookieStore@.
data WKHTTPCookieStore

instance IsObjCObject (Id WKHTTPCookieStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKHTTPCookieStore"

class IsNSObject a => IsWKHTTPCookieStore a where
  toWKHTTPCookieStore :: a -> Id WKHTTPCookieStore

instance IsWKHTTPCookieStore (Id WKHTTPCookieStore) where
  toWKHTTPCookieStore = unsafeCastId

instance IsNSObject (Id WKHTTPCookieStore) where
  toNSObject = unsafeCastId

-- ---------- WKNavigation ----------

-- | A WKNavigation object can be used for tracking the loading progress of a webpage.
--
-- A navigation is returned from the web view load methods, and is also passed to the navigation delegate methods, to uniquely identify a webpage load from start to finish.
-- 
-- Phantom type for @WKNavigation@.
data WKNavigation

instance IsObjCObject (Id WKNavigation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKNavigation"

class IsNSObject a => IsWKNavigation a where
  toWKNavigation :: a -> Id WKNavigation

instance IsWKNavigation (Id WKNavigation) where
  toWKNavigation = unsafeCastId

instance IsNSObject (Id WKNavigation) where
  toNSObject = unsafeCastId

-- ---------- WKNavigationAction ----------

-- | A WKNavigationAction object contains information about an action that may cause a navigation, used for making policy decisions.
-- 
-- Phantom type for @WKNavigationAction@.
data WKNavigationAction

instance IsObjCObject (Id WKNavigationAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKNavigationAction"

class IsNSObject a => IsWKNavigationAction a where
  toWKNavigationAction :: a -> Id WKNavigationAction

instance IsWKNavigationAction (Id WKNavigationAction) where
  toWKNavigationAction = unsafeCastId

instance IsNSObject (Id WKNavigationAction) where
  toNSObject = unsafeCastId

-- ---------- WKNavigationResponse ----------

-- | Contains information about a navigation response, used for making policy decisions.
-- 
-- Phantom type for @WKNavigationResponse@.
data WKNavigationResponse

instance IsObjCObject (Id WKNavigationResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKNavigationResponse"

class IsNSObject a => IsWKNavigationResponse a where
  toWKNavigationResponse :: a -> Id WKNavigationResponse

instance IsWKNavigationResponse (Id WKNavigationResponse) where
  toWKNavigationResponse = unsafeCastId

instance IsNSObject (Id WKNavigationResponse) where
  toNSObject = unsafeCastId

-- ---------- WKOpenPanelParameters ----------

-- | WKOpenPanelParameters contains parameters that a file upload control has specified.
-- 
-- Phantom type for @WKOpenPanelParameters@.
data WKOpenPanelParameters

instance IsObjCObject (Id WKOpenPanelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKOpenPanelParameters"

class IsNSObject a => IsWKOpenPanelParameters a where
  toWKOpenPanelParameters :: a -> Id WKOpenPanelParameters

instance IsWKOpenPanelParameters (Id WKOpenPanelParameters) where
  toWKOpenPanelParameters = unsafeCastId

instance IsNSObject (Id WKOpenPanelParameters) where
  toNSObject = unsafeCastId

-- ---------- WKPDFConfiguration ----------

-- | Phantom type for @WKPDFConfiguration@.
data WKPDFConfiguration

instance IsObjCObject (Id WKPDFConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKPDFConfiguration"

class IsNSObject a => IsWKPDFConfiguration a where
  toWKPDFConfiguration :: a -> Id WKPDFConfiguration

instance IsWKPDFConfiguration (Id WKPDFConfiguration) where
  toWKPDFConfiguration = unsafeCastId

instance IsNSObject (Id WKPDFConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKPreferences ----------

-- | A WKPreferences object encapsulates the preference settings for a web view. The preferences object associated with a web view is specified by its web view configuration.
-- 
-- Phantom type for @WKPreferences@.
data WKPreferences

instance IsObjCObject (Id WKPreferences) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKPreferences"

class IsNSObject a => IsWKPreferences a where
  toWKPreferences :: a -> Id WKPreferences

instance IsWKPreferences (Id WKPreferences) where
  toWKPreferences = unsafeCastId

instance IsNSObject (Id WKPreferences) where
  toNSObject = unsafeCastId

-- ---------- WKProcessPool ----------

-- | A WKProcessPool object represents a pool of web content processes. The process pool associated with a web view is specified by its web view configuration. Each web view is given its own web content process until an implementation-defined process limit is reached; after that, web views with the same process pool end up sharing web content processes.
-- 
-- Phantom type for @WKProcessPool@.
data WKProcessPool

instance IsObjCObject (Id WKProcessPool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKProcessPool"

class IsNSObject a => IsWKProcessPool a where
  toWKProcessPool :: a -> Id WKProcessPool

instance IsWKProcessPool (Id WKProcessPool) where
  toWKProcessPool = unsafeCastId

instance IsNSObject (Id WKProcessPool) where
  toNSObject = unsafeCastId

-- ---------- WKScriptMessage ----------

-- | A WKScriptMessage object contains information about a message sent from a webpage.
-- 
-- Phantom type for @WKScriptMessage@.
data WKScriptMessage

instance IsObjCObject (Id WKScriptMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKScriptMessage"

class IsNSObject a => IsWKScriptMessage a where
  toWKScriptMessage :: a -> Id WKScriptMessage

instance IsWKScriptMessage (Id WKScriptMessage) where
  toWKScriptMessage = unsafeCastId

instance IsNSObject (Id WKScriptMessage) where
  toNSObject = unsafeCastId

-- ---------- WKSecurityOrigin ----------

-- | A WKSecurityOrigin object contains information about a security origin.
--
-- An instance of this class is a transient, data-only object; it does not uniquely identify a security origin across multiple delegate method calls.
-- 
-- Phantom type for @WKSecurityOrigin@.
data WKSecurityOrigin

instance IsObjCObject (Id WKSecurityOrigin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKSecurityOrigin"

class IsNSObject a => IsWKSecurityOrigin a where
  toWKSecurityOrigin :: a -> Id WKSecurityOrigin

instance IsWKSecurityOrigin (Id WKSecurityOrigin) where
  toWKSecurityOrigin = unsafeCastId

instance IsNSObject (Id WKSecurityOrigin) where
  toNSObject = unsafeCastId

-- ---------- WKSnapshotConfiguration ----------

-- | Phantom type for @WKSnapshotConfiguration@.
data WKSnapshotConfiguration

instance IsObjCObject (Id WKSnapshotConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKSnapshotConfiguration"

class IsNSObject a => IsWKSnapshotConfiguration a where
  toWKSnapshotConfiguration :: a -> Id WKSnapshotConfiguration

instance IsWKSnapshotConfiguration (Id WKSnapshotConfiguration) where
  toWKSnapshotConfiguration = unsafeCastId

instance IsNSObject (Id WKSnapshotConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKUserContentController ----------

-- | A WKUserContentController object provides a way for JavaScript to post messages to a web view. The user content controller associated with a web view is specified by its web view configuration.
-- 
-- Phantom type for @WKUserContentController@.
data WKUserContentController

instance IsObjCObject (Id WKUserContentController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKUserContentController"

class IsNSObject a => IsWKUserContentController a where
  toWKUserContentController :: a -> Id WKUserContentController

instance IsWKUserContentController (Id WKUserContentController) where
  toWKUserContentController = unsafeCastId

instance IsNSObject (Id WKUserContentController) where
  toNSObject = unsafeCastId

-- ---------- WKUserScript ----------

-- | A
--
-- WKUserScript
--
-- object represents a script that can be injected into webpages.
-- 
-- Phantom type for @WKUserScript@.
data WKUserScript

instance IsObjCObject (Id WKUserScript) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKUserScript"

class IsNSObject a => IsWKUserScript a where
  toWKUserScript :: a -> Id WKUserScript

instance IsWKUserScript (Id WKUserScript) where
  toWKUserScript = unsafeCastId

instance IsNSObject (Id WKUserScript) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtension ----------

-- | A ``WKWebExtension`` object encapsulates a web extension’s resources that are defined by a @manifest.json@` file.
--
-- This class handles the reading and parsing of the manifest file along with the supporting resources like icons and localizations.
-- 
-- Phantom type for @WKWebExtension@.
data WKWebExtension

instance IsObjCObject (Id WKWebExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtension"

class IsNSObject a => IsWKWebExtension a where
  toWKWebExtension :: a -> Id WKWebExtension

instance IsWKWebExtension (Id WKWebExtension) where
  toWKWebExtension = unsafeCastId

instance IsNSObject (Id WKWebExtension) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionAction ----------

-- | A ``WKWebExtensionAction`` object encapsulates the properties for an individual web extension action.
--
-- Provides access to action properties such as popup, icon, and title, with tab-specific values.
-- 
-- Phantom type for @WKWebExtensionAction@.
data WKWebExtensionAction

instance IsObjCObject (Id WKWebExtensionAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionAction"

class IsNSObject a => IsWKWebExtensionAction a where
  toWKWebExtensionAction :: a -> Id WKWebExtensionAction

instance IsWKWebExtensionAction (Id WKWebExtensionAction) where
  toWKWebExtensionAction = unsafeCastId

instance IsNSObject (Id WKWebExtensionAction) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionCommand ----------

-- | A ``WKWebExtensionCommand`` object encapsulates the properties for an individual web extension command.
--
-- Provides access to command properties such as a unique identifier, a descriptive title, and shortcut keys. Commands can be used by a web extension to perform specific actions within a web extension context, such toggling features, or interacting with web content. These commands enhance the functionality of the extension by allowing users to invoke actions quickly.
-- 
-- Phantom type for @WKWebExtensionCommand@.
data WKWebExtensionCommand

instance IsObjCObject (Id WKWebExtensionCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionCommand"

class IsNSObject a => IsWKWebExtensionCommand a where
  toWKWebExtensionCommand :: a -> Id WKWebExtensionCommand

instance IsWKWebExtensionCommand (Id WKWebExtensionCommand) where
  toWKWebExtensionCommand = unsafeCastId

instance IsNSObject (Id WKWebExtensionCommand) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionContext ----------

-- | A ``WKWebExtensionContext`` object represents the runtime environment for a web extension.
--
-- This class provides methods for managing the extension's permissions, allowing it to inject content, run background logic, show popovers, and display other web-based UI to the user.
-- 
-- Phantom type for @WKWebExtensionContext@.
data WKWebExtensionContext

instance IsObjCObject (Id WKWebExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionContext"

class IsNSObject a => IsWKWebExtensionContext a where
  toWKWebExtensionContext :: a -> Id WKWebExtensionContext

instance IsWKWebExtensionContext (Id WKWebExtensionContext) where
  toWKWebExtensionContext = unsafeCastId

instance IsNSObject (Id WKWebExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionController ----------

-- | A ``WKWebExtensionController`` object manages a set of loaded extension contexts.
--
-- You can have one or more extension controller instances, allowing different parts of the app to use different sets of extensions. A controller is associated with ``WKWebView`` via the ``webExtensionController`` property on ``WKWebViewConfiguration``.
-- 
-- Phantom type for @WKWebExtensionController@.
data WKWebExtensionController

instance IsObjCObject (Id WKWebExtensionController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionController"

class IsNSObject a => IsWKWebExtensionController a where
  toWKWebExtensionController :: a -> Id WKWebExtensionController

instance IsWKWebExtensionController (Id WKWebExtensionController) where
  toWKWebExtensionController = unsafeCastId

instance IsNSObject (Id WKWebExtensionController) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionControllerConfiguration ----------

-- | A ``WKWebExtensionControllerConfiguration`` object with which to initialize a web extension controller.
--
-- Contains properties used to configure a ``WKWebExtensionController``.
-- 
-- Phantom type for @WKWebExtensionControllerConfiguration@.
data WKWebExtensionControllerConfiguration

instance IsObjCObject (Id WKWebExtensionControllerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionControllerConfiguration"

class IsNSObject a => IsWKWebExtensionControllerConfiguration a where
  toWKWebExtensionControllerConfiguration :: a -> Id WKWebExtensionControllerConfiguration

instance IsWKWebExtensionControllerConfiguration (Id WKWebExtensionControllerConfiguration) where
  toWKWebExtensionControllerConfiguration = unsafeCastId

instance IsNSObject (Id WKWebExtensionControllerConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionDataRecord ----------

-- | A ``WKWebExtensionDataRecord`` object represents a record of stored data for a specific web extension context.
--
-- Contains properties and methods to query the data types and sizes.
-- 
-- Phantom type for @WKWebExtensionDataRecord@.
data WKWebExtensionDataRecord

instance IsObjCObject (Id WKWebExtensionDataRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionDataRecord"

class IsNSObject a => IsWKWebExtensionDataRecord a where
  toWKWebExtensionDataRecord :: a -> Id WKWebExtensionDataRecord

instance IsWKWebExtensionDataRecord (Id WKWebExtensionDataRecord) where
  toWKWebExtensionDataRecord = unsafeCastId

instance IsNSObject (Id WKWebExtensionDataRecord) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionMatchPattern ----------

-- | A ``WKWebExtensionMatchPattern`` object represents a way to specify groups of URLs.
--
-- All match patterns are specified as strings. Apart from the special `<all_urls>` pattern, match patterns consist of three parts: scheme, host, and path.
-- 
-- Phantom type for @WKWebExtensionMatchPattern@.
data WKWebExtensionMatchPattern

instance IsObjCObject (Id WKWebExtensionMatchPattern) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionMatchPattern"

class IsNSObject a => IsWKWebExtensionMatchPattern a where
  toWKWebExtensionMatchPattern :: a -> Id WKWebExtensionMatchPattern

instance IsWKWebExtensionMatchPattern (Id WKWebExtensionMatchPattern) where
  toWKWebExtensionMatchPattern = unsafeCastId

instance IsNSObject (Id WKWebExtensionMatchPattern) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionMessagePort ----------

-- | A ``WKWebExtensionMessagePort`` object manages message-based communication with a web extension.
--
-- Contains properties and methods to handle message exchanges with a web extension.
-- 
-- Phantom type for @WKWebExtensionMessagePort@.
data WKWebExtensionMessagePort

instance IsObjCObject (Id WKWebExtensionMessagePort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionMessagePort"

class IsNSObject a => IsWKWebExtensionMessagePort a where
  toWKWebExtensionMessagePort :: a -> Id WKWebExtensionMessagePort

instance IsWKWebExtensionMessagePort (Id WKWebExtensionMessagePort) where
  toWKWebExtensionMessagePort = unsafeCastId

instance IsNSObject (Id WKWebExtensionMessagePort) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionTabConfiguration ----------

-- | A ``WKWebExtensionTabConfiguration`` object encapsulates configuration options for a tab in an extension.
--
-- This class holds various options that influence the behavior and initial state of a tab. The app retains the discretion to disregard any or all of these options, or even opt not to create a tab.
-- 
-- Phantom type for @WKWebExtensionTabConfiguration@.
data WKWebExtensionTabConfiguration

instance IsObjCObject (Id WKWebExtensionTabConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionTabConfiguration"

class IsNSObject a => IsWKWebExtensionTabConfiguration a where
  toWKWebExtensionTabConfiguration :: a -> Id WKWebExtensionTabConfiguration

instance IsWKWebExtensionTabConfiguration (Id WKWebExtensionTabConfiguration) where
  toWKWebExtensionTabConfiguration = unsafeCastId

instance IsNSObject (Id WKWebExtensionTabConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKWebExtensionWindowConfiguration ----------

-- | A ``WKWebExtensionWindowConfiguration`` object encapsulates configuration options for a window in an extension.
--
-- This class holds various options that influence the behavior and initial state of a window. The app retains the discretion to disregard any or all of these options, or even opt not to create a window.
-- 
-- Phantom type for @WKWebExtensionWindowConfiguration@.
data WKWebExtensionWindowConfiguration

instance IsObjCObject (Id WKWebExtensionWindowConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebExtensionWindowConfiguration"

class IsNSObject a => IsWKWebExtensionWindowConfiguration a where
  toWKWebExtensionWindowConfiguration :: a -> Id WKWebExtensionWindowConfiguration

instance IsWKWebExtensionWindowConfiguration (Id WKWebExtensionWindowConfiguration) where
  toWKWebExtensionWindowConfiguration = unsafeCastId

instance IsNSObject (Id WKWebExtensionWindowConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKWebViewConfiguration ----------

-- | A WKWebViewConfiguration object is a collection of properties with which to initialize a web view.
--
-- Contains properties used to configure a
--
-- WKWebView
--
-- .
-- 
-- Phantom type for @WKWebViewConfiguration@.
data WKWebViewConfiguration

instance IsObjCObject (Id WKWebViewConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebViewConfiguration"

class IsNSObject a => IsWKWebViewConfiguration a where
  toWKWebViewConfiguration :: a -> Id WKWebViewConfiguration

instance IsWKWebViewConfiguration (Id WKWebViewConfiguration) where
  toWKWebViewConfiguration = unsafeCastId

instance IsNSObject (Id WKWebViewConfiguration) where
  toNSObject = unsafeCastId

-- ---------- WKWebpagePreferences ----------

-- | A WKWebpagePreferences object is a collection of properties that determine the preferences to use when loading and rendering a page.
--
-- Contains properties used to determine webpage preferences.
-- 
-- Phantom type for @WKWebpagePreferences@.
data WKWebpagePreferences

instance IsObjCObject (Id WKWebpagePreferences) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebpagePreferences"

class IsNSObject a => IsWKWebpagePreferences a where
  toWKWebpagePreferences :: a -> Id WKWebpagePreferences

instance IsWKWebpagePreferences (Id WKWebpagePreferences) where
  toWKWebpagePreferences = unsafeCastId

instance IsNSObject (Id WKWebpagePreferences) where
  toNSObject = unsafeCastId

-- ---------- WKWebsiteDataRecord ----------

-- | A WKWebsiteDataRecord represents website data, grouped by domain name using the public suffix list.
-- 
-- Phantom type for @WKWebsiteDataRecord@.
data WKWebsiteDataRecord

instance IsObjCObject (Id WKWebsiteDataRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebsiteDataRecord"

class IsNSObject a => IsWKWebsiteDataRecord a where
  toWKWebsiteDataRecord :: a -> Id WKWebsiteDataRecord

instance IsWKWebsiteDataRecord (Id WKWebsiteDataRecord) where
  toWKWebsiteDataRecord = unsafeCastId

instance IsNSObject (Id WKWebsiteDataRecord) where
  toNSObject = unsafeCastId

-- ---------- WKWebsiteDataStore ----------

-- | A WKWebsiteDataStore represents various types of data that a website might make use of. This includes cookies, disk and memory caches, and persistent data such as WebSQL, IndexedDB databases, and local storage.
-- 
-- Phantom type for @WKWebsiteDataStore@.
data WKWebsiteDataStore

instance IsObjCObject (Id WKWebsiteDataStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebsiteDataStore"

class IsNSObject a => IsWKWebsiteDataStore a where
  toWKWebsiteDataStore :: a -> Id WKWebsiteDataStore

instance IsWKWebsiteDataStore (Id WKWebsiteDataStore) where
  toWKWebsiteDataStore = unsafeCastId

instance IsNSObject (Id WKWebsiteDataStore) where
  toNSObject = unsafeCastId

-- ---------- WKWindowFeatures ----------

-- | WKWindowFeatures specifies optional attributes for the containing window when a new WKWebView is requested.
-- 
-- Phantom type for @WKWindowFeatures@.
data WKWindowFeatures

instance IsObjCObject (Id WKWindowFeatures) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWindowFeatures"

class IsNSObject a => IsWKWindowFeatures a where
  toWKWindowFeatures :: a -> Id WKWindowFeatures

instance IsWKWindowFeatures (Id WKWindowFeatures) where
  toWKWindowFeatures = unsafeCastId

instance IsNSObject (Id WKWindowFeatures) where
  toNSObject = unsafeCastId

-- ---------- WebArchive ----------

-- | WebArchive
--
-- WebArchive represents a main resource as well as all the subresources and subframes associated with the main resource.    The main resource can be an entire web page, a portion of a web page, or some other kind of data such as an image.    This class can be used for saving standalone web pages, representing portions of a web page on the pasteboard, or any other    application where one class is needed to represent rich web content.
-- 
-- Phantom type for @WebArchive@.
data WebArchive

instance IsObjCObject (Id WebArchive) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebArchive"

class IsNSObject a => IsWebArchive a where
  toWebArchive :: a -> Id WebArchive

instance IsWebArchive (Id WebArchive) where
  toWebArchive = unsafeCastId

instance IsNSObject (Id WebArchive) where
  toNSObject = unsafeCastId

-- ---------- WebBackForwardList ----------

-- | WebBackForwardList
--
-- WebBackForwardList holds an ordered list of WebHistoryItems that comprises the back and    forward lists.
--
-- Note that the methods which modify instances of this class do not cause    navigation to happen in other layers of the stack;  they are only for maintaining this data    structure.
-- 
-- Phantom type for @WebBackForwardList@.
data WebBackForwardList

instance IsObjCObject (Id WebBackForwardList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebBackForwardList"

class IsNSObject a => IsWebBackForwardList a where
  toWebBackForwardList :: a -> Id WebBackForwardList

instance IsWebBackForwardList (Id WebBackForwardList) where
  toWebBackForwardList = unsafeCastId

instance IsNSObject (Id WebBackForwardList) where
  toNSObject = unsafeCastId

-- ---------- WebDataSource ----------

-- | WebDataSource
--
-- A WebDataSource represents the data associated with a web page.    A datasource has a WebDocumentRepresentation which holds an appropriate    representation of the data.  WebDataSources manage a hierarchy of WebFrames.    WebDataSources are typically related to a view by their containing WebFrame.
-- 
-- Phantom type for @WebDataSource@.
data WebDataSource

instance IsObjCObject (Id WebDataSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebDataSource"

class IsNSObject a => IsWebDataSource a where
  toWebDataSource :: a -> Id WebDataSource

instance IsWebDataSource (Id WebDataSource) where
  toWebDataSource = unsafeCastId

instance IsNSObject (Id WebDataSource) where
  toNSObject = unsafeCastId

-- ---------- WebFrame ----------

-- | WebFrame
--
-- Every web page is represented by at least one WebFrame.  A WebFrame    has a WebFrameView and a WebDataSource.
-- 
-- Phantom type for @WebFrame@.
data WebFrame

instance IsObjCObject (Id WebFrame) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebFrame"

class IsNSObject a => IsWebFrame a where
  toWebFrame :: a -> Id WebFrame

instance IsWebFrame (Id WebFrame) where
  toWebFrame = unsafeCastId

instance IsNSObject (Id WebFrame) where
  toNSObject = unsafeCastId

-- ---------- WebHistory ----------

-- | WebHistory
--
-- WebHistory is used to track pages that have been loaded    by WebKit.
-- 
-- Phantom type for @WebHistory@.
data WebHistory

instance IsObjCObject (Id WebHistory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebHistory"

class IsNSObject a => IsWebHistory a where
  toWebHistory :: a -> Id WebHistory

instance IsWebHistory (Id WebHistory) where
  toWebHistory = unsafeCastId

instance IsNSObject (Id WebHistory) where
  toNSObject = unsafeCastId

-- ---------- WebHistoryItem ----------

-- | WebHistoryItem
--
-- WebHistoryItems are created by WebKit to represent pages visited.    The WebBackForwardList and WebHistory classes both use WebHistoryItems to represent    pages visited.  With the exception of the displayTitle, the properties of     WebHistoryItems are set by WebKit.  WebHistoryItems are normally never created directly.
-- 
-- Phantom type for @WebHistoryItem@.
data WebHistoryItem

instance IsObjCObject (Id WebHistoryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebHistoryItem"

class IsNSObject a => IsWebHistoryItem a where
  toWebHistoryItem :: a -> Id WebHistoryItem

instance IsWebHistoryItem (Id WebHistoryItem) where
  toWebHistoryItem = unsafeCastId

instance IsNSObject (Id WebHistoryItem) where
  toNSObject = unsafeCastId

-- ---------- WebPreferences ----------

-- | WebPreferences
-- 
-- Phantom type for @WebPreferences@.
data WebPreferences

instance IsObjCObject (Id WebPreferences) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebPreferences"

class IsNSObject a => IsWebPreferences a where
  toWebPreferences :: a -> Id WebPreferences

instance IsWebPreferences (Id WebPreferences) where
  toWebPreferences = unsafeCastId

instance IsNSObject (Id WebPreferences) where
  toNSObject = unsafeCastId

-- ---------- WebResource ----------

-- | WebResource
--
-- A WebResource represents a fully downloaded URL.     It includes the data of the resource as well as the metadata associated with the resource.
-- 
-- Phantom type for @WebResource@.
data WebResource

instance IsObjCObject (Id WebResource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebResource"

class IsNSObject a => IsWebResource a where
  toWebResource :: a -> Id WebResource

instance IsWebResource (Id WebResource) where
  toWebResource = unsafeCastId

instance IsNSObject (Id WebResource) where
  toNSObject = unsafeCastId

-- ---------- WebScriptObject ----------

-- | WebScriptObject
--
-- WebScriptObjects are used to wrap script objects passed from    script environments to Objective-C. WebScriptObjects cannot be created    directly. In normal uses of WebKit, you gain access to the script    environment using the "windowScriptObject" method on WebView.
--
-- The following KVC methods are commonly used to access properties of the    WebScriptObject:
--
-- - (void)setValue:(id)value forKey:(NSString *)key        - (id)valueForKey:(NSString *)key
--
-- As it possible to remove attributes from web script objects, the following    additional method augments the basic KVC methods:
--
-- - (void)removeWebScriptKey:(NSString *)name;
--
-- Also, since the sparse array access allowed in script objects doesn't map well    to NSArray, the following methods can be used to access index based properties:
--
-- - (id)webScriptValueAtIndex:(unsigned)index;        - (void)setWebScriptValueAtIndex:(unsigned)index value:(id)value;
-- 
-- Phantom type for @WebScriptObject@.
data WebScriptObject

instance IsObjCObject (Id WebScriptObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebScriptObject"

class IsNSObject a => IsWebScriptObject a where
  toWebScriptObject :: a -> Id WebScriptObject

instance IsWebScriptObject (Id WebScriptObject) where
  toWebScriptObject = unsafeCastId

instance IsNSObject (Id WebScriptObject) where
  toNSObject = unsafeCastId

-- ---------- WebUndefined ----------

-- | WebUndefined
-- 
-- Phantom type for @WebUndefined@.
data WebUndefined

instance IsObjCObject (Id WebUndefined) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebUndefined"

class IsNSObject a => IsWebUndefined a where
  toWebUndefined :: a -> Id WebUndefined

instance IsWebUndefined (Id WebUndefined) where
  toWebUndefined = unsafeCastId

instance IsNSObject (Id WebUndefined) where
  toNSObject = unsafeCastId

-- ---------- WebDownload ----------

-- | WebDownload
--
-- A WebDownload works just like an NSURLDownload, with    one extra feature: if you do not implement the    authentication-related delegate methods, it will automatically    prompt for authentication using the standard WebKit authentication    panel, as either a sheet or window. It provides no extra methods,    but does have one additional delegate method.
-- 
-- Phantom type for @WebDownload@.
data WebDownload

instance IsObjCObject (Id WebDownload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebDownload"

class IsNSURLDownload a => IsWebDownload a where
  toWebDownload :: a -> Id WebDownload

instance IsWebDownload (Id WebDownload) where
  toWebDownload = unsafeCastId

instance IsNSObject (Id WebDownload) where
  toNSObject = unsafeCastId

instance IsNSURLDownload (Id WebDownload) where
  toNSURLDownload = unsafeCastId

-- ---------- DOMObject ----------

-- | Phantom type for @DOMObject@.
data DOMObject

instance IsObjCObject (Id DOMObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMObject"

class IsWebScriptObject a => IsDOMObject a where
  toDOMObject :: a -> Id DOMObject

instance IsDOMObject (Id DOMObject) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMObject) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMObject) where
  toWebScriptObject = unsafeCastId

-- ---------- WKWebView ----------

-- | Phantom type for @WKWebView@.
data WKWebView

instance IsObjCObject (Id WKWebView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WKWebView"

class IsNSView a => IsWKWebView a where
  toWKWebView :: a -> Id WKWebView

instance IsWKWebView (Id WKWebView) where
  toWKWebView = unsafeCastId

instance IsNSObject (Id WKWebView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id WKWebView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id WKWebView) where
  toNSView = unsafeCastId

-- ---------- WebFrameView ----------

-- | WebFrameView
-- 
-- Phantom type for @WebFrameView@.
data WebFrameView

instance IsObjCObject (Id WebFrameView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebFrameView"

class IsNSView a => IsWebFrameView a where
  toWebFrameView :: a -> Id WebFrameView

instance IsWebFrameView (Id WebFrameView) where
  toWebFrameView = unsafeCastId

instance IsNSObject (Id WebFrameView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id WebFrameView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id WebFrameView) where
  toNSView = unsafeCastId

-- ---------- WebView ----------

-- | Phantom type for @WebView@.
data WebView

instance IsObjCObject (Id WebView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "WebView"

class IsNSView a => IsWebView a where
  toWebView :: a -> Id WebView

instance IsWebView (Id WebView) where
  toWebView = unsafeCastId

instance IsNSObject (Id WebView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id WebView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id WebView) where
  toNSView = unsafeCastId

-- ---------- DOMAbstractView ----------

-- | Phantom type for @DOMAbstractView@.
data DOMAbstractView

instance IsObjCObject (Id DOMAbstractView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMAbstractView"

class IsDOMObject a => IsDOMAbstractView a where
  toDOMAbstractView :: a -> Id DOMAbstractView

instance IsDOMAbstractView (Id DOMAbstractView) where
  toDOMAbstractView = unsafeCastId

instance IsDOMObject (Id DOMAbstractView) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMAbstractView) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMAbstractView) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMBlob ----------

-- | Phantom type for @DOMBlob@.
data DOMBlob

instance IsObjCObject (Id DOMBlob) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMBlob"

class IsDOMObject a => IsDOMBlob a where
  toDOMBlob :: a -> Id DOMBlob

instance IsDOMBlob (Id DOMBlob) where
  toDOMBlob = unsafeCastId

instance IsDOMObject (Id DOMBlob) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMBlob) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMBlob) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSRule ----------

-- | Phantom type for @DOMCSSRule@.
data DOMCSSRule

instance IsObjCObject (Id DOMCSSRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSRule"

class IsDOMObject a => IsDOMCSSRule a where
  toDOMCSSRule :: a -> Id DOMCSSRule

instance IsDOMCSSRule (Id DOMCSSRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSRuleList ----------

-- | Phantom type for @DOMCSSRuleList@.
data DOMCSSRuleList

instance IsObjCObject (Id DOMCSSRuleList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSRuleList"

class IsDOMObject a => IsDOMCSSRuleList a where
  toDOMCSSRuleList :: a -> Id DOMCSSRuleList

instance IsDOMCSSRuleList (Id DOMCSSRuleList) where
  toDOMCSSRuleList = unsafeCastId

instance IsDOMObject (Id DOMCSSRuleList) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSRuleList) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSRuleList) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSStyleDeclaration ----------

-- | Phantom type for @DOMCSSStyleDeclaration@.
data DOMCSSStyleDeclaration

instance IsObjCObject (Id DOMCSSStyleDeclaration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSStyleDeclaration"

class IsDOMObject a => IsDOMCSSStyleDeclaration a where
  toDOMCSSStyleDeclaration :: a -> Id DOMCSSStyleDeclaration

instance IsDOMCSSStyleDeclaration (Id DOMCSSStyleDeclaration) where
  toDOMCSSStyleDeclaration = unsafeCastId

instance IsDOMObject (Id DOMCSSStyleDeclaration) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSStyleDeclaration) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSStyleDeclaration) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSValue ----------

-- | Phantom type for @DOMCSSValue@.
data DOMCSSValue

instance IsObjCObject (Id DOMCSSValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSValue"

class IsDOMObject a => IsDOMCSSValue a where
  toDOMCSSValue :: a -> Id DOMCSSValue

instance IsDOMCSSValue (Id DOMCSSValue) where
  toDOMCSSValue = unsafeCastId

instance IsDOMObject (Id DOMCSSValue) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSValue) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSValue) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCounter ----------

-- | Phantom type for @DOMCounter@.
data DOMCounter

instance IsObjCObject (Id DOMCounter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCounter"

class IsDOMObject a => IsDOMCounter a where
  toDOMCounter :: a -> Id DOMCounter

instance IsDOMCounter (Id DOMCounter) where
  toDOMCounter = unsafeCastId

instance IsDOMObject (Id DOMCounter) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCounter) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCounter) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMEvent ----------

-- | Phantom type for @DOMEvent@.
data DOMEvent

instance IsObjCObject (Id DOMEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMEvent"

class IsDOMObject a => IsDOMEvent a where
  toDOMEvent :: a -> Id DOMEvent

instance IsDOMEvent (Id DOMEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMEvent) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMFileList ----------

-- | Phantom type for @DOMFileList@.
data DOMFileList

instance IsObjCObject (Id DOMFileList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMFileList"

class IsDOMObject a => IsDOMFileList a where
  toDOMFileList :: a -> Id DOMFileList

instance IsDOMFileList (Id DOMFileList) where
  toDOMFileList = unsafeCastId

instance IsDOMObject (Id DOMFileList) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMFileList) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMFileList) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLCollection ----------

-- | Phantom type for @DOMHTMLCollection@.
data DOMHTMLCollection

instance IsObjCObject (Id DOMHTMLCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLCollection"

class IsDOMObject a => IsDOMHTMLCollection a where
  toDOMHTMLCollection :: a -> Id DOMHTMLCollection

instance IsDOMHTMLCollection (Id DOMHTMLCollection) where
  toDOMHTMLCollection = unsafeCastId

instance IsDOMObject (Id DOMHTMLCollection) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLCollection) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLCollection) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLOptionsCollection ----------

-- | Phantom type for @DOMHTMLOptionsCollection@.
data DOMHTMLOptionsCollection

instance IsObjCObject (Id DOMHTMLOptionsCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLOptionsCollection"

class IsDOMObject a => IsDOMHTMLOptionsCollection a where
  toDOMHTMLOptionsCollection :: a -> Id DOMHTMLOptionsCollection

instance IsDOMHTMLOptionsCollection (Id DOMHTMLOptionsCollection) where
  toDOMHTMLOptionsCollection = unsafeCastId

instance IsDOMObject (Id DOMHTMLOptionsCollection) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLOptionsCollection) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLOptionsCollection) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMImplementation ----------

-- | Phantom type for @DOMImplementation@.
data DOMImplementation

instance IsObjCObject (Id DOMImplementation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMImplementation"

class IsDOMObject a => IsDOMImplementation a where
  toDOMImplementation :: a -> Id DOMImplementation

instance IsDOMImplementation (Id DOMImplementation) where
  toDOMImplementation = unsafeCastId

instance IsDOMObject (Id DOMImplementation) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMImplementation) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMImplementation) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMMediaList ----------

-- | Phantom type for @DOMMediaList@.
data DOMMediaList

instance IsObjCObject (Id DOMMediaList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMMediaList"

class IsDOMObject a => IsDOMMediaList a where
  toDOMMediaList :: a -> Id DOMMediaList

instance IsDOMMediaList (Id DOMMediaList) where
  toDOMMediaList = unsafeCastId

instance IsDOMObject (Id DOMMediaList) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMMediaList) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMMediaList) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMNamedNodeMap ----------

-- | Phantom type for @DOMNamedNodeMap@.
data DOMNamedNodeMap

instance IsObjCObject (Id DOMNamedNodeMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMNamedNodeMap"

class IsDOMObject a => IsDOMNamedNodeMap a where
  toDOMNamedNodeMap :: a -> Id DOMNamedNodeMap

instance IsDOMNamedNodeMap (Id DOMNamedNodeMap) where
  toDOMNamedNodeMap = unsafeCastId

instance IsDOMObject (Id DOMNamedNodeMap) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMNamedNodeMap) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMNamedNodeMap) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMNode ----------

-- | Phantom type for @DOMNode@.
data DOMNode

instance IsObjCObject (Id DOMNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMNode"

class IsDOMObject a => IsDOMNode a where
  toDOMNode :: a -> Id DOMNode

instance IsDOMNode (Id DOMNode) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMNode) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMNode) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMNode) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMNodeIterator ----------

-- | Phantom type for @DOMNodeIterator@.
data DOMNodeIterator

instance IsObjCObject (Id DOMNodeIterator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMNodeIterator"

class IsDOMObject a => IsDOMNodeIterator a where
  toDOMNodeIterator :: a -> Id DOMNodeIterator

instance IsDOMNodeIterator (Id DOMNodeIterator) where
  toDOMNodeIterator = unsafeCastId

instance IsDOMObject (Id DOMNodeIterator) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMNodeIterator) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMNodeIterator) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMNodeList ----------

-- | Phantom type for @DOMNodeList@.
data DOMNodeList

instance IsObjCObject (Id DOMNodeList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMNodeList"

class IsDOMObject a => IsDOMNodeList a where
  toDOMNodeList :: a -> Id DOMNodeList

instance IsDOMNodeList (Id DOMNodeList) where
  toDOMNodeList = unsafeCastId

instance IsDOMObject (Id DOMNodeList) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMNodeList) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMNodeList) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMRGBColor ----------

-- | Phantom type for @DOMRGBColor@.
data DOMRGBColor

instance IsObjCObject (Id DOMRGBColor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMRGBColor"

class IsDOMObject a => IsDOMRGBColor a where
  toDOMRGBColor :: a -> Id DOMRGBColor

instance IsDOMRGBColor (Id DOMRGBColor) where
  toDOMRGBColor = unsafeCastId

instance IsDOMObject (Id DOMRGBColor) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMRGBColor) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMRGBColor) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMRange ----------

-- | Phantom type for @DOMRange@.
data DOMRange

instance IsObjCObject (Id DOMRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMRange"

class IsDOMObject a => IsDOMRange a where
  toDOMRange :: a -> Id DOMRange

instance IsDOMRange (Id DOMRange) where
  toDOMRange = unsafeCastId

instance IsDOMObject (Id DOMRange) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMRange) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMRange) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMRect ----------

-- | Phantom type for @DOMRect@.
data DOMRect

instance IsObjCObject (Id DOMRect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMRect"

class IsDOMObject a => IsDOMRect a where
  toDOMRect :: a -> Id DOMRect

instance IsDOMRect (Id DOMRect) where
  toDOMRect = unsafeCastId

instance IsDOMObject (Id DOMRect) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMRect) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMRect) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMStyleSheet ----------

-- | Phantom type for @DOMStyleSheet@.
data DOMStyleSheet

instance IsObjCObject (Id DOMStyleSheet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMStyleSheet"

class IsDOMObject a => IsDOMStyleSheet a where
  toDOMStyleSheet :: a -> Id DOMStyleSheet

instance IsDOMStyleSheet (Id DOMStyleSheet) where
  toDOMStyleSheet = unsafeCastId

instance IsDOMObject (Id DOMStyleSheet) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMStyleSheet) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMStyleSheet) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMStyleSheetList ----------

-- | Phantom type for @DOMStyleSheetList@.
data DOMStyleSheetList

instance IsObjCObject (Id DOMStyleSheetList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMStyleSheetList"

class IsDOMObject a => IsDOMStyleSheetList a where
  toDOMStyleSheetList :: a -> Id DOMStyleSheetList

instance IsDOMStyleSheetList (Id DOMStyleSheetList) where
  toDOMStyleSheetList = unsafeCastId

instance IsDOMObject (Id DOMStyleSheetList) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMStyleSheetList) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMStyleSheetList) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMTreeWalker ----------

-- | Phantom type for @DOMTreeWalker@.
data DOMTreeWalker

instance IsObjCObject (Id DOMTreeWalker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMTreeWalker"

class IsDOMObject a => IsDOMTreeWalker a where
  toDOMTreeWalker :: a -> Id DOMTreeWalker

instance IsDOMTreeWalker (Id DOMTreeWalker) where
  toDOMTreeWalker = unsafeCastId

instance IsDOMObject (Id DOMTreeWalker) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMTreeWalker) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMTreeWalker) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMXPathExpression ----------

-- | Phantom type for @DOMXPathExpression@.
data DOMXPathExpression

instance IsObjCObject (Id DOMXPathExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMXPathExpression"

class IsDOMObject a => IsDOMXPathExpression a where
  toDOMXPathExpression :: a -> Id DOMXPathExpression

instance IsDOMXPathExpression (Id DOMXPathExpression) where
  toDOMXPathExpression = unsafeCastId

instance IsDOMObject (Id DOMXPathExpression) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMXPathExpression) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMXPathExpression) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMXPathResult ----------

-- | Phantom type for @DOMXPathResult@.
data DOMXPathResult

instance IsObjCObject (Id DOMXPathResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMXPathResult"

class IsDOMObject a => IsDOMXPathResult a where
  toDOMXPathResult :: a -> Id DOMXPathResult

instance IsDOMXPathResult (Id DOMXPathResult) where
  toDOMXPathResult = unsafeCastId

instance IsDOMObject (Id DOMXPathResult) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMXPathResult) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMXPathResult) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMFile ----------

-- | Phantom type for @DOMFile@.
data DOMFile

instance IsObjCObject (Id DOMFile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMFile"

class IsDOMBlob a => IsDOMFile a where
  toDOMFile :: a -> Id DOMFile

instance IsDOMFile (Id DOMFile) where
  toDOMFile = unsafeCastId

instance IsDOMBlob (Id DOMFile) where
  toDOMBlob = unsafeCastId

instance IsDOMObject (Id DOMFile) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMFile) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMFile) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSCharsetRule ----------

-- | Phantom type for @DOMCSSCharsetRule@.
data DOMCSSCharsetRule

instance IsObjCObject (Id DOMCSSCharsetRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSCharsetRule"

class IsDOMCSSRule a => IsDOMCSSCharsetRule a where
  toDOMCSSCharsetRule :: a -> Id DOMCSSCharsetRule

instance IsDOMCSSCharsetRule (Id DOMCSSCharsetRule) where
  toDOMCSSCharsetRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSCharsetRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSCharsetRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSCharsetRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSCharsetRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSFontFaceRule ----------

-- | Phantom type for @DOMCSSFontFaceRule@.
data DOMCSSFontFaceRule

instance IsObjCObject (Id DOMCSSFontFaceRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSFontFaceRule"

class IsDOMCSSRule a => IsDOMCSSFontFaceRule a where
  toDOMCSSFontFaceRule :: a -> Id DOMCSSFontFaceRule

instance IsDOMCSSFontFaceRule (Id DOMCSSFontFaceRule) where
  toDOMCSSFontFaceRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSFontFaceRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSFontFaceRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSFontFaceRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSFontFaceRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSImportRule ----------

-- | Phantom type for @DOMCSSImportRule@.
data DOMCSSImportRule

instance IsObjCObject (Id DOMCSSImportRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSImportRule"

class IsDOMCSSRule a => IsDOMCSSImportRule a where
  toDOMCSSImportRule :: a -> Id DOMCSSImportRule

instance IsDOMCSSImportRule (Id DOMCSSImportRule) where
  toDOMCSSImportRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSImportRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSImportRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSImportRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSImportRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSMediaRule ----------

-- | Phantom type for @DOMCSSMediaRule@.
data DOMCSSMediaRule

instance IsObjCObject (Id DOMCSSMediaRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSMediaRule"

class IsDOMCSSRule a => IsDOMCSSMediaRule a where
  toDOMCSSMediaRule :: a -> Id DOMCSSMediaRule

instance IsDOMCSSMediaRule (Id DOMCSSMediaRule) where
  toDOMCSSMediaRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSMediaRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSMediaRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSMediaRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSMediaRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSPageRule ----------

-- | Phantom type for @DOMCSSPageRule@.
data DOMCSSPageRule

instance IsObjCObject (Id DOMCSSPageRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSPageRule"

class IsDOMCSSRule a => IsDOMCSSPageRule a where
  toDOMCSSPageRule :: a -> Id DOMCSSPageRule

instance IsDOMCSSPageRule (Id DOMCSSPageRule) where
  toDOMCSSPageRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSPageRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSPageRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSPageRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSPageRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSStyleRule ----------

-- | Phantom type for @DOMCSSStyleRule@.
data DOMCSSStyleRule

instance IsObjCObject (Id DOMCSSStyleRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSStyleRule"

class IsDOMCSSRule a => IsDOMCSSStyleRule a where
  toDOMCSSStyleRule :: a -> Id DOMCSSStyleRule

instance IsDOMCSSStyleRule (Id DOMCSSStyleRule) where
  toDOMCSSStyleRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSStyleRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSStyleRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSStyleRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSStyleRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSUnknownRule ----------

-- | Phantom type for @DOMCSSUnknownRule@.
data DOMCSSUnknownRule

instance IsObjCObject (Id DOMCSSUnknownRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSUnknownRule"

class IsDOMCSSRule a => IsDOMCSSUnknownRule a where
  toDOMCSSUnknownRule :: a -> Id DOMCSSUnknownRule

instance IsDOMCSSUnknownRule (Id DOMCSSUnknownRule) where
  toDOMCSSUnknownRule = unsafeCastId

instance IsDOMCSSRule (Id DOMCSSUnknownRule) where
  toDOMCSSRule = unsafeCastId

instance IsDOMObject (Id DOMCSSUnknownRule) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSUnknownRule) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSUnknownRule) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSPrimitiveValue ----------

-- | Phantom type for @DOMCSSPrimitiveValue@.
data DOMCSSPrimitiveValue

instance IsObjCObject (Id DOMCSSPrimitiveValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSPrimitiveValue"

class IsDOMCSSValue a => IsDOMCSSPrimitiveValue a where
  toDOMCSSPrimitiveValue :: a -> Id DOMCSSPrimitiveValue

instance IsDOMCSSPrimitiveValue (Id DOMCSSPrimitiveValue) where
  toDOMCSSPrimitiveValue = unsafeCastId

instance IsDOMCSSValue (Id DOMCSSPrimitiveValue) where
  toDOMCSSValue = unsafeCastId

instance IsDOMObject (Id DOMCSSPrimitiveValue) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSPrimitiveValue) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSPrimitiveValue) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSValueList ----------

-- | Phantom type for @DOMCSSValueList@.
data DOMCSSValueList

instance IsObjCObject (Id DOMCSSValueList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSValueList"

class IsDOMCSSValue a => IsDOMCSSValueList a where
  toDOMCSSValueList :: a -> Id DOMCSSValueList

instance IsDOMCSSValueList (Id DOMCSSValueList) where
  toDOMCSSValueList = unsafeCastId

instance IsDOMCSSValue (Id DOMCSSValueList) where
  toDOMCSSValue = unsafeCastId

instance IsDOMObject (Id DOMCSSValueList) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCSSValueList) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSValueList) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMMutationEvent ----------

-- | Phantom type for @DOMMutationEvent@.
data DOMMutationEvent

instance IsObjCObject (Id DOMMutationEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMMutationEvent"

class IsDOMEvent a => IsDOMMutationEvent a where
  toDOMMutationEvent :: a -> Id DOMMutationEvent

instance IsDOMMutationEvent (Id DOMMutationEvent) where
  toDOMMutationEvent = unsafeCastId

instance IsDOMEvent (Id DOMMutationEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMMutationEvent) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMMutationEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMMutationEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMOverflowEvent ----------

-- | Phantom type for @DOMOverflowEvent@.
data DOMOverflowEvent

instance IsObjCObject (Id DOMOverflowEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMOverflowEvent"

class IsDOMEvent a => IsDOMOverflowEvent a where
  toDOMOverflowEvent :: a -> Id DOMOverflowEvent

instance IsDOMOverflowEvent (Id DOMOverflowEvent) where
  toDOMOverflowEvent = unsafeCastId

instance IsDOMEvent (Id DOMOverflowEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMOverflowEvent) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMOverflowEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMOverflowEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMProgressEvent ----------

-- | Phantom type for @DOMProgressEvent@.
data DOMProgressEvent

instance IsObjCObject (Id DOMProgressEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMProgressEvent"

class IsDOMEvent a => IsDOMProgressEvent a where
  toDOMProgressEvent :: a -> Id DOMProgressEvent

instance IsDOMProgressEvent (Id DOMProgressEvent) where
  toDOMProgressEvent = unsafeCastId

instance IsDOMEvent (Id DOMProgressEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMProgressEvent) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMProgressEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMProgressEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMUIEvent ----------

-- | Phantom type for @DOMUIEvent@.
data DOMUIEvent

instance IsObjCObject (Id DOMUIEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMUIEvent"

class IsDOMEvent a => IsDOMUIEvent a where
  toDOMUIEvent :: a -> Id DOMUIEvent

instance IsDOMUIEvent (Id DOMUIEvent) where
  toDOMUIEvent = unsafeCastId

instance IsDOMEvent (Id DOMUIEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMUIEvent) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMUIEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMUIEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMAttr ----------

-- | Phantom type for @DOMAttr@.
data DOMAttr

instance IsObjCObject (Id DOMAttr) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMAttr"

class IsDOMNode a => IsDOMAttr a where
  toDOMAttr :: a -> Id DOMAttr

instance IsDOMAttr (Id DOMAttr) where
  toDOMAttr = unsafeCastId

instance IsDOMNode (Id DOMAttr) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMAttr) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMAttr) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMAttr) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCharacterData ----------

-- | Phantom type for @DOMCharacterData@.
data DOMCharacterData

instance IsObjCObject (Id DOMCharacterData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCharacterData"

class IsDOMNode a => IsDOMCharacterData a where
  toDOMCharacterData :: a -> Id DOMCharacterData

instance IsDOMCharacterData (Id DOMCharacterData) where
  toDOMCharacterData = unsafeCastId

instance IsDOMNode (Id DOMCharacterData) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMCharacterData) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMCharacterData) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCharacterData) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMDocument ----------

-- | Phantom type for @DOMDocument@.
data DOMDocument

instance IsObjCObject (Id DOMDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMDocument"

class IsDOMNode a => IsDOMDocument a where
  toDOMDocument :: a -> Id DOMDocument

instance IsDOMDocument (Id DOMDocument) where
  toDOMDocument = unsafeCastId

instance IsDOMNode (Id DOMDocument) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMDocument) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMDocument) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMDocument) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMDocumentFragment ----------

-- | Phantom type for @DOMDocumentFragment@.
data DOMDocumentFragment

instance IsObjCObject (Id DOMDocumentFragment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMDocumentFragment"

class IsDOMNode a => IsDOMDocumentFragment a where
  toDOMDocumentFragment :: a -> Id DOMDocumentFragment

instance IsDOMDocumentFragment (Id DOMDocumentFragment) where
  toDOMDocumentFragment = unsafeCastId

instance IsDOMNode (Id DOMDocumentFragment) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMDocumentFragment) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMDocumentFragment) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMDocumentFragment) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMDocumentType ----------

-- | Phantom type for @DOMDocumentType@.
data DOMDocumentType

instance IsObjCObject (Id DOMDocumentType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMDocumentType"

class IsDOMNode a => IsDOMDocumentType a where
  toDOMDocumentType :: a -> Id DOMDocumentType

instance IsDOMDocumentType (Id DOMDocumentType) where
  toDOMDocumentType = unsafeCastId

instance IsDOMNode (Id DOMDocumentType) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMDocumentType) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMDocumentType) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMDocumentType) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMElement ----------

-- | Phantom type for @DOMElement@.
data DOMElement

instance IsObjCObject (Id DOMElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMElement"

class IsDOMNode a => IsDOMElement a where
  toDOMElement :: a -> Id DOMElement

instance IsDOMElement (Id DOMElement) where
  toDOMElement = unsafeCastId

instance IsDOMNode (Id DOMElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMEntity ----------

-- | Phantom type for @DOMEntity@.
data DOMEntity

instance IsObjCObject (Id DOMEntity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMEntity"

class IsDOMNode a => IsDOMEntity a where
  toDOMEntity :: a -> Id DOMEntity

instance IsDOMEntity (Id DOMEntity) where
  toDOMEntity = unsafeCastId

instance IsDOMNode (Id DOMEntity) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMEntity) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMEntity) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMEntity) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMEntityReference ----------

-- | Phantom type for @DOMEntityReference@.
data DOMEntityReference

instance IsObjCObject (Id DOMEntityReference) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMEntityReference"

class IsDOMNode a => IsDOMEntityReference a where
  toDOMEntityReference :: a -> Id DOMEntityReference

instance IsDOMEntityReference (Id DOMEntityReference) where
  toDOMEntityReference = unsafeCastId

instance IsDOMNode (Id DOMEntityReference) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMEntityReference) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMEntityReference) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMEntityReference) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCSSStyleSheet ----------

-- | Phantom type for @DOMCSSStyleSheet@.
data DOMCSSStyleSheet

instance IsObjCObject (Id DOMCSSStyleSheet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCSSStyleSheet"

class IsDOMStyleSheet a => IsDOMCSSStyleSheet a where
  toDOMCSSStyleSheet :: a -> Id DOMCSSStyleSheet

instance IsDOMCSSStyleSheet (Id DOMCSSStyleSheet) where
  toDOMCSSStyleSheet = unsafeCastId

instance IsDOMObject (Id DOMCSSStyleSheet) where
  toDOMObject = unsafeCastId

instance IsDOMStyleSheet (Id DOMCSSStyleSheet) where
  toDOMStyleSheet = unsafeCastId

instance IsNSObject (Id DOMCSSStyleSheet) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCSSStyleSheet) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMKeyboardEvent ----------

-- | Phantom type for @DOMKeyboardEvent@.
data DOMKeyboardEvent

instance IsObjCObject (Id DOMKeyboardEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMKeyboardEvent"

class IsDOMUIEvent a => IsDOMKeyboardEvent a where
  toDOMKeyboardEvent :: a -> Id DOMKeyboardEvent

instance IsDOMKeyboardEvent (Id DOMKeyboardEvent) where
  toDOMKeyboardEvent = unsafeCastId

instance IsDOMEvent (Id DOMKeyboardEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMKeyboardEvent) where
  toDOMObject = unsafeCastId

instance IsDOMUIEvent (Id DOMKeyboardEvent) where
  toDOMUIEvent = unsafeCastId

instance IsNSObject (Id DOMKeyboardEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMKeyboardEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMMouseEvent ----------

-- | Phantom type for @DOMMouseEvent@.
data DOMMouseEvent

instance IsObjCObject (Id DOMMouseEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMMouseEvent"

class IsDOMUIEvent a => IsDOMMouseEvent a where
  toDOMMouseEvent :: a -> Id DOMMouseEvent

instance IsDOMMouseEvent (Id DOMMouseEvent) where
  toDOMMouseEvent = unsafeCastId

instance IsDOMEvent (Id DOMMouseEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMObject (Id DOMMouseEvent) where
  toDOMObject = unsafeCastId

instance IsDOMUIEvent (Id DOMMouseEvent) where
  toDOMUIEvent = unsafeCastId

instance IsNSObject (Id DOMMouseEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMMouseEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMComment ----------

-- | Phantom type for @DOMComment@.
data DOMComment

instance IsObjCObject (Id DOMComment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMComment"

class IsDOMCharacterData a => IsDOMComment a where
  toDOMComment :: a -> Id DOMComment

instance IsDOMComment (Id DOMComment) where
  toDOMComment = unsafeCastId

instance IsDOMCharacterData (Id DOMComment) where
  toDOMCharacterData = unsafeCastId

instance IsDOMNode (Id DOMComment) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMComment) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMComment) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMComment) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMProcessingInstruction ----------

-- | Phantom type for @DOMProcessingInstruction@.
data DOMProcessingInstruction

instance IsObjCObject (Id DOMProcessingInstruction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMProcessingInstruction"

class IsDOMCharacterData a => IsDOMProcessingInstruction a where
  toDOMProcessingInstruction :: a -> Id DOMProcessingInstruction

instance IsDOMProcessingInstruction (Id DOMProcessingInstruction) where
  toDOMProcessingInstruction = unsafeCastId

instance IsDOMCharacterData (Id DOMProcessingInstruction) where
  toDOMCharacterData = unsafeCastId

instance IsDOMNode (Id DOMProcessingInstruction) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMProcessingInstruction) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMProcessingInstruction) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMProcessingInstruction) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMText ----------

-- | Phantom type for @DOMText@.
data DOMText

instance IsObjCObject (Id DOMText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMText"

class IsDOMCharacterData a => IsDOMText a where
  toDOMText :: a -> Id DOMText

instance IsDOMText (Id DOMText) where
  toDOMText = unsafeCastId

instance IsDOMCharacterData (Id DOMText) where
  toDOMCharacterData = unsafeCastId

instance IsDOMNode (Id DOMText) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMText) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMText) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMText) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLDocument ----------

-- | Phantom type for @DOMHTMLDocument@.
data DOMHTMLDocument

instance IsObjCObject (Id DOMHTMLDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLDocument"

class IsDOMDocument a => IsDOMHTMLDocument a where
  toDOMHTMLDocument :: a -> Id DOMHTMLDocument

instance IsDOMHTMLDocument (Id DOMHTMLDocument) where
  toDOMHTMLDocument = unsafeCastId

instance IsDOMDocument (Id DOMHTMLDocument) where
  toDOMDocument = unsafeCastId

instance IsDOMNode (Id DOMHTMLDocument) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLDocument) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLDocument) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLDocument) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLElement ----------

-- | Phantom type for @DOMHTMLElement@.
data DOMHTMLElement

instance IsObjCObject (Id DOMHTMLElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLElement"

class IsDOMElement a => IsDOMHTMLElement a where
  toDOMHTMLElement :: a -> Id DOMHTMLElement

instance IsDOMHTMLElement (Id DOMHTMLElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLElement) where
  toDOMElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMWheelEvent ----------

-- | Phantom type for @DOMWheelEvent@.
data DOMWheelEvent

instance IsObjCObject (Id DOMWheelEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMWheelEvent"

class IsDOMMouseEvent a => IsDOMWheelEvent a where
  toDOMWheelEvent :: a -> Id DOMWheelEvent

instance IsDOMWheelEvent (Id DOMWheelEvent) where
  toDOMWheelEvent = unsafeCastId

instance IsDOMEvent (Id DOMWheelEvent) where
  toDOMEvent = unsafeCastId

instance IsDOMMouseEvent (Id DOMWheelEvent) where
  toDOMMouseEvent = unsafeCastId

instance IsDOMObject (Id DOMWheelEvent) where
  toDOMObject = unsafeCastId

instance IsDOMUIEvent (Id DOMWheelEvent) where
  toDOMUIEvent = unsafeCastId

instance IsNSObject (Id DOMWheelEvent) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMWheelEvent) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMCDATASection ----------

-- | Phantom type for @DOMCDATASection@.
data DOMCDATASection

instance IsObjCObject (Id DOMCDATASection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMCDATASection"

class IsDOMText a => IsDOMCDATASection a where
  toDOMCDATASection :: a -> Id DOMCDATASection

instance IsDOMCDATASection (Id DOMCDATASection) where
  toDOMCDATASection = unsafeCastId

instance IsDOMCharacterData (Id DOMCDATASection) where
  toDOMCharacterData = unsafeCastId

instance IsDOMNode (Id DOMCDATASection) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMCDATASection) where
  toDOMObject = unsafeCastId

instance IsDOMText (Id DOMCDATASection) where
  toDOMText = unsafeCastId

instance IsNSObject (Id DOMCDATASection) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMCDATASection) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLAnchorElement ----------

-- | Phantom type for @DOMHTMLAnchorElement@.
data DOMHTMLAnchorElement

instance IsObjCObject (Id DOMHTMLAnchorElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLAnchorElement"

class IsDOMHTMLElement a => IsDOMHTMLAnchorElement a where
  toDOMHTMLAnchorElement :: a -> Id DOMHTMLAnchorElement

instance IsDOMHTMLAnchorElement (Id DOMHTMLAnchorElement) where
  toDOMHTMLAnchorElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLAnchorElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLAnchorElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLAnchorElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLAnchorElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLAnchorElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLAnchorElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLAppletElement ----------

-- | Phantom type for @DOMHTMLAppletElement@.
data DOMHTMLAppletElement

instance IsObjCObject (Id DOMHTMLAppletElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLAppletElement"

class IsDOMHTMLElement a => IsDOMHTMLAppletElement a where
  toDOMHTMLAppletElement :: a -> Id DOMHTMLAppletElement

instance IsDOMHTMLAppletElement (Id DOMHTMLAppletElement) where
  toDOMHTMLAppletElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLAppletElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLAppletElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLAppletElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLAppletElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLAppletElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLAppletElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLAreaElement ----------

-- | Phantom type for @DOMHTMLAreaElement@.
data DOMHTMLAreaElement

instance IsObjCObject (Id DOMHTMLAreaElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLAreaElement"

class IsDOMHTMLElement a => IsDOMHTMLAreaElement a where
  toDOMHTMLAreaElement :: a -> Id DOMHTMLAreaElement

instance IsDOMHTMLAreaElement (Id DOMHTMLAreaElement) where
  toDOMHTMLAreaElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLAreaElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLAreaElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLAreaElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLAreaElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLAreaElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLAreaElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLBRElement ----------

-- | Phantom type for @DOMHTMLBRElement@.
data DOMHTMLBRElement

instance IsObjCObject (Id DOMHTMLBRElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLBRElement"

class IsDOMHTMLElement a => IsDOMHTMLBRElement a where
  toDOMHTMLBRElement :: a -> Id DOMHTMLBRElement

instance IsDOMHTMLBRElement (Id DOMHTMLBRElement) where
  toDOMHTMLBRElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLBRElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLBRElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLBRElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLBRElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLBRElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLBRElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLBaseElement ----------

-- | Phantom type for @DOMHTMLBaseElement@.
data DOMHTMLBaseElement

instance IsObjCObject (Id DOMHTMLBaseElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLBaseElement"

class IsDOMHTMLElement a => IsDOMHTMLBaseElement a where
  toDOMHTMLBaseElement :: a -> Id DOMHTMLBaseElement

instance IsDOMHTMLBaseElement (Id DOMHTMLBaseElement) where
  toDOMHTMLBaseElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLBaseElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLBaseElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLBaseElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLBaseElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLBaseElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLBaseElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLBaseFontElement ----------

-- | Phantom type for @DOMHTMLBaseFontElement@.
data DOMHTMLBaseFontElement

instance IsObjCObject (Id DOMHTMLBaseFontElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLBaseFontElement"

class IsDOMHTMLElement a => IsDOMHTMLBaseFontElement a where
  toDOMHTMLBaseFontElement :: a -> Id DOMHTMLBaseFontElement

instance IsDOMHTMLBaseFontElement (Id DOMHTMLBaseFontElement) where
  toDOMHTMLBaseFontElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLBaseFontElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLBaseFontElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLBaseFontElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLBaseFontElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLBaseFontElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLBaseFontElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLBodyElement ----------

-- | Phantom type for @DOMHTMLBodyElement@.
data DOMHTMLBodyElement

instance IsObjCObject (Id DOMHTMLBodyElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLBodyElement"

class IsDOMHTMLElement a => IsDOMHTMLBodyElement a where
  toDOMHTMLBodyElement :: a -> Id DOMHTMLBodyElement

instance IsDOMHTMLBodyElement (Id DOMHTMLBodyElement) where
  toDOMHTMLBodyElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLBodyElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLBodyElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLBodyElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLBodyElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLBodyElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLBodyElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLButtonElement ----------

-- | Phantom type for @DOMHTMLButtonElement@.
data DOMHTMLButtonElement

instance IsObjCObject (Id DOMHTMLButtonElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLButtonElement"

class IsDOMHTMLElement a => IsDOMHTMLButtonElement a where
  toDOMHTMLButtonElement :: a -> Id DOMHTMLButtonElement

instance IsDOMHTMLButtonElement (Id DOMHTMLButtonElement) where
  toDOMHTMLButtonElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLButtonElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLButtonElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLButtonElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLButtonElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLButtonElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLButtonElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLDListElement ----------

-- | Phantom type for @DOMHTMLDListElement@.
data DOMHTMLDListElement

instance IsObjCObject (Id DOMHTMLDListElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLDListElement"

class IsDOMHTMLElement a => IsDOMHTMLDListElement a where
  toDOMHTMLDListElement :: a -> Id DOMHTMLDListElement

instance IsDOMHTMLDListElement (Id DOMHTMLDListElement) where
  toDOMHTMLDListElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLDListElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLDListElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLDListElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLDListElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLDListElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLDListElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLDirectoryElement ----------

-- | Phantom type for @DOMHTMLDirectoryElement@.
data DOMHTMLDirectoryElement

instance IsObjCObject (Id DOMHTMLDirectoryElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLDirectoryElement"

class IsDOMHTMLElement a => IsDOMHTMLDirectoryElement a where
  toDOMHTMLDirectoryElement :: a -> Id DOMHTMLDirectoryElement

instance IsDOMHTMLDirectoryElement (Id DOMHTMLDirectoryElement) where
  toDOMHTMLDirectoryElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLDirectoryElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLDirectoryElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLDirectoryElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLDirectoryElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLDirectoryElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLDirectoryElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLDivElement ----------

-- | Phantom type for @DOMHTMLDivElement@.
data DOMHTMLDivElement

instance IsObjCObject (Id DOMHTMLDivElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLDivElement"

class IsDOMHTMLElement a => IsDOMHTMLDivElement a where
  toDOMHTMLDivElement :: a -> Id DOMHTMLDivElement

instance IsDOMHTMLDivElement (Id DOMHTMLDivElement) where
  toDOMHTMLDivElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLDivElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLDivElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLDivElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLDivElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLDivElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLDivElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLEmbedElement ----------

-- | Phantom type for @DOMHTMLEmbedElement@.
data DOMHTMLEmbedElement

instance IsObjCObject (Id DOMHTMLEmbedElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLEmbedElement"

class IsDOMHTMLElement a => IsDOMHTMLEmbedElement a where
  toDOMHTMLEmbedElement :: a -> Id DOMHTMLEmbedElement

instance IsDOMHTMLEmbedElement (Id DOMHTMLEmbedElement) where
  toDOMHTMLEmbedElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLEmbedElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLEmbedElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLEmbedElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLEmbedElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLEmbedElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLEmbedElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLFieldSetElement ----------

-- | Phantom type for @DOMHTMLFieldSetElement@.
data DOMHTMLFieldSetElement

instance IsObjCObject (Id DOMHTMLFieldSetElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLFieldSetElement"

class IsDOMHTMLElement a => IsDOMHTMLFieldSetElement a where
  toDOMHTMLFieldSetElement :: a -> Id DOMHTMLFieldSetElement

instance IsDOMHTMLFieldSetElement (Id DOMHTMLFieldSetElement) where
  toDOMHTMLFieldSetElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLFieldSetElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLFieldSetElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLFieldSetElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLFieldSetElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLFieldSetElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLFieldSetElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLFontElement ----------

-- | Phantom type for @DOMHTMLFontElement@.
data DOMHTMLFontElement

instance IsObjCObject (Id DOMHTMLFontElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLFontElement"

class IsDOMHTMLElement a => IsDOMHTMLFontElement a where
  toDOMHTMLFontElement :: a -> Id DOMHTMLFontElement

instance IsDOMHTMLFontElement (Id DOMHTMLFontElement) where
  toDOMHTMLFontElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLFontElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLFontElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLFontElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLFontElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLFontElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLFontElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLFormElement ----------

-- | Phantom type for @DOMHTMLFormElement@.
data DOMHTMLFormElement

instance IsObjCObject (Id DOMHTMLFormElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLFormElement"

class IsDOMHTMLElement a => IsDOMHTMLFormElement a where
  toDOMHTMLFormElement :: a -> Id DOMHTMLFormElement

instance IsDOMHTMLFormElement (Id DOMHTMLFormElement) where
  toDOMHTMLFormElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLFormElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLFormElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLFormElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLFormElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLFormElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLFormElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLFrameElement ----------

-- | Phantom type for @DOMHTMLFrameElement@.
data DOMHTMLFrameElement

instance IsObjCObject (Id DOMHTMLFrameElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLFrameElement"

class IsDOMHTMLElement a => IsDOMHTMLFrameElement a where
  toDOMHTMLFrameElement :: a -> Id DOMHTMLFrameElement

instance IsDOMHTMLFrameElement (Id DOMHTMLFrameElement) where
  toDOMHTMLFrameElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLFrameElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLFrameElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLFrameElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLFrameElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLFrameElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLFrameElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLFrameSetElement ----------

-- | Phantom type for @DOMHTMLFrameSetElement@.
data DOMHTMLFrameSetElement

instance IsObjCObject (Id DOMHTMLFrameSetElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLFrameSetElement"

class IsDOMHTMLElement a => IsDOMHTMLFrameSetElement a where
  toDOMHTMLFrameSetElement :: a -> Id DOMHTMLFrameSetElement

instance IsDOMHTMLFrameSetElement (Id DOMHTMLFrameSetElement) where
  toDOMHTMLFrameSetElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLFrameSetElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLFrameSetElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLFrameSetElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLFrameSetElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLFrameSetElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLFrameSetElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLHRElement ----------

-- | Phantom type for @DOMHTMLHRElement@.
data DOMHTMLHRElement

instance IsObjCObject (Id DOMHTMLHRElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLHRElement"

class IsDOMHTMLElement a => IsDOMHTMLHRElement a where
  toDOMHTMLHRElement :: a -> Id DOMHTMLHRElement

instance IsDOMHTMLHRElement (Id DOMHTMLHRElement) where
  toDOMHTMLHRElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLHRElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLHRElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLHRElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLHRElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLHRElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLHRElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLHeadElement ----------

-- | Phantom type for @DOMHTMLHeadElement@.
data DOMHTMLHeadElement

instance IsObjCObject (Id DOMHTMLHeadElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLHeadElement"

class IsDOMHTMLElement a => IsDOMHTMLHeadElement a where
  toDOMHTMLHeadElement :: a -> Id DOMHTMLHeadElement

instance IsDOMHTMLHeadElement (Id DOMHTMLHeadElement) where
  toDOMHTMLHeadElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLHeadElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLHeadElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLHeadElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLHeadElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLHeadElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLHeadElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLHeadingElement ----------

-- | Phantom type for @DOMHTMLHeadingElement@.
data DOMHTMLHeadingElement

instance IsObjCObject (Id DOMHTMLHeadingElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLHeadingElement"

class IsDOMHTMLElement a => IsDOMHTMLHeadingElement a where
  toDOMHTMLHeadingElement :: a -> Id DOMHTMLHeadingElement

instance IsDOMHTMLHeadingElement (Id DOMHTMLHeadingElement) where
  toDOMHTMLHeadingElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLHeadingElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLHeadingElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLHeadingElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLHeadingElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLHeadingElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLHeadingElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLHtmlElement ----------

-- | Phantom type for @DOMHTMLHtmlElement@.
data DOMHTMLHtmlElement

instance IsObjCObject (Id DOMHTMLHtmlElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLHtmlElement"

class IsDOMHTMLElement a => IsDOMHTMLHtmlElement a where
  toDOMHTMLHtmlElement :: a -> Id DOMHTMLHtmlElement

instance IsDOMHTMLHtmlElement (Id DOMHTMLHtmlElement) where
  toDOMHTMLHtmlElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLHtmlElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLHtmlElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLHtmlElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLHtmlElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLHtmlElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLHtmlElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLIFrameElement ----------

-- | Phantom type for @DOMHTMLIFrameElement@.
data DOMHTMLIFrameElement

instance IsObjCObject (Id DOMHTMLIFrameElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLIFrameElement"

class IsDOMHTMLElement a => IsDOMHTMLIFrameElement a where
  toDOMHTMLIFrameElement :: a -> Id DOMHTMLIFrameElement

instance IsDOMHTMLIFrameElement (Id DOMHTMLIFrameElement) where
  toDOMHTMLIFrameElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLIFrameElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLIFrameElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLIFrameElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLIFrameElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLIFrameElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLIFrameElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLImageElement ----------

-- | Phantom type for @DOMHTMLImageElement@.
data DOMHTMLImageElement

instance IsObjCObject (Id DOMHTMLImageElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLImageElement"

class IsDOMHTMLElement a => IsDOMHTMLImageElement a where
  toDOMHTMLImageElement :: a -> Id DOMHTMLImageElement

instance IsDOMHTMLImageElement (Id DOMHTMLImageElement) where
  toDOMHTMLImageElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLImageElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLImageElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLImageElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLImageElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLImageElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLImageElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLInputElement ----------

-- | Phantom type for @DOMHTMLInputElement@.
data DOMHTMLInputElement

instance IsObjCObject (Id DOMHTMLInputElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLInputElement"

class IsDOMHTMLElement a => IsDOMHTMLInputElement a where
  toDOMHTMLInputElement :: a -> Id DOMHTMLInputElement

instance IsDOMHTMLInputElement (Id DOMHTMLInputElement) where
  toDOMHTMLInputElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLInputElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLInputElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLInputElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLInputElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLInputElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLInputElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLLIElement ----------

-- | Phantom type for @DOMHTMLLIElement@.
data DOMHTMLLIElement

instance IsObjCObject (Id DOMHTMLLIElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLLIElement"

class IsDOMHTMLElement a => IsDOMHTMLLIElement a where
  toDOMHTMLLIElement :: a -> Id DOMHTMLLIElement

instance IsDOMHTMLLIElement (Id DOMHTMLLIElement) where
  toDOMHTMLLIElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLLIElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLLIElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLLIElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLLIElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLLIElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLLIElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLLabelElement ----------

-- | Phantom type for @DOMHTMLLabelElement@.
data DOMHTMLLabelElement

instance IsObjCObject (Id DOMHTMLLabelElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLLabelElement"

class IsDOMHTMLElement a => IsDOMHTMLLabelElement a where
  toDOMHTMLLabelElement :: a -> Id DOMHTMLLabelElement

instance IsDOMHTMLLabelElement (Id DOMHTMLLabelElement) where
  toDOMHTMLLabelElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLLabelElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLLabelElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLLabelElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLLabelElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLLabelElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLLabelElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLLegendElement ----------

-- | Phantom type for @DOMHTMLLegendElement@.
data DOMHTMLLegendElement

instance IsObjCObject (Id DOMHTMLLegendElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLLegendElement"

class IsDOMHTMLElement a => IsDOMHTMLLegendElement a where
  toDOMHTMLLegendElement :: a -> Id DOMHTMLLegendElement

instance IsDOMHTMLLegendElement (Id DOMHTMLLegendElement) where
  toDOMHTMLLegendElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLLegendElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLLegendElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLLegendElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLLegendElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLLegendElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLLegendElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLLinkElement ----------

-- | Phantom type for @DOMHTMLLinkElement@.
data DOMHTMLLinkElement

instance IsObjCObject (Id DOMHTMLLinkElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLLinkElement"

class IsDOMHTMLElement a => IsDOMHTMLLinkElement a where
  toDOMHTMLLinkElement :: a -> Id DOMHTMLLinkElement

instance IsDOMHTMLLinkElement (Id DOMHTMLLinkElement) where
  toDOMHTMLLinkElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLLinkElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLLinkElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLLinkElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLLinkElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLLinkElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLLinkElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLMapElement ----------

-- | Phantom type for @DOMHTMLMapElement@.
data DOMHTMLMapElement

instance IsObjCObject (Id DOMHTMLMapElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLMapElement"

class IsDOMHTMLElement a => IsDOMHTMLMapElement a where
  toDOMHTMLMapElement :: a -> Id DOMHTMLMapElement

instance IsDOMHTMLMapElement (Id DOMHTMLMapElement) where
  toDOMHTMLMapElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLMapElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLMapElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLMapElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLMapElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLMapElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLMapElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLMarqueeElement ----------

-- | Phantom type for @DOMHTMLMarqueeElement@.
data DOMHTMLMarqueeElement

instance IsObjCObject (Id DOMHTMLMarqueeElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLMarqueeElement"

class IsDOMHTMLElement a => IsDOMHTMLMarqueeElement a where
  toDOMHTMLMarqueeElement :: a -> Id DOMHTMLMarqueeElement

instance IsDOMHTMLMarqueeElement (Id DOMHTMLMarqueeElement) where
  toDOMHTMLMarqueeElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLMarqueeElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLMarqueeElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLMarqueeElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLMarqueeElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLMarqueeElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLMarqueeElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLMenuElement ----------

-- | Phantom type for @DOMHTMLMenuElement@.
data DOMHTMLMenuElement

instance IsObjCObject (Id DOMHTMLMenuElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLMenuElement"

class IsDOMHTMLElement a => IsDOMHTMLMenuElement a where
  toDOMHTMLMenuElement :: a -> Id DOMHTMLMenuElement

instance IsDOMHTMLMenuElement (Id DOMHTMLMenuElement) where
  toDOMHTMLMenuElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLMenuElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLMenuElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLMenuElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLMenuElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLMenuElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLMenuElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLMetaElement ----------

-- | Phantom type for @DOMHTMLMetaElement@.
data DOMHTMLMetaElement

instance IsObjCObject (Id DOMHTMLMetaElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLMetaElement"

class IsDOMHTMLElement a => IsDOMHTMLMetaElement a where
  toDOMHTMLMetaElement :: a -> Id DOMHTMLMetaElement

instance IsDOMHTMLMetaElement (Id DOMHTMLMetaElement) where
  toDOMHTMLMetaElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLMetaElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLMetaElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLMetaElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLMetaElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLMetaElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLMetaElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLModElement ----------

-- | Phantom type for @DOMHTMLModElement@.
data DOMHTMLModElement

instance IsObjCObject (Id DOMHTMLModElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLModElement"

class IsDOMHTMLElement a => IsDOMHTMLModElement a where
  toDOMHTMLModElement :: a -> Id DOMHTMLModElement

instance IsDOMHTMLModElement (Id DOMHTMLModElement) where
  toDOMHTMLModElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLModElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLModElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLModElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLModElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLModElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLModElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLOListElement ----------

-- | Phantom type for @DOMHTMLOListElement@.
data DOMHTMLOListElement

instance IsObjCObject (Id DOMHTMLOListElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLOListElement"

class IsDOMHTMLElement a => IsDOMHTMLOListElement a where
  toDOMHTMLOListElement :: a -> Id DOMHTMLOListElement

instance IsDOMHTMLOListElement (Id DOMHTMLOListElement) where
  toDOMHTMLOListElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLOListElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLOListElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLOListElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLOListElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLOListElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLOListElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLObjectElement ----------

-- | Phantom type for @DOMHTMLObjectElement@.
data DOMHTMLObjectElement

instance IsObjCObject (Id DOMHTMLObjectElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLObjectElement"

class IsDOMHTMLElement a => IsDOMHTMLObjectElement a where
  toDOMHTMLObjectElement :: a -> Id DOMHTMLObjectElement

instance IsDOMHTMLObjectElement (Id DOMHTMLObjectElement) where
  toDOMHTMLObjectElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLObjectElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLObjectElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLObjectElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLObjectElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLObjectElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLObjectElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLOptGroupElement ----------

-- | Phantom type for @DOMHTMLOptGroupElement@.
data DOMHTMLOptGroupElement

instance IsObjCObject (Id DOMHTMLOptGroupElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLOptGroupElement"

class IsDOMHTMLElement a => IsDOMHTMLOptGroupElement a where
  toDOMHTMLOptGroupElement :: a -> Id DOMHTMLOptGroupElement

instance IsDOMHTMLOptGroupElement (Id DOMHTMLOptGroupElement) where
  toDOMHTMLOptGroupElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLOptGroupElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLOptGroupElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLOptGroupElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLOptGroupElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLOptGroupElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLOptGroupElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLOptionElement ----------

-- | Phantom type for @DOMHTMLOptionElement@.
data DOMHTMLOptionElement

instance IsObjCObject (Id DOMHTMLOptionElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLOptionElement"

class IsDOMHTMLElement a => IsDOMHTMLOptionElement a where
  toDOMHTMLOptionElement :: a -> Id DOMHTMLOptionElement

instance IsDOMHTMLOptionElement (Id DOMHTMLOptionElement) where
  toDOMHTMLOptionElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLOptionElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLOptionElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLOptionElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLOptionElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLOptionElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLOptionElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLParagraphElement ----------

-- | Phantom type for @DOMHTMLParagraphElement@.
data DOMHTMLParagraphElement

instance IsObjCObject (Id DOMHTMLParagraphElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLParagraphElement"

class IsDOMHTMLElement a => IsDOMHTMLParagraphElement a where
  toDOMHTMLParagraphElement :: a -> Id DOMHTMLParagraphElement

instance IsDOMHTMLParagraphElement (Id DOMHTMLParagraphElement) where
  toDOMHTMLParagraphElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLParagraphElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLParagraphElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLParagraphElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLParagraphElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLParagraphElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLParagraphElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLParamElement ----------

-- | Phantom type for @DOMHTMLParamElement@.
data DOMHTMLParamElement

instance IsObjCObject (Id DOMHTMLParamElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLParamElement"

class IsDOMHTMLElement a => IsDOMHTMLParamElement a where
  toDOMHTMLParamElement :: a -> Id DOMHTMLParamElement

instance IsDOMHTMLParamElement (Id DOMHTMLParamElement) where
  toDOMHTMLParamElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLParamElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLParamElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLParamElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLParamElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLParamElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLParamElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLPreElement ----------

-- | Phantom type for @DOMHTMLPreElement@.
data DOMHTMLPreElement

instance IsObjCObject (Id DOMHTMLPreElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLPreElement"

class IsDOMHTMLElement a => IsDOMHTMLPreElement a where
  toDOMHTMLPreElement :: a -> Id DOMHTMLPreElement

instance IsDOMHTMLPreElement (Id DOMHTMLPreElement) where
  toDOMHTMLPreElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLPreElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLPreElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLPreElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLPreElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLPreElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLPreElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLQuoteElement ----------

-- | Phantom type for @DOMHTMLQuoteElement@.
data DOMHTMLQuoteElement

instance IsObjCObject (Id DOMHTMLQuoteElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLQuoteElement"

class IsDOMHTMLElement a => IsDOMHTMLQuoteElement a where
  toDOMHTMLQuoteElement :: a -> Id DOMHTMLQuoteElement

instance IsDOMHTMLQuoteElement (Id DOMHTMLQuoteElement) where
  toDOMHTMLQuoteElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLQuoteElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLQuoteElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLQuoteElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLQuoteElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLQuoteElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLQuoteElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLScriptElement ----------

-- | Phantom type for @DOMHTMLScriptElement@.
data DOMHTMLScriptElement

instance IsObjCObject (Id DOMHTMLScriptElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLScriptElement"

class IsDOMHTMLElement a => IsDOMHTMLScriptElement a where
  toDOMHTMLScriptElement :: a -> Id DOMHTMLScriptElement

instance IsDOMHTMLScriptElement (Id DOMHTMLScriptElement) where
  toDOMHTMLScriptElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLScriptElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLScriptElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLScriptElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLScriptElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLScriptElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLScriptElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLSelectElement ----------

-- | Phantom type for @DOMHTMLSelectElement@.
data DOMHTMLSelectElement

instance IsObjCObject (Id DOMHTMLSelectElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLSelectElement"

class IsDOMHTMLElement a => IsDOMHTMLSelectElement a where
  toDOMHTMLSelectElement :: a -> Id DOMHTMLSelectElement

instance IsDOMHTMLSelectElement (Id DOMHTMLSelectElement) where
  toDOMHTMLSelectElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLSelectElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLSelectElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLSelectElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLSelectElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLSelectElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLSelectElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLStyleElement ----------

-- | Phantom type for @DOMHTMLStyleElement@.
data DOMHTMLStyleElement

instance IsObjCObject (Id DOMHTMLStyleElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLStyleElement"

class IsDOMHTMLElement a => IsDOMHTMLStyleElement a where
  toDOMHTMLStyleElement :: a -> Id DOMHTMLStyleElement

instance IsDOMHTMLStyleElement (Id DOMHTMLStyleElement) where
  toDOMHTMLStyleElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLStyleElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLStyleElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLStyleElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLStyleElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLStyleElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLStyleElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTableCaptionElement ----------

-- | Phantom type for @DOMHTMLTableCaptionElement@.
data DOMHTMLTableCaptionElement

instance IsObjCObject (Id DOMHTMLTableCaptionElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTableCaptionElement"

class IsDOMHTMLElement a => IsDOMHTMLTableCaptionElement a where
  toDOMHTMLTableCaptionElement :: a -> Id DOMHTMLTableCaptionElement

instance IsDOMHTMLTableCaptionElement (Id DOMHTMLTableCaptionElement) where
  toDOMHTMLTableCaptionElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTableCaptionElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTableCaptionElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTableCaptionElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTableCaptionElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTableCaptionElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTableCaptionElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTableCellElement ----------

-- | Phantom type for @DOMHTMLTableCellElement@.
data DOMHTMLTableCellElement

instance IsObjCObject (Id DOMHTMLTableCellElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTableCellElement"

class IsDOMHTMLElement a => IsDOMHTMLTableCellElement a where
  toDOMHTMLTableCellElement :: a -> Id DOMHTMLTableCellElement

instance IsDOMHTMLTableCellElement (Id DOMHTMLTableCellElement) where
  toDOMHTMLTableCellElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTableCellElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTableCellElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTableCellElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTableCellElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTableCellElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTableCellElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTableColElement ----------

-- | Phantom type for @DOMHTMLTableColElement@.
data DOMHTMLTableColElement

instance IsObjCObject (Id DOMHTMLTableColElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTableColElement"

class IsDOMHTMLElement a => IsDOMHTMLTableColElement a where
  toDOMHTMLTableColElement :: a -> Id DOMHTMLTableColElement

instance IsDOMHTMLTableColElement (Id DOMHTMLTableColElement) where
  toDOMHTMLTableColElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTableColElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTableColElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTableColElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTableColElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTableColElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTableColElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTableElement ----------

-- | Phantom type for @DOMHTMLTableElement@.
data DOMHTMLTableElement

instance IsObjCObject (Id DOMHTMLTableElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTableElement"

class IsDOMHTMLElement a => IsDOMHTMLTableElement a where
  toDOMHTMLTableElement :: a -> Id DOMHTMLTableElement

instance IsDOMHTMLTableElement (Id DOMHTMLTableElement) where
  toDOMHTMLTableElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTableElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTableElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTableElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTableElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTableElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTableElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTableRowElement ----------

-- | Phantom type for @DOMHTMLTableRowElement@.
data DOMHTMLTableRowElement

instance IsObjCObject (Id DOMHTMLTableRowElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTableRowElement"

class IsDOMHTMLElement a => IsDOMHTMLTableRowElement a where
  toDOMHTMLTableRowElement :: a -> Id DOMHTMLTableRowElement

instance IsDOMHTMLTableRowElement (Id DOMHTMLTableRowElement) where
  toDOMHTMLTableRowElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTableRowElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTableRowElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTableRowElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTableRowElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTableRowElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTableRowElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTableSectionElement ----------

-- | Phantom type for @DOMHTMLTableSectionElement@.
data DOMHTMLTableSectionElement

instance IsObjCObject (Id DOMHTMLTableSectionElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTableSectionElement"

class IsDOMHTMLElement a => IsDOMHTMLTableSectionElement a where
  toDOMHTMLTableSectionElement :: a -> Id DOMHTMLTableSectionElement

instance IsDOMHTMLTableSectionElement (Id DOMHTMLTableSectionElement) where
  toDOMHTMLTableSectionElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTableSectionElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTableSectionElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTableSectionElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTableSectionElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTableSectionElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTableSectionElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTextAreaElement ----------

-- | Phantom type for @DOMHTMLTextAreaElement@.
data DOMHTMLTextAreaElement

instance IsObjCObject (Id DOMHTMLTextAreaElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTextAreaElement"

class IsDOMHTMLElement a => IsDOMHTMLTextAreaElement a where
  toDOMHTMLTextAreaElement :: a -> Id DOMHTMLTextAreaElement

instance IsDOMHTMLTextAreaElement (Id DOMHTMLTextAreaElement) where
  toDOMHTMLTextAreaElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTextAreaElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTextAreaElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTextAreaElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTextAreaElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTextAreaElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTextAreaElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLTitleElement ----------

-- | Phantom type for @DOMHTMLTitleElement@.
data DOMHTMLTitleElement

instance IsObjCObject (Id DOMHTMLTitleElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLTitleElement"

class IsDOMHTMLElement a => IsDOMHTMLTitleElement a where
  toDOMHTMLTitleElement :: a -> Id DOMHTMLTitleElement

instance IsDOMHTMLTitleElement (Id DOMHTMLTitleElement) where
  toDOMHTMLTitleElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLTitleElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLTitleElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLTitleElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLTitleElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLTitleElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLTitleElement) where
  toWebScriptObject = unsafeCastId

-- ---------- DOMHTMLUListElement ----------

-- | Phantom type for @DOMHTMLUListElement@.
data DOMHTMLUListElement

instance IsObjCObject (Id DOMHTMLUListElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DOMHTMLUListElement"

class IsDOMHTMLElement a => IsDOMHTMLUListElement a where
  toDOMHTMLUListElement :: a -> Id DOMHTMLUListElement

instance IsDOMHTMLUListElement (Id DOMHTMLUListElement) where
  toDOMHTMLUListElement = unsafeCastId

instance IsDOMElement (Id DOMHTMLUListElement) where
  toDOMElement = unsafeCastId

instance IsDOMHTMLElement (Id DOMHTMLUListElement) where
  toDOMHTMLElement = unsafeCastId

instance IsDOMNode (Id DOMHTMLUListElement) where
  toDOMNode = unsafeCastId

instance IsDOMObject (Id DOMHTMLUListElement) where
  toDOMObject = unsafeCastId

instance IsNSObject (Id DOMHTMLUListElement) where
  toNSObject = unsafeCastId

instance IsWebScriptObject (Id DOMHTMLUListElement) where
  toWebScriptObject = unsafeCastId
