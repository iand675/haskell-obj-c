{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKUserContentController object provides a way for JavaScript to post messages to a web view. The user content controller associated with a web view is specified by its web view configuration.
--
-- Generated bindings for @WKUserContentController@.
module ObjC.WebKit.WKUserContentController
  ( WKUserContentController
  , IsWKUserContentController(..)
  , addUserScript
  , removeAllUserScripts
  , addScriptMessageHandler_contentWorld_name
  , addScriptMessageHandlerWithReply_contentWorld_name
  , addScriptMessageHandler_name
  , removeScriptMessageHandlerForName_contentWorld
  , removeScriptMessageHandlerForName
  , removeAllScriptMessageHandlersFromContentWorld
  , removeAllScriptMessageHandlers
  , addContentRuleList
  , removeContentRuleList
  , removeAllContentRuleLists
  , userScripts
  , addUserScriptSelector
  , removeAllUserScriptsSelector
  , addScriptMessageHandler_contentWorld_nameSelector
  , addScriptMessageHandlerWithReply_contentWorld_nameSelector
  , addScriptMessageHandler_nameSelector
  , removeScriptMessageHandlerForName_contentWorldSelector
  , removeScriptMessageHandlerForNameSelector
  , removeAllScriptMessageHandlersFromContentWorldSelector
  , removeAllScriptMessageHandlersSelector
  , addContentRuleListSelector
  , removeContentRuleListSelector
  , removeAllContentRuleListsSelector
  , userScriptsSelector


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
import ObjC.Foundation.Internal.Classes

-- | Adds a user script.
--
-- @userScript@ — The user script to add.
--
-- ObjC selector: @- addUserScript:@
addUserScript :: (IsWKUserContentController wkUserContentController, IsWKUserScript userScript) => wkUserContentController -> userScript -> IO ()
addUserScript wkUserContentController  userScript =
withObjCPtr userScript $ \raw_userScript ->
    sendMsg wkUserContentController (mkSelector "addUserScript:") retVoid [argPtr (castPtr raw_userScript :: Ptr ())]

-- | Removes all associated user scripts.
--
-- ObjC selector: @- removeAllUserScripts@
removeAllUserScripts :: IsWKUserContentController wkUserContentController => wkUserContentController -> IO ()
removeAllUserScripts wkUserContentController  =
  sendMsg wkUserContentController (mkSelector "removeAllUserScripts") retVoid []

-- | Adds a script message handler.
--
-- @scriptMessageHandler@ — The script message handler to add.
--
-- @contentWorld@ — The WKContentWorld in which to add the script message handler.
--
-- @name@ — The name of the message handler.
--
-- Adding a script message handler adds a function window.webkit.messageHandlers.<name>.postMessage(<messageBody>) to all frames, available in the given WKContentWorld.
--
-- The name argument must be a non-empty string.
--
-- Each WKContentWorld can have any number of script message handlers, but only one per unique name.
--
-- Once any script message handler has been added to a WKContentWorld for a given name, it is an error to add another script message handler to that WKContentWorld for that same name without first removing the previous script message handler.
--
-- The above restriction applies to any type of script message handler - WKScriptMessageHandler and WKScriptMessageHandlerWithReply objects will conflict with each other if you try to add them to the same WKContentWorld with the same name.
--
-- ObjC selector: @- addScriptMessageHandler:contentWorld:name:@
addScriptMessageHandler_contentWorld_name :: (IsWKUserContentController wkUserContentController, IsWKContentWorld world, IsNSString name) => wkUserContentController -> RawId -> world -> name -> IO ()
addScriptMessageHandler_contentWorld_name wkUserContentController  scriptMessageHandler world name =
withObjCPtr world $ \raw_world ->
  withObjCPtr name $ \raw_name ->
      sendMsg wkUserContentController (mkSelector "addScriptMessageHandler:contentWorld:name:") retVoid [argPtr (castPtr (unRawId scriptMessageHandler) :: Ptr ()), argPtr (castPtr raw_world :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | Adds a script message handler.
--
-- @scriptMessageHandlerWithReply@ — The script message handler to add.
--
-- @contentWorld@ — The WKContentWorld in which to add the script message handler.
--
-- @name@ — The name of the message handler.
--
-- Adding a script message handler adds a function window.webkit.messageHandlers.<name>.postMessage(<messageBody>) to all frames, available in the given WKContentWorld.
--
-- The name argument must be a non-empty string.
--
-- Each WKContentWorld can have any number of script message handlers, but only one per unique name.
--
-- Once any script message handler has been added to a WKContentWorld for a given name, it is an error to add another script message handler to that WKContentWorld for that same name without first removing the previous script message handler.
--
-- The above restriction applies to any type of script message handler - WKScriptMessageHandlerWithReply and WKScriptMessageHandler objects will conflict with each other if you try to add them to the same WKContentWorld with the same name.
--
-- Refer to the WKScriptMessageHandlerWithReply documentation for examples of how it is more flexible than WKScriptMessageHandler.
--
-- ObjC selector: @- addScriptMessageHandlerWithReply:contentWorld:name:@
addScriptMessageHandlerWithReply_contentWorld_name :: (IsWKUserContentController wkUserContentController, IsWKContentWorld contentWorld, IsNSString name) => wkUserContentController -> RawId -> contentWorld -> name -> IO ()
addScriptMessageHandlerWithReply_contentWorld_name wkUserContentController  scriptMessageHandlerWithReply contentWorld name =
withObjCPtr contentWorld $ \raw_contentWorld ->
  withObjCPtr name $ \raw_name ->
      sendMsg wkUserContentController (mkSelector "addScriptMessageHandlerWithReply:contentWorld:name:") retVoid [argPtr (castPtr (unRawId scriptMessageHandlerWithReply) :: Ptr ()), argPtr (castPtr raw_contentWorld :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | Adds a script message handler to the main world used by page content itself.
--
-- @scriptMessageHandler@ — The script message handler to add.
--
-- @name@ — The name of the message handler.
--
-- Calling this method is equivalent to calling addScriptMessageHandler:contentWorld:name: with [WKContentWorld pageWorld] as the contentWorld argument.
--
-- ObjC selector: @- addScriptMessageHandler:name:@
addScriptMessageHandler_name :: (IsWKUserContentController wkUserContentController, IsNSString name) => wkUserContentController -> RawId -> name -> IO ()
addScriptMessageHandler_name wkUserContentController  scriptMessageHandler name =
withObjCPtr name $ \raw_name ->
    sendMsg wkUserContentController (mkSelector "addScriptMessageHandler:name:") retVoid [argPtr (castPtr (unRawId scriptMessageHandler) :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | Removes a script message handler.
--
-- @name@ — The name of the message handler to remove.
--
-- @contentWorld@ — The WKContentWorld from which to remove the script message handler.
--
-- ObjC selector: @- removeScriptMessageHandlerForName:contentWorld:@
removeScriptMessageHandlerForName_contentWorld :: (IsWKUserContentController wkUserContentController, IsNSString name, IsWKContentWorld contentWorld) => wkUserContentController -> name -> contentWorld -> IO ()
removeScriptMessageHandlerForName_contentWorld wkUserContentController  name contentWorld =
withObjCPtr name $ \raw_name ->
  withObjCPtr contentWorld $ \raw_contentWorld ->
      sendMsg wkUserContentController (mkSelector "removeScriptMessageHandlerForName:contentWorld:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_contentWorld :: Ptr ())]

-- | Removes a script message handler.
--
-- @name@ — The name of the message handler to remove.
--
-- Calling this method is equivalent to calling removeScriptMessageHandlerForName:contentWorld:  with [WKContentWorld pageWorld] as the contentWorld argument.
--
-- ObjC selector: @- removeScriptMessageHandlerForName:@
removeScriptMessageHandlerForName :: (IsWKUserContentController wkUserContentController, IsNSString name) => wkUserContentController -> name -> IO ()
removeScriptMessageHandlerForName wkUserContentController  name =
withObjCPtr name $ \raw_name ->
    sendMsg wkUserContentController (mkSelector "removeScriptMessageHandlerForName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | Removes all script message handlers from a given WKContentWorld.
--
-- @contentWorld@ — The WKContentWorld from which to remove all script message handlers.
--
-- ObjC selector: @- removeAllScriptMessageHandlersFromContentWorld:@
removeAllScriptMessageHandlersFromContentWorld :: (IsWKUserContentController wkUserContentController, IsWKContentWorld contentWorld) => wkUserContentController -> contentWorld -> IO ()
removeAllScriptMessageHandlersFromContentWorld wkUserContentController  contentWorld =
withObjCPtr contentWorld $ \raw_contentWorld ->
    sendMsg wkUserContentController (mkSelector "removeAllScriptMessageHandlersFromContentWorld:") retVoid [argPtr (castPtr raw_contentWorld :: Ptr ())]

-- | Removes all associated script message handlers.
--
-- ObjC selector: @- removeAllScriptMessageHandlers@
removeAllScriptMessageHandlers :: IsWKUserContentController wkUserContentController => wkUserContentController -> IO ()
removeAllScriptMessageHandlers wkUserContentController  =
  sendMsg wkUserContentController (mkSelector "removeAllScriptMessageHandlers") retVoid []

-- | Adds a content rule list.
--
-- @contentRuleList@ — The content rule list to add.
--
-- ObjC selector: @- addContentRuleList:@
addContentRuleList :: (IsWKUserContentController wkUserContentController, IsWKContentRuleList contentRuleList) => wkUserContentController -> contentRuleList -> IO ()
addContentRuleList wkUserContentController  contentRuleList =
withObjCPtr contentRuleList $ \raw_contentRuleList ->
    sendMsg wkUserContentController (mkSelector "addContentRuleList:") retVoid [argPtr (castPtr raw_contentRuleList :: Ptr ())]

-- | Removes a content rule list.
--
-- @contentRuleList@ — The content rule list to remove.
--
-- ObjC selector: @- removeContentRuleList:@
removeContentRuleList :: (IsWKUserContentController wkUserContentController, IsWKContentRuleList contentRuleList) => wkUserContentController -> contentRuleList -> IO ()
removeContentRuleList wkUserContentController  contentRuleList =
withObjCPtr contentRuleList $ \raw_contentRuleList ->
    sendMsg wkUserContentController (mkSelector "removeContentRuleList:") retVoid [argPtr (castPtr raw_contentRuleList :: Ptr ())]

-- | Removes all associated content rule lists.
--
-- ObjC selector: @- removeAllContentRuleLists@
removeAllContentRuleLists :: IsWKUserContentController wkUserContentController => wkUserContentController -> IO ()
removeAllContentRuleLists wkUserContentController  =
  sendMsg wkUserContentController (mkSelector "removeAllContentRuleLists") retVoid []

-- | The user scripts associated with this user content controller.
--
-- ObjC selector: @- userScripts@
userScripts :: IsWKUserContentController wkUserContentController => wkUserContentController -> IO (Id NSArray)
userScripts wkUserContentController  =
  sendMsg wkUserContentController (mkSelector "userScripts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addUserScript:@
addUserScriptSelector :: Selector
addUserScriptSelector = mkSelector "addUserScript:"

-- | @Selector@ for @removeAllUserScripts@
removeAllUserScriptsSelector :: Selector
removeAllUserScriptsSelector = mkSelector "removeAllUserScripts"

-- | @Selector@ for @addScriptMessageHandler:contentWorld:name:@
addScriptMessageHandler_contentWorld_nameSelector :: Selector
addScriptMessageHandler_contentWorld_nameSelector = mkSelector "addScriptMessageHandler:contentWorld:name:"

-- | @Selector@ for @addScriptMessageHandlerWithReply:contentWorld:name:@
addScriptMessageHandlerWithReply_contentWorld_nameSelector :: Selector
addScriptMessageHandlerWithReply_contentWorld_nameSelector = mkSelector "addScriptMessageHandlerWithReply:contentWorld:name:"

-- | @Selector@ for @addScriptMessageHandler:name:@
addScriptMessageHandler_nameSelector :: Selector
addScriptMessageHandler_nameSelector = mkSelector "addScriptMessageHandler:name:"

-- | @Selector@ for @removeScriptMessageHandlerForName:contentWorld:@
removeScriptMessageHandlerForName_contentWorldSelector :: Selector
removeScriptMessageHandlerForName_contentWorldSelector = mkSelector "removeScriptMessageHandlerForName:contentWorld:"

-- | @Selector@ for @removeScriptMessageHandlerForName:@
removeScriptMessageHandlerForNameSelector :: Selector
removeScriptMessageHandlerForNameSelector = mkSelector "removeScriptMessageHandlerForName:"

-- | @Selector@ for @removeAllScriptMessageHandlersFromContentWorld:@
removeAllScriptMessageHandlersFromContentWorldSelector :: Selector
removeAllScriptMessageHandlersFromContentWorldSelector = mkSelector "removeAllScriptMessageHandlersFromContentWorld:"

-- | @Selector@ for @removeAllScriptMessageHandlers@
removeAllScriptMessageHandlersSelector :: Selector
removeAllScriptMessageHandlersSelector = mkSelector "removeAllScriptMessageHandlers"

-- | @Selector@ for @addContentRuleList:@
addContentRuleListSelector :: Selector
addContentRuleListSelector = mkSelector "addContentRuleList:"

-- | @Selector@ for @removeContentRuleList:@
removeContentRuleListSelector :: Selector
removeContentRuleListSelector = mkSelector "removeContentRuleList:"

-- | @Selector@ for @removeAllContentRuleLists@
removeAllContentRuleListsSelector :: Selector
removeAllContentRuleListsSelector = mkSelector "removeAllContentRuleLists"

-- | @Selector@ for @userScripts@
userScriptsSelector :: Selector
userScriptsSelector = mkSelector "userScripts"

