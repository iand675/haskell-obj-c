{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariApplication@.
module ObjC.SafariServices.SFSafariApplication
  ( SFSafariApplication
  , IsSFSafariApplication(..)
  , new
  , init_
  , getActiveWindowWithCompletionHandler
  , openWindowWithURL_completionHandler
  , setToolbarItemsNeedUpdate
  , getHostApplicationWithCompletionHandler
  , showPreferencesForExtensionWithIdentifier_completionHandler
  , dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandler
  , dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector
  , getActiveWindowWithCompletionHandlerSelector
  , getHostApplicationWithCompletionHandlerSelector
  , initSelector
  , newSelector
  , openWindowWithURL_completionHandlerSelector
  , setToolbarItemsNeedUpdateSelector
  , showPreferencesForExtensionWithIdentifier_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFSafariApplication)
new  =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFSafariApplication sfSafariApplication => sfSafariApplication -> IO (Id SFSafariApplication)
init_ sfSafariApplication =
  sendOwnedMessage sfSafariApplication initSelector

-- | Calls the completion handler with the active browser window.
--
-- ObjC selector: @+ getActiveWindowWithCompletionHandler:@
getActiveWindowWithCompletionHandler :: Ptr () -> IO ()
getActiveWindowWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMessage cls' getActiveWindowWithCompletionHandlerSelector completionHandler

-- | Opens a new window with a tab containing the URL to pass in.
--
-- ObjC selector: @+ openWindowWithURL:completionHandler:@
openWindowWithURL_completionHandler :: IsNSURL url => url -> Ptr () -> IO ()
openWindowWithURL_completionHandler url completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMessage cls' openWindowWithURL_completionHandlerSelector (toNSURL url) completionHandler

-- | This will cause -validateToolbarItemInWindow:completionHandler: to be called on all windows, to let the extension update enabled states or badges of its toolbar items.
--
-- ObjC selector: @+ setToolbarItemsNeedUpdate@
setToolbarItemsNeedUpdate :: IO ()
setToolbarItemsNeedUpdate  =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMessage cls' setToolbarItemsNeedUpdateSelector

-- | Gets an NSRunningApplication instance with information about the app that this extension is connected to.
--
-- ObjC selector: @+ getHostApplicationWithCompletionHandler:@
getHostApplicationWithCompletionHandler :: Ptr () -> IO ()
getHostApplicationWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMessage cls' getHostApplicationWithCompletionHandlerSelector completionHandler

-- | Opens Safari Extensions preferences and selects extension with the identifier.
--
-- ObjC selector: @+ showPreferencesForExtensionWithIdentifier:completionHandler:@
showPreferencesForExtensionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
showPreferencesForExtensionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMessage cls' showPreferencesForExtensionWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | @+ dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:@
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandler :: (IsNSString messageName, IsNSString identifier, IsNSDictionary userInfo) => messageName -> identifier -> userInfo -> Ptr () -> IO ()
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandler messageName identifier userInfo completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMessage cls' dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector (toNSString messageName) (toNSString identifier) (toNSDictionary userInfo) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFSafariApplication)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSafariApplication)
initSelector = mkSelector "init"

-- | @Selector@ for @getActiveWindowWithCompletionHandler:@
getActiveWindowWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getActiveWindowWithCompletionHandlerSelector = mkSelector "getActiveWindowWithCompletionHandler:"

-- | @Selector@ for @openWindowWithURL:completionHandler:@
openWindowWithURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
openWindowWithURL_completionHandlerSelector = mkSelector "openWindowWithURL:completionHandler:"

-- | @Selector@ for @setToolbarItemsNeedUpdate@
setToolbarItemsNeedUpdateSelector :: Selector '[] ()
setToolbarItemsNeedUpdateSelector = mkSelector "setToolbarItemsNeedUpdate"

-- | @Selector@ for @getHostApplicationWithCompletionHandler:@
getHostApplicationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getHostApplicationWithCompletionHandlerSelector = mkSelector "getHostApplicationWithCompletionHandler:"

-- | @Selector@ for @showPreferencesForExtensionWithIdentifier:completionHandler:@
showPreferencesForExtensionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
showPreferencesForExtensionWithIdentifier_completionHandlerSelector = mkSelector "showPreferencesForExtensionWithIdentifier:completionHandler:"

-- | @Selector@ for @dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:@
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary, Ptr ()] ()
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector = mkSelector "dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:"

