{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariPage@.
module ObjC.SafariServices.SFSafariPage
  ( SFSafariPage
  , IsSFSafariPage(..)
  , new
  , init_
  , dispatchMessageToScriptWithName_userInfo
  , reload
  , getPagePropertiesWithCompletionHandler
  , getContainingTabWithCompletionHandler
  , getScreenshotOfVisibleAreaWithCompletionHandler
  , dispatchMessageToScriptWithName_userInfoSelector
  , getContainingTabWithCompletionHandlerSelector
  , getPagePropertiesWithCompletionHandlerSelector
  , getScreenshotOfVisibleAreaWithCompletionHandlerSelector
  , initSelector
  , newSelector
  , reloadSelector


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
new :: IO (Id SFSafariPage)
new  =
  do
    cls' <- getRequiredClass "SFSafariPage"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFSafariPage sfSafariPage => sfSafariPage -> IO (Id SFSafariPage)
init_ sfSafariPage =
  sendOwnedMessage sfSafariPage initSelector

-- | Dispatches a message to the content script injected in this page.
--
-- ObjC selector: @- dispatchMessageToScriptWithName:userInfo:@
dispatchMessageToScriptWithName_userInfo :: (IsSFSafariPage sfSafariPage, IsNSString messageName, IsNSDictionary userInfo) => sfSafariPage -> messageName -> userInfo -> IO ()
dispatchMessageToScriptWithName_userInfo sfSafariPage messageName userInfo =
  sendMessage sfSafariPage dispatchMessageToScriptWithName_userInfoSelector (toNSString messageName) (toNSDictionary userInfo)

-- | Reloads the page.
--
-- ObjC selector: @- reload@
reload :: IsSFSafariPage sfSafariPage => sfSafariPage -> IO ()
reload sfSafariPage =
  sendMessage sfSafariPage reloadSelector

-- | This calls the completion handler with the properties of the page.
--
-- ObjC selector: @- getPagePropertiesWithCompletionHandler:@
getPagePropertiesWithCompletionHandler :: IsSFSafariPage sfSafariPage => sfSafariPage -> Ptr () -> IO ()
getPagePropertiesWithCompletionHandler sfSafariPage completionHandler =
  sendMessage sfSafariPage getPagePropertiesWithCompletionHandlerSelector completionHandler

-- | This calls the completion handler with the tab containing this page. This will return a non-nil tab for any pages being preloaded by Safari.
--
-- ObjC selector: @- getContainingTabWithCompletionHandler:@
getContainingTabWithCompletionHandler :: IsSFSafariPage sfSafariPage => sfSafariPage -> Ptr () -> IO ()
getContainingTabWithCompletionHandler sfSafariPage completionHandler =
  sendMessage sfSafariPage getContainingTabWithCompletionHandlerSelector completionHandler

-- | Gets a screenshot of the currently visible area of the page.
--
-- ObjC selector: @- getScreenshotOfVisibleAreaWithCompletionHandler:@
getScreenshotOfVisibleAreaWithCompletionHandler :: IsSFSafariPage sfSafariPage => sfSafariPage -> Ptr () -> IO ()
getScreenshotOfVisibleAreaWithCompletionHandler sfSafariPage completionHandler =
  sendMessage sfSafariPage getScreenshotOfVisibleAreaWithCompletionHandlerSelector completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFSafariPage)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSafariPage)
initSelector = mkSelector "init"

-- | @Selector@ for @dispatchMessageToScriptWithName:userInfo:@
dispatchMessageToScriptWithName_userInfoSelector :: Selector '[Id NSString, Id NSDictionary] ()
dispatchMessageToScriptWithName_userInfoSelector = mkSelector "dispatchMessageToScriptWithName:userInfo:"

-- | @Selector@ for @reload@
reloadSelector :: Selector '[] ()
reloadSelector = mkSelector "reload"

-- | @Selector@ for @getPagePropertiesWithCompletionHandler:@
getPagePropertiesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getPagePropertiesWithCompletionHandlerSelector = mkSelector "getPagePropertiesWithCompletionHandler:"

-- | @Selector@ for @getContainingTabWithCompletionHandler:@
getContainingTabWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getContainingTabWithCompletionHandlerSelector = mkSelector "getContainingTabWithCompletionHandler:"

-- | @Selector@ for @getScreenshotOfVisibleAreaWithCompletionHandler:@
getScreenshotOfVisibleAreaWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getScreenshotOfVisibleAreaWithCompletionHandlerSelector = mkSelector "getScreenshotOfVisibleAreaWithCompletionHandler:"

