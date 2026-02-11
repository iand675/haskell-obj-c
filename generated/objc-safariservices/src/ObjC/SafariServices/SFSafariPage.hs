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
  , newSelector
  , initSelector
  , dispatchMessageToScriptWithName_userInfoSelector
  , reloadSelector
  , getPagePropertiesWithCompletionHandlerSelector
  , getContainingTabWithCompletionHandlerSelector
  , getScreenshotOfVisibleAreaWithCompletionHandlerSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFSafariPage)
new  =
  do
    cls' <- getRequiredClass "SFSafariPage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFSafariPage sfSafariPage => sfSafariPage -> IO (Id SFSafariPage)
init_ sfSafariPage  =
  sendMsg sfSafariPage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Dispatches a message to the content script injected in this page.
--
-- ObjC selector: @- dispatchMessageToScriptWithName:userInfo:@
dispatchMessageToScriptWithName_userInfo :: (IsSFSafariPage sfSafariPage, IsNSString messageName, IsNSDictionary userInfo) => sfSafariPage -> messageName -> userInfo -> IO ()
dispatchMessageToScriptWithName_userInfo sfSafariPage  messageName userInfo =
withObjCPtr messageName $ \raw_messageName ->
  withObjCPtr userInfo $ \raw_userInfo ->
      sendMsg sfSafariPage (mkSelector "dispatchMessageToScriptWithName:userInfo:") retVoid [argPtr (castPtr raw_messageName :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ())]

-- | Reloads the page.
--
-- ObjC selector: @- reload@
reload :: IsSFSafariPage sfSafariPage => sfSafariPage -> IO ()
reload sfSafariPage  =
  sendMsg sfSafariPage (mkSelector "reload") retVoid []

-- | This calls the completion handler with the properties of the page.
--
-- ObjC selector: @- getPagePropertiesWithCompletionHandler:@
getPagePropertiesWithCompletionHandler :: IsSFSafariPage sfSafariPage => sfSafariPage -> Ptr () -> IO ()
getPagePropertiesWithCompletionHandler sfSafariPage  completionHandler =
  sendMsg sfSafariPage (mkSelector "getPagePropertiesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | This calls the completion handler with the tab containing this page. This will return a non-nil tab for any pages being preloaded by Safari.
--
-- ObjC selector: @- getContainingTabWithCompletionHandler:@
getContainingTabWithCompletionHandler :: IsSFSafariPage sfSafariPage => sfSafariPage -> Ptr () -> IO ()
getContainingTabWithCompletionHandler sfSafariPage  completionHandler =
  sendMsg sfSafariPage (mkSelector "getContainingTabWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Gets a screenshot of the currently visible area of the page.
--
-- ObjC selector: @- getScreenshotOfVisibleAreaWithCompletionHandler:@
getScreenshotOfVisibleAreaWithCompletionHandler :: IsSFSafariPage sfSafariPage => sfSafariPage -> Ptr () -> IO ()
getScreenshotOfVisibleAreaWithCompletionHandler sfSafariPage  completionHandler =
  sendMsg sfSafariPage (mkSelector "getScreenshotOfVisibleAreaWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @dispatchMessageToScriptWithName:userInfo:@
dispatchMessageToScriptWithName_userInfoSelector :: Selector
dispatchMessageToScriptWithName_userInfoSelector = mkSelector "dispatchMessageToScriptWithName:userInfo:"

-- | @Selector@ for @reload@
reloadSelector :: Selector
reloadSelector = mkSelector "reload"

-- | @Selector@ for @getPagePropertiesWithCompletionHandler:@
getPagePropertiesWithCompletionHandlerSelector :: Selector
getPagePropertiesWithCompletionHandlerSelector = mkSelector "getPagePropertiesWithCompletionHandler:"

-- | @Selector@ for @getContainingTabWithCompletionHandler:@
getContainingTabWithCompletionHandlerSelector :: Selector
getContainingTabWithCompletionHandlerSelector = mkSelector "getContainingTabWithCompletionHandler:"

-- | @Selector@ for @getScreenshotOfVisibleAreaWithCompletionHandler:@
getScreenshotOfVisibleAreaWithCompletionHandlerSelector :: Selector
getScreenshotOfVisibleAreaWithCompletionHandlerSelector = mkSelector "getScreenshotOfVisibleAreaWithCompletionHandler:"

