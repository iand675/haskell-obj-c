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
  , newSelector
  , initSelector
  , getActiveWindowWithCompletionHandlerSelector
  , openWindowWithURL_completionHandlerSelector
  , setToolbarItemsNeedUpdateSelector
  , getHostApplicationWithCompletionHandlerSelector
  , showPreferencesForExtensionWithIdentifier_completionHandlerSelector
  , dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector


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
new :: IO (Id SFSafariApplication)
new  =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFSafariApplication sfSafariApplication => sfSafariApplication -> IO (Id SFSafariApplication)
init_ sfSafariApplication  =
  sendMsg sfSafariApplication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Calls the completion handler with the active browser window.
--
-- ObjC selector: @+ getActiveWindowWithCompletionHandler:@
getActiveWindowWithCompletionHandler :: Ptr () -> IO ()
getActiveWindowWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMsg cls' (mkSelector "getActiveWindowWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Opens a new window with a tab containing the URL to pass in.
--
-- ObjC selector: @+ openWindowWithURL:completionHandler:@
openWindowWithURL_completionHandler :: IsNSURL url => url -> Ptr () -> IO ()
openWindowWithURL_completionHandler url completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "openWindowWithURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | This will cause -validateToolbarItemInWindow:completionHandler: to be called on all windows, to let the extension update enabled states or badges of its toolbar items.
--
-- ObjC selector: @+ setToolbarItemsNeedUpdate@
setToolbarItemsNeedUpdate :: IO ()
setToolbarItemsNeedUpdate  =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMsg cls' (mkSelector "setToolbarItemsNeedUpdate") retVoid []

-- | Gets an NSRunningApplication instance with information about the app that this extension is connected to.
--
-- ObjC selector: @+ getHostApplicationWithCompletionHandler:@
getHostApplicationWithCompletionHandler :: Ptr () -> IO ()
getHostApplicationWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    sendClassMsg cls' (mkSelector "getHostApplicationWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Opens Safari Extensions preferences and selects extension with the identifier.
--
-- ObjC selector: @+ showPreferencesForExtensionWithIdentifier:completionHandler:@
showPreferencesForExtensionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
showPreferencesForExtensionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "showPreferencesForExtensionWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:@
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandler :: (IsNSString messageName, IsNSString identifier, IsNSDictionary userInfo) => messageName -> identifier -> userInfo -> Ptr () -> IO ()
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandler messageName identifier userInfo completionHandler =
  do
    cls' <- getRequiredClass "SFSafariApplication"
    withObjCPtr messageName $ \raw_messageName ->
      withObjCPtr identifier $ \raw_identifier ->
        withObjCPtr userInfo $ \raw_userInfo ->
          sendClassMsg cls' (mkSelector "dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:") retVoid [argPtr (castPtr raw_messageName :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @getActiveWindowWithCompletionHandler:@
getActiveWindowWithCompletionHandlerSelector :: Selector
getActiveWindowWithCompletionHandlerSelector = mkSelector "getActiveWindowWithCompletionHandler:"

-- | @Selector@ for @openWindowWithURL:completionHandler:@
openWindowWithURL_completionHandlerSelector :: Selector
openWindowWithURL_completionHandlerSelector = mkSelector "openWindowWithURL:completionHandler:"

-- | @Selector@ for @setToolbarItemsNeedUpdate@
setToolbarItemsNeedUpdateSelector :: Selector
setToolbarItemsNeedUpdateSelector = mkSelector "setToolbarItemsNeedUpdate"

-- | @Selector@ for @getHostApplicationWithCompletionHandler:@
getHostApplicationWithCompletionHandlerSelector :: Selector
getHostApplicationWithCompletionHandlerSelector = mkSelector "getHostApplicationWithCompletionHandler:"

-- | @Selector@ for @showPreferencesForExtensionWithIdentifier:completionHandler:@
showPreferencesForExtensionWithIdentifier_completionHandlerSelector :: Selector
showPreferencesForExtensionWithIdentifier_completionHandlerSelector = mkSelector "showPreferencesForExtensionWithIdentifier:completionHandler:"

-- | @Selector@ for @dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:@
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector :: Selector
dispatchMessageWithName_toExtensionWithIdentifier_userInfo_completionHandlerSelector = mkSelector "dispatchMessageWithName:toExtensionWithIdentifier:userInfo:completionHandler:"

