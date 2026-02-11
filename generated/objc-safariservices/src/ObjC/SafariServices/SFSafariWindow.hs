{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariWindow@.
module ObjC.SafariServices.SFSafariWindow
  ( SFSafariWindow
  , IsSFSafariWindow(..)
  , new
  , init_
  , getActiveTabWithCompletionHandler
  , openTabWithURL_makeActiveIfPossible_completionHandler
  , getToolbarItemWithCompletionHandler
  , close
  , newSelector
  , initSelector
  , getActiveTabWithCompletionHandlerSelector
  , openTabWithURL_makeActiveIfPossible_completionHandlerSelector
  , getToolbarItemWithCompletionHandlerSelector
  , closeSelector


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
new :: IO (Id SFSafariWindow)
new  =
  do
    cls' <- getRequiredClass "SFSafariWindow"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> IO (Id SFSafariWindow)
init_ sfSafariWindow  =
  sendMsg sfSafariWindow (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Calls the completion handler with the active tab in the window.
--
-- ObjC selector: @- getActiveTabWithCompletionHandler:@
getActiveTabWithCompletionHandler :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> Ptr () -> IO ()
getActiveTabWithCompletionHandler sfSafariWindow  completionHandler =
  sendMsg sfSafariWindow (mkSelector "getActiveTabWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | This will open a tab at the end of the tab list. The completion handler is called when the tab has been opened.
--
-- ObjC selector: @- openTabWithURL:makeActiveIfPossible:completionHandler:@
openTabWithURL_makeActiveIfPossible_completionHandler :: (IsSFSafariWindow sfSafariWindow, IsNSURL url) => sfSafariWindow -> url -> Bool -> Ptr () -> IO ()
openTabWithURL_makeActiveIfPossible_completionHandler sfSafariWindow  url activateTab completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg sfSafariWindow (mkSelector "openTabWithURL:makeActiveIfPossible:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (if activateTab then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

-- | Gets the extension’s toolbar item in this window.
--
-- ObjC selector: @- getToolbarItemWithCompletionHandler:@
getToolbarItemWithCompletionHandler :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> Ptr () -> IO ()
getToolbarItemWithCompletionHandler sfSafariWindow  completionHandler =
  sendMsg sfSafariWindow (mkSelector "getToolbarItemWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Closes this window.
--
-- ObjC selector: @- close@
close :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> IO ()
close sfSafariWindow  =
  sendMsg sfSafariWindow (mkSelector "close") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @getActiveTabWithCompletionHandler:@
getActiveTabWithCompletionHandlerSelector :: Selector
getActiveTabWithCompletionHandlerSelector = mkSelector "getActiveTabWithCompletionHandler:"

-- | @Selector@ for @openTabWithURL:makeActiveIfPossible:completionHandler:@
openTabWithURL_makeActiveIfPossible_completionHandlerSelector :: Selector
openTabWithURL_makeActiveIfPossible_completionHandlerSelector = mkSelector "openTabWithURL:makeActiveIfPossible:completionHandler:"

-- | @Selector@ for @getToolbarItemWithCompletionHandler:@
getToolbarItemWithCompletionHandlerSelector :: Selector
getToolbarItemWithCompletionHandlerSelector = mkSelector "getToolbarItemWithCompletionHandler:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

