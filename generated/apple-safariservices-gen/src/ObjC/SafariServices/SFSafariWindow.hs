{-# LANGUAGE DataKinds #-}
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
  , closeSelector
  , getActiveTabWithCompletionHandlerSelector
  , getToolbarItemWithCompletionHandlerSelector
  , initSelector
  , newSelector
  , openTabWithURL_makeActiveIfPossible_completionHandlerSelector


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
new :: IO (Id SFSafariWindow)
new  =
  do
    cls' <- getRequiredClass "SFSafariWindow"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> IO (Id SFSafariWindow)
init_ sfSafariWindow =
  sendOwnedMessage sfSafariWindow initSelector

-- | Calls the completion handler with the active tab in the window.
--
-- ObjC selector: @- getActiveTabWithCompletionHandler:@
getActiveTabWithCompletionHandler :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> Ptr () -> IO ()
getActiveTabWithCompletionHandler sfSafariWindow completionHandler =
  sendMessage sfSafariWindow getActiveTabWithCompletionHandlerSelector completionHandler

-- | This will open a tab at the end of the tab list. The completion handler is called when the tab has been opened.
--
-- ObjC selector: @- openTabWithURL:makeActiveIfPossible:completionHandler:@
openTabWithURL_makeActiveIfPossible_completionHandler :: (IsSFSafariWindow sfSafariWindow, IsNSURL url) => sfSafariWindow -> url -> Bool -> Ptr () -> IO ()
openTabWithURL_makeActiveIfPossible_completionHandler sfSafariWindow url activateTab completionHandler =
  sendMessage sfSafariWindow openTabWithURL_makeActiveIfPossible_completionHandlerSelector (toNSURL url) activateTab completionHandler

-- | Gets the extension’s toolbar item in this window.
--
-- ObjC selector: @- getToolbarItemWithCompletionHandler:@
getToolbarItemWithCompletionHandler :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> Ptr () -> IO ()
getToolbarItemWithCompletionHandler sfSafariWindow completionHandler =
  sendMessage sfSafariWindow getToolbarItemWithCompletionHandlerSelector completionHandler

-- | Closes this window.
--
-- ObjC selector: @- close@
close :: IsSFSafariWindow sfSafariWindow => sfSafariWindow -> IO ()
close sfSafariWindow =
  sendMessage sfSafariWindow closeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFSafariWindow)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSafariWindow)
initSelector = mkSelector "init"

-- | @Selector@ for @getActiveTabWithCompletionHandler:@
getActiveTabWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getActiveTabWithCompletionHandlerSelector = mkSelector "getActiveTabWithCompletionHandler:"

-- | @Selector@ for @openTabWithURL:makeActiveIfPossible:completionHandler:@
openTabWithURL_makeActiveIfPossible_completionHandlerSelector :: Selector '[Id NSURL, Bool, Ptr ()] ()
openTabWithURL_makeActiveIfPossible_completionHandlerSelector = mkSelector "openTabWithURL:makeActiveIfPossible:completionHandler:"

-- | @Selector@ for @getToolbarItemWithCompletionHandler:@
getToolbarItemWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getToolbarItemWithCompletionHandlerSelector = mkSelector "getToolbarItemWithCompletionHandler:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

