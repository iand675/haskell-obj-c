{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariTab@.
module ObjC.SafariServices.SFSafariTab
  ( SFSafariTab
  , IsSFSafariTab(..)
  , new
  , init_
  , getActivePageWithCompletionHandler
  , getContainingWindowWithCompletionHandler
  , activateWithCompletionHandler
  , navigateToURL
  , close
  , activateWithCompletionHandlerSelector
  , closeSelector
  , getActivePageWithCompletionHandlerSelector
  , getContainingWindowWithCompletionHandlerSelector
  , initSelector
  , navigateToURLSelector
  , newSelector


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
new :: IO (Id SFSafariTab)
new  =
  do
    cls' <- getRequiredClass "SFSafariTab"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFSafariTab sfSafariTab => sfSafariTab -> IO (Id SFSafariTab)
init_ sfSafariTab =
  sendOwnedMessage sfSafariTab initSelector

-- | This calls the completion handler passing the active page in the tab.
--
-- ObjC selector: @- getActivePageWithCompletionHandler:@
getActivePageWithCompletionHandler :: IsSFSafariTab sfSafariTab => sfSafariTab -> Ptr () -> IO ()
getActivePageWithCompletionHandler sfSafariTab completionHandler =
  sendMessage sfSafariTab getActivePageWithCompletionHandlerSelector completionHandler

-- | This calls completion handler with the window containing this tab. If the tab is pinned, the window is nil.
--
-- ObjC selector: @- getContainingWindowWithCompletionHandler:@
getContainingWindowWithCompletionHandler :: IsSFSafariTab sfSafariTab => sfSafariTab -> Ptr () -> IO ()
getContainingWindowWithCompletionHandler sfSafariTab completionHandler =
  sendMessage sfSafariTab getContainingWindowWithCompletionHandlerSelector completionHandler

-- | Activates this tab in the window it belongs to.
--
-- ObjC selector: @- activateWithCompletionHandler:@
activateWithCompletionHandler :: IsSFSafariTab sfSafariTab => sfSafariTab -> Ptr () -> IO ()
activateWithCompletionHandler sfSafariTab completionHandler =
  sendMessage sfSafariTab activateWithCompletionHandlerSelector completionHandler

-- | Navigates this tab to the given URL. The extension doesn't need permission to access the URL to navigate to it.
--
-- ObjC selector: @- navigateToURL:@
navigateToURL :: (IsSFSafariTab sfSafariTab, IsNSURL url) => sfSafariTab -> url -> IO ()
navigateToURL sfSafariTab url =
  sendMessage sfSafariTab navigateToURLSelector (toNSURL url)

-- | Closes this tab. If this is the last tab in its window, the window is also closed.
--
-- ObjC selector: @- close@
close :: IsSFSafariTab sfSafariTab => sfSafariTab -> IO ()
close sfSafariTab =
  sendMessage sfSafariTab closeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFSafariTab)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSafariTab)
initSelector = mkSelector "init"

-- | @Selector@ for @getActivePageWithCompletionHandler:@
getActivePageWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getActivePageWithCompletionHandlerSelector = mkSelector "getActivePageWithCompletionHandler:"

-- | @Selector@ for @getContainingWindowWithCompletionHandler:@
getContainingWindowWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getContainingWindowWithCompletionHandlerSelector = mkSelector "getContainingWindowWithCompletionHandler:"

-- | @Selector@ for @activateWithCompletionHandler:@
activateWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
activateWithCompletionHandlerSelector = mkSelector "activateWithCompletionHandler:"

-- | @Selector@ for @navigateToURL:@
navigateToURLSelector :: Selector '[Id NSURL] ()
navigateToURLSelector = mkSelector "navigateToURL:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

