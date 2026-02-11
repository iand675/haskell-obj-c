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
  , newSelector
  , initSelector
  , getActivePageWithCompletionHandlerSelector
  , getContainingWindowWithCompletionHandlerSelector
  , activateWithCompletionHandlerSelector
  , navigateToURLSelector
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
new :: IO (Id SFSafariTab)
new  =
  do
    cls' <- getRequiredClass "SFSafariTab"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFSafariTab sfSafariTab => sfSafariTab -> IO (Id SFSafariTab)
init_ sfSafariTab  =
  sendMsg sfSafariTab (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | This calls the completion handler passing the active page in the tab.
--
-- ObjC selector: @- getActivePageWithCompletionHandler:@
getActivePageWithCompletionHandler :: IsSFSafariTab sfSafariTab => sfSafariTab -> Ptr () -> IO ()
getActivePageWithCompletionHandler sfSafariTab  completionHandler =
  sendMsg sfSafariTab (mkSelector "getActivePageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | This calls completion handler with the window containing this tab. If the tab is pinned, the window is nil.
--
-- ObjC selector: @- getContainingWindowWithCompletionHandler:@
getContainingWindowWithCompletionHandler :: IsSFSafariTab sfSafariTab => sfSafariTab -> Ptr () -> IO ()
getContainingWindowWithCompletionHandler sfSafariTab  completionHandler =
  sendMsg sfSafariTab (mkSelector "getContainingWindowWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Activates this tab in the window it belongs to.
--
-- ObjC selector: @- activateWithCompletionHandler:@
activateWithCompletionHandler :: IsSFSafariTab sfSafariTab => sfSafariTab -> Ptr () -> IO ()
activateWithCompletionHandler sfSafariTab  completionHandler =
  sendMsg sfSafariTab (mkSelector "activateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Navigates this tab to the given URL. The extension doesn't need permission to access the URL to navigate to it.
--
-- ObjC selector: @- navigateToURL:@
navigateToURL :: (IsSFSafariTab sfSafariTab, IsNSURL url) => sfSafariTab -> url -> IO ()
navigateToURL sfSafariTab  url =
withObjCPtr url $ \raw_url ->
    sendMsg sfSafariTab (mkSelector "navigateToURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | Closes this tab. If this is the last tab in its window, the window is also closed.
--
-- ObjC selector: @- close@
close :: IsSFSafariTab sfSafariTab => sfSafariTab -> IO ()
close sfSafariTab  =
  sendMsg sfSafariTab (mkSelector "close") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @getActivePageWithCompletionHandler:@
getActivePageWithCompletionHandlerSelector :: Selector
getActivePageWithCompletionHandlerSelector = mkSelector "getActivePageWithCompletionHandler:"

-- | @Selector@ for @getContainingWindowWithCompletionHandler:@
getContainingWindowWithCompletionHandlerSelector :: Selector
getContainingWindowWithCompletionHandlerSelector = mkSelector "getContainingWindowWithCompletionHandler:"

-- | @Selector@ for @activateWithCompletionHandler:@
activateWithCompletionHandlerSelector :: Selector
activateWithCompletionHandlerSelector = mkSelector "activateWithCompletionHandler:"

-- | @Selector@ for @navigateToURL:@
navigateToURLSelector :: Selector
navigateToURLSelector = mkSelector "navigateToURL:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

