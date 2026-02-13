{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCShareableContent@.
module ObjC.ScreenCaptureKit.SCShareableContent
  ( SCShareableContent
  , IsSCShareableContent(..)
  , getShareableContentWithCompletionHandler
  , getCurrentProcessShareableContentWithCompletionHandler
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandler
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandler
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandler
  , infoForFilter
  , init_
  , new
  , windows
  , displays
  , applications
  , applicationsSelector
  , displaysSelector
  , getCurrentProcessShareableContentWithCompletionHandlerSelector
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector
  , getShareableContentWithCompletionHandlerSelector
  , infoForFilterSelector
  , initSelector
  , newSelector
  , windowsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | getShareableContentWithCompletionHandler:completionHandler
--
-- @completionHandler@ — the call back that will hand you back a SCShareableContent object
--
-- this method will create a SCShareableContent object that is called on the supplied queue. The SCShareableContent will contain the windows, displays and applications that are available to capture
--
-- ObjC selector: @+ getShareableContentWithCompletionHandler:@
getShareableContentWithCompletionHandler :: Ptr () -> IO ()
getShareableContentWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMessage cls' getShareableContentWithCompletionHandlerSelector completionHandler

-- | getCurrentProcessShareableContentWithCompletionHandler:completionHandler
--
-- @completionHandler@ — the call back that will hand you back a SCShareableContent object
--
-- this method will create a SCShareableContent object that is called on the supplied queue. The SCShareableContent will contain redacted information about windows, displays and applications that are available to capture by current process without user consent via TCC
--
-- ObjC selector: @+ getCurrentProcessShareableContentWithCompletionHandler:@
getCurrentProcessShareableContentWithCompletionHandler :: Ptr () -> IO ()
getCurrentProcessShareableContentWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMessage cls' getCurrentProcessShareableContentWithCompletionHandlerSelector completionHandler

-- | getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler
--
-- @excludeDesktopWindows@ — a BOOL indicating if we should exclude desktop windows
--
-- @onScreenWindowsOnly@ — filter only windows that are on screen
--
-- @completionHandler@ — the call back that will hand you back a SCShareableContent object
--
-- this method will create a SCShareableContent object that is called on the supplied queue. The SCShareableContent will contain the windows, displays and applications that are available to capture
--
-- ObjC selector: @+ getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandler :: Bool -> Bool -> Ptr () -> IO ()
getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandler excludeDesktopWindows onScreenWindowsOnly completionHandler =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMessage cls' getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector excludeDesktopWindows onScreenWindowsOnly completionHandler

-- | getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler
--
-- @excludeDesktopWindows@ — a BOOL indicating if we should exclude desktop windows
--
-- @window@ — filter only windows below this SCWindow
--
-- @completionHandler@ — the call back that will hand you back a SCShareableContent object
--
-- this method will create a SCShareableContent object that is called on the supplied queue. The SCShareableContent will contain the windows, displays and applications that are available to capture
--
-- ObjC selector: @+ getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandler :: IsSCWindow window => Bool -> window -> Ptr () -> IO ()
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandler excludeDesktopWindows window completionHandler =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMessage cls' getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector excludeDesktopWindows (toSCWindow window) completionHandler

-- | getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler
--
-- @excludeDesktopWindows@ — a BOOL indicating if we should exclude desktop windows
--
-- @window@ — filter only windows above this SCWindow
--
-- @completionHandler@ — the call back that will hand you back a SCShareableContent object
--
-- this method will create a SCShareableContent object that is called on the supplied queue. The SCShareableContent will contain the windows, displays and applications that are available to capture
--
-- ObjC selector: @+ getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandler :: IsSCWindow window => Bool -> window -> Ptr () -> IO ()
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandler excludeDesktopWindows window completionHandler =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMessage cls' getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector excludeDesktopWindows (toSCWindow window) completionHandler

-- | infoForFilter:
--
-- @filter@ — content filter to translate to content details
--
-- this method will create a SCShareableContentInformation object given a filter
--
-- ObjC selector: @+ infoForFilter:@
infoForFilter :: IsSCContentFilter filter_ => filter_ -> IO (Id SCShareableContentInfo)
infoForFilter filter_ =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMessage cls' infoForFilterSelector (toSCContentFilter filter_)

-- | @- init@
init_ :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id SCShareableContent)
init_ scShareableContent =
  sendOwnedMessage scShareableContent initSelector

-- | @+ new@
new :: IO (Id SCShareableContent)
new  =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendOwnedClassMessage cls' newSelector

-- | windows SCShareableContent property that contains all the sharable SCWindows
--
-- ObjC selector: @- windows@
windows :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id NSArray)
windows scShareableContent =
  sendMessage scShareableContent windowsSelector

-- | displays SCShareableContent property that contains all the sharable SCDisplays
--
-- ObjC selector: @- displays@
displays :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id NSArray)
displays scShareableContent =
  sendMessage scShareableContent displaysSelector

-- | applications SCShareableContent property that contains all the sharable SCRunningApplications
--
-- ObjC selector: @- applications@
applications :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id NSArray)
applications scShareableContent =
  sendMessage scShareableContent applicationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getShareableContentWithCompletionHandler:@
getShareableContentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getShareableContentWithCompletionHandlerSelector = mkSelector "getShareableContentWithCompletionHandler:"

-- | @Selector@ for @getCurrentProcessShareableContentWithCompletionHandler:@
getCurrentProcessShareableContentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getCurrentProcessShareableContentWithCompletionHandlerSelector = mkSelector "getCurrentProcessShareableContentWithCompletionHandler:"

-- | @Selector@ for @getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector :: Selector '[Bool, Bool, Ptr ()] ()
getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector = mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler:"

-- | @Selector@ for @getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector :: Selector '[Bool, Id SCWindow, Ptr ()] ()
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector = mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler:"

-- | @Selector@ for @getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector :: Selector '[Bool, Id SCWindow, Ptr ()] ()
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector = mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler:"

-- | @Selector@ for @infoForFilter:@
infoForFilterSelector :: Selector '[Id SCContentFilter] (Id SCShareableContentInfo)
infoForFilterSelector = mkSelector "infoForFilter:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCShareableContent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SCShareableContent)
newSelector = mkSelector "new"

-- | @Selector@ for @windows@
windowsSelector :: Selector '[] (Id NSArray)
windowsSelector = mkSelector "windows"

-- | @Selector@ for @displays@
displaysSelector :: Selector '[] (Id NSArray)
displaysSelector = mkSelector "displays"

-- | @Selector@ for @applications@
applicationsSelector :: Selector '[] (Id NSArray)
applicationsSelector = mkSelector "applications"

