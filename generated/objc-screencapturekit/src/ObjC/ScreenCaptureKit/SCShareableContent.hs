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
  , getShareableContentWithCompletionHandlerSelector
  , getCurrentProcessShareableContentWithCompletionHandlerSelector
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector
  , getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector
  , infoForFilterSelector
  , initSelector
  , newSelector
  , windowsSelector
  , displaysSelector
  , applicationsSelector


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
    sendClassMsg cls' (mkSelector "getShareableContentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

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
    sendClassMsg cls' (mkSelector "getCurrentProcessShareableContentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

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
    sendClassMsg cls' (mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler:") retVoid [argCULong (if excludeDesktopWindows then 1 else 0), argCULong (if onScreenWindowsOnly then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

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
    withObjCPtr window $ \raw_window ->
      sendClassMsg cls' (mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler:") retVoid [argCULong (if excludeDesktopWindows then 1 else 0), argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
    withObjCPtr window $ \raw_window ->
      sendClassMsg cls' (mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler:") retVoid [argCULong (if excludeDesktopWindows then 1 else 0), argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
    withObjCPtr filter_ $ \raw_filter_ ->
      sendClassMsg cls' (mkSelector "infoForFilter:") (retPtr retVoid) [argPtr (castPtr raw_filter_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id SCShareableContent)
init_ scShareableContent  =
  sendMsg scShareableContent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SCShareableContent)
new  =
  do
    cls' <- getRequiredClass "SCShareableContent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | windows SCShareableContent property that contains all the sharable SCWindows
--
-- ObjC selector: @- windows@
windows :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id NSArray)
windows scShareableContent  =
  sendMsg scShareableContent (mkSelector "windows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | displays SCShareableContent property that contains all the sharable SCDisplays
--
-- ObjC selector: @- displays@
displays :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id NSArray)
displays scShareableContent  =
  sendMsg scShareableContent (mkSelector "displays") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applications SCShareableContent property that contains all the sharable SCRunningApplications
--
-- ObjC selector: @- applications@
applications :: IsSCShareableContent scShareableContent => scShareableContent -> IO (Id NSArray)
applications scShareableContent  =
  sendMsg scShareableContent (mkSelector "applications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getShareableContentWithCompletionHandler:@
getShareableContentWithCompletionHandlerSelector :: Selector
getShareableContentWithCompletionHandlerSelector = mkSelector "getShareableContentWithCompletionHandler:"

-- | @Selector@ for @getCurrentProcessShareableContentWithCompletionHandler:@
getCurrentProcessShareableContentWithCompletionHandlerSelector :: Selector
getCurrentProcessShareableContentWithCompletionHandlerSelector = mkSelector "getCurrentProcessShareableContentWithCompletionHandler:"

-- | @Selector@ for @getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector :: Selector
getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandlerSelector = mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnly:completionHandler:"

-- | @Selector@ for @getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector :: Selector
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyBelowWindow_completionHandlerSelector = mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyBelowWindow:completionHandler:"

-- | @Selector@ for @getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler:@
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector :: Selector
getShareableContentExcludingDesktopWindows_onScreenWindowsOnlyAboveWindow_completionHandlerSelector = mkSelector "getShareableContentExcludingDesktopWindows:onScreenWindowsOnlyAboveWindow:completionHandler:"

-- | @Selector@ for @infoForFilter:@
infoForFilterSelector :: Selector
infoForFilterSelector = mkSelector "infoForFilter:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @windows@
windowsSelector :: Selector
windowsSelector = mkSelector "windows"

-- | @Selector@ for @displays@
displaysSelector :: Selector
displaysSelector = mkSelector "displays"

-- | @Selector@ for @applications@
applicationsSelector :: Selector
applicationsSelector = mkSelector "applications"

