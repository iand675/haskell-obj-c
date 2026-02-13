{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWindowTabGroup@.
module ObjC.AppKit.NSWindowTabGroup
  ( NSWindowTabGroup
  , IsNSWindowTabGroup(..)
  , addWindow
  , insertWindow_atIndex
  , removeWindow
  , identifier
  , windows
  , overviewVisible
  , setOverviewVisible
  , tabBarVisible
  , selectedWindow
  , setSelectedWindow
  , addWindowSelector
  , identifierSelector
  , insertWindow_atIndexSelector
  , overviewVisibleSelector
  , removeWindowSelector
  , selectedWindowSelector
  , setOverviewVisibleSelector
  , setSelectedWindowSelector
  , tabBarVisibleSelector
  , windowsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addWindow:@
addWindow :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow window) => nsWindowTabGroup -> window -> IO ()
addWindow nsWindowTabGroup window =
  sendMessage nsWindowTabGroup addWindowSelector (toNSWindow window)

-- | @- insertWindow:atIndex:@
insertWindow_atIndex :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow window) => nsWindowTabGroup -> window -> CLong -> IO ()
insertWindow_atIndex nsWindowTabGroup window index =
  sendMessage nsWindowTabGroup insertWindow_atIndexSelector (toNSWindow window) index

-- | @- removeWindow:@
removeWindow :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow window) => nsWindowTabGroup -> window -> IO ()
removeWindow nsWindowTabGroup window =
  sendMessage nsWindowTabGroup removeWindowSelector (toNSWindow window)

-- | @- identifier@
identifier :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO (Id NSString)
identifier nsWindowTabGroup =
  sendMessage nsWindowTabGroup identifierSelector

-- | @- windows@
windows :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO (Id NSArray)
windows nsWindowTabGroup =
  sendMessage nsWindowTabGroup windowsSelector

-- | @- overviewVisible@
overviewVisible :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO Bool
overviewVisible nsWindowTabGroup =
  sendMessage nsWindowTabGroup overviewVisibleSelector

-- | @- setOverviewVisible:@
setOverviewVisible :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> Bool -> IO ()
setOverviewVisible nsWindowTabGroup value =
  sendMessage nsWindowTabGroup setOverviewVisibleSelector value

-- | @- tabBarVisible@
tabBarVisible :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO Bool
tabBarVisible nsWindowTabGroup =
  sendMessage nsWindowTabGroup tabBarVisibleSelector

-- | @- selectedWindow@
selectedWindow :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO (Id NSWindow)
selectedWindow nsWindowTabGroup =
  sendMessage nsWindowTabGroup selectedWindowSelector

-- | @- setSelectedWindow:@
setSelectedWindow :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow value) => nsWindowTabGroup -> value -> IO ()
setSelectedWindow nsWindowTabGroup value =
  sendMessage nsWindowTabGroup setSelectedWindowSelector (toNSWindow value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addWindow:@
addWindowSelector :: Selector '[Id NSWindow] ()
addWindowSelector = mkSelector "addWindow:"

-- | @Selector@ for @insertWindow:atIndex:@
insertWindow_atIndexSelector :: Selector '[Id NSWindow, CLong] ()
insertWindow_atIndexSelector = mkSelector "insertWindow:atIndex:"

-- | @Selector@ for @removeWindow:@
removeWindowSelector :: Selector '[Id NSWindow] ()
removeWindowSelector = mkSelector "removeWindow:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @windows@
windowsSelector :: Selector '[] (Id NSArray)
windowsSelector = mkSelector "windows"

-- | @Selector@ for @overviewVisible@
overviewVisibleSelector :: Selector '[] Bool
overviewVisibleSelector = mkSelector "overviewVisible"

-- | @Selector@ for @setOverviewVisible:@
setOverviewVisibleSelector :: Selector '[Bool] ()
setOverviewVisibleSelector = mkSelector "setOverviewVisible:"

-- | @Selector@ for @tabBarVisible@
tabBarVisibleSelector :: Selector '[] Bool
tabBarVisibleSelector = mkSelector "tabBarVisible"

-- | @Selector@ for @selectedWindow@
selectedWindowSelector :: Selector '[] (Id NSWindow)
selectedWindowSelector = mkSelector "selectedWindow"

-- | @Selector@ for @setSelectedWindow:@
setSelectedWindowSelector :: Selector '[Id NSWindow] ()
setSelectedWindowSelector = mkSelector "setSelectedWindow:"

