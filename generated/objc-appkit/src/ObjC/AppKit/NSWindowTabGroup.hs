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
  , insertWindow_atIndexSelector
  , removeWindowSelector
  , identifierSelector
  , windowsSelector
  , overviewVisibleSelector
  , setOverviewVisibleSelector
  , tabBarVisibleSelector
  , selectedWindowSelector
  , setSelectedWindowSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addWindow:@
addWindow :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow window) => nsWindowTabGroup -> window -> IO ()
addWindow nsWindowTabGroup  window =
withObjCPtr window $ \raw_window ->
    sendMsg nsWindowTabGroup (mkSelector "addWindow:") retVoid [argPtr (castPtr raw_window :: Ptr ())]

-- | @- insertWindow:atIndex:@
insertWindow_atIndex :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow window) => nsWindowTabGroup -> window -> CLong -> IO ()
insertWindow_atIndex nsWindowTabGroup  window index =
withObjCPtr window $ \raw_window ->
    sendMsg nsWindowTabGroup (mkSelector "insertWindow:atIndex:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argCLong (fromIntegral index)]

-- | @- removeWindow:@
removeWindow :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow window) => nsWindowTabGroup -> window -> IO ()
removeWindow nsWindowTabGroup  window =
withObjCPtr window $ \raw_window ->
    sendMsg nsWindowTabGroup (mkSelector "removeWindow:") retVoid [argPtr (castPtr raw_window :: Ptr ())]

-- | @- identifier@
identifier :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO (Id NSString)
identifier nsWindowTabGroup  =
  sendMsg nsWindowTabGroup (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windows@
windows :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO (Id NSArray)
windows nsWindowTabGroup  =
  sendMsg nsWindowTabGroup (mkSelector "windows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- overviewVisible@
overviewVisible :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO Bool
overviewVisible nsWindowTabGroup  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindowTabGroup (mkSelector "overviewVisible") retCULong []

-- | @- setOverviewVisible:@
setOverviewVisible :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> Bool -> IO ()
setOverviewVisible nsWindowTabGroup  value =
  sendMsg nsWindowTabGroup (mkSelector "setOverviewVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tabBarVisible@
tabBarVisible :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO Bool
tabBarVisible nsWindowTabGroup  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindowTabGroup (mkSelector "tabBarVisible") retCULong []

-- | @- selectedWindow@
selectedWindow :: IsNSWindowTabGroup nsWindowTabGroup => nsWindowTabGroup -> IO (Id NSWindow)
selectedWindow nsWindowTabGroup  =
  sendMsg nsWindowTabGroup (mkSelector "selectedWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedWindow:@
setSelectedWindow :: (IsNSWindowTabGroup nsWindowTabGroup, IsNSWindow value) => nsWindowTabGroup -> value -> IO ()
setSelectedWindow nsWindowTabGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWindowTabGroup (mkSelector "setSelectedWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addWindow:@
addWindowSelector :: Selector
addWindowSelector = mkSelector "addWindow:"

-- | @Selector@ for @insertWindow:atIndex:@
insertWindow_atIndexSelector :: Selector
insertWindow_atIndexSelector = mkSelector "insertWindow:atIndex:"

-- | @Selector@ for @removeWindow:@
removeWindowSelector :: Selector
removeWindowSelector = mkSelector "removeWindow:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @windows@
windowsSelector :: Selector
windowsSelector = mkSelector "windows"

-- | @Selector@ for @overviewVisible@
overviewVisibleSelector :: Selector
overviewVisibleSelector = mkSelector "overviewVisible"

-- | @Selector@ for @setOverviewVisible:@
setOverviewVisibleSelector :: Selector
setOverviewVisibleSelector = mkSelector "setOverviewVisible:"

-- | @Selector@ for @tabBarVisible@
tabBarVisibleSelector :: Selector
tabBarVisibleSelector = mkSelector "tabBarVisible"

-- | @Selector@ for @selectedWindow@
selectedWindowSelector :: Selector
selectedWindowSelector = mkSelector "selectedWindow"

-- | @Selector@ for @setSelectedWindow:@
setSelectedWindowSelector :: Selector
setSelectedWindowSelector = mkSelector "setSelectedWindow:"

