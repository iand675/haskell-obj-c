{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that implements the Quick Look preview panel to display a preview of a list of items.
--
-- Every application has a single shared instance of ``QuickLookUI/QLPreviewPanel`` accessible through ``QuickLookUI/QLPreviewPanel/sharedPreviewPanel``. The preview panel follows the responder chain and adapts to the first responder willing to control it. A preview panel controller provides the content through methods defined in the ``QuickLookUI/QLPreviewPanelDataSource`` protocol.
--
-- You can’t subclass ``QuickLookUI/QLPreviewPanel``; you can, however, customize its behavior using a ``QuickLookUI/QLPreviewPanel/delegate``. See the ``QuickLookUI/QLPreviewPanelDelegate`` protocol for the methods to customize a preview panel’s behavior.
--
-- Generated bindings for @QLPreviewPanel@.
module ObjC.QuickLookUI.QLPreviewPanel
  ( QLPreviewPanel
  , IsQLPreviewPanel(..)
  , sharedPreviewPanel
  , sharedPreviewPanelExists
  , updateController
  , reloadData
  , refreshCurrentPreviewItem
  , enterFullScreenMode_withOptions
  , exitFullScreenModeWithOptions
  , currentController
  , dataSource
  , setDataSource
  , currentPreviewItemIndex
  , setCurrentPreviewItemIndex
  , currentPreviewItem
  , displayState
  , setDisplayState
  , delegate
  , setDelegate
  , inFullScreenMode
  , sharedPreviewPanelSelector
  , sharedPreviewPanelExistsSelector
  , updateControllerSelector
  , reloadDataSelector
  , refreshCurrentPreviewItemSelector
  , enterFullScreenMode_withOptionsSelector
  , exitFullScreenModeWithOptionsSelector
  , currentControllerSelector
  , dataSourceSelector
  , setDataSourceSelector
  , currentPreviewItemIndexSelector
  , setCurrentPreviewItemIndexSelector
  , currentPreviewItemSelector
  , displayStateSelector
  , setDisplayStateSelector
  , delegateSelector
  , setDelegateSelector
  , inFullScreenModeSelector


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

import ObjC.QuickLookUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the shared Quick Look preview panel instance.
--
-- This method creates the panel if it doesn’t exist yet. Use ``QuickLookUI/QLPreviewPanel/sharedPreviewPanelExists`` if you want to determine whether the panel exists without creating it.
--
-- - Returns: The shared Quick Look preview panel instance for the application.
--
-- ObjC selector: @+ sharedPreviewPanel@
sharedPreviewPanel :: IO (Id QLPreviewPanel)
sharedPreviewPanel  =
  do
    cls' <- getRequiredClass "QLPreviewPanel"
    sendClassMsg cls' (mkSelector "sharedPreviewPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a Boolean value that indicates whether the system has created a shared Quick Look preview panel.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the shared Quick Look preview panel instance has been created, otherwise <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @+ sharedPreviewPanelExists@
sharedPreviewPanelExists :: IO Bool
sharedPreviewPanelExists  =
  do
    cls' <- getRequiredClass "QLPreviewPanel"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "sharedPreviewPanelExists") retCULong []

-- | Asks the preview panel to update its current controller.
--
-- The preview panel automatically updates its controller (by searching the responder chain) whenever the main or key window changes. You should only invoke this method if the responder chain changes without explicit notice.
--
-- ObjC selector: @- updateController@
updateController :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO ()
updateController qlPreviewPanel  =
    sendMsg qlPreviewPanel (mkSelector "updateController") retVoid []

-- | Asks the preview panel to reload its data from its data source.
--
-- This method doesn’t refresh the visible item if it hasn’t changed.
--
-- ObjC selector: @- reloadData@
reloadData :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO ()
reloadData qlPreviewPanel  =
    sendMsg qlPreviewPanel (mkSelector "reloadData") retVoid []

-- | Asks the preview panel to recompute the preview of the current preview item.
--
-- ObjC selector: @- refreshCurrentPreviewItem@
refreshCurrentPreviewItem :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO ()
refreshCurrentPreviewItem qlPreviewPanel  =
    sendMsg qlPreviewPanel (mkSelector "refreshCurrentPreviewItem") retVoid []

-- | Instructs the panel to enter full screen mode.
--
-- If the panel isn’t onscreen, the panel goes directly to full screen mode.
--
-- The panel chooses the appropriate screen depending on where the panel is or, if entering fullscreen directly, where the panel zooms from.
--
-- - Parameters:   - screen: This parameter isn’t currently used—pass @nil@.
--
-- - options: This parameter isn’t currently used—pass @nil@.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the panel was able to enter full screen mode; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- enterFullScreenMode:withOptions:@
enterFullScreenMode_withOptions :: (IsQLPreviewPanel qlPreviewPanel, IsNSScreen screen, IsNSDictionary options) => qlPreviewPanel -> screen -> options -> IO Bool
enterFullScreenMode_withOptions qlPreviewPanel  screen options =
  withObjCPtr screen $ \raw_screen ->
    withObjCPtr options $ \raw_options ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg qlPreviewPanel (mkSelector "enterFullScreenMode:withOptions:") retCULong [argPtr (castPtr raw_screen :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | Instructs the panel to exit full screen mode.
--
-- - Parameters:   - options: This parameter isn’t used — pass @nil@.
--
-- ObjC selector: @- exitFullScreenModeWithOptions:@
exitFullScreenModeWithOptions :: (IsQLPreviewPanel qlPreviewPanel, IsNSDictionary options) => qlPreviewPanel -> options -> IO ()
exitFullScreenModeWithOptions qlPreviewPanel  options =
  withObjCPtr options $ \raw_options ->
      sendMsg qlPreviewPanel (mkSelector "exitFullScreenModeWithOptions:") retVoid [argPtr (castPtr raw_options :: Ptr ())]

-- | The current first responder accepting to control the preview panel.
--
-- You should never change the preview panel’s state (for example, its delegate, datasource, and so on) if you aren’t controlling it.
--
-- ObjC selector: @- currentController@
currentController :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO RawId
currentController qlPreviewPanel  =
    fmap (RawId . castPtr) $ sendMsg qlPreviewPanel (mkSelector "currentController") (retPtr retVoid) []

-- | The preview panel data source.
--
-- ObjC selector: @- dataSource@
dataSource :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO RawId
dataSource qlPreviewPanel  =
    fmap (RawId . castPtr) $ sendMsg qlPreviewPanel (mkSelector "dataSource") (retPtr retVoid) []

-- | The preview panel data source.
--
-- ObjC selector: @- setDataSource:@
setDataSource :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> RawId -> IO ()
setDataSource qlPreviewPanel  value =
    sendMsg qlPreviewPanel (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The index of the current preview item.
--
-- The value is @NSNotFound@ if there’s no current preview item.
--
-- ObjC selector: @- currentPreviewItemIndex@
currentPreviewItemIndex :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO CLong
currentPreviewItemIndex qlPreviewPanel  =
    sendMsg qlPreviewPanel (mkSelector "currentPreviewItemIndex") retCLong []

-- | The index of the current preview item.
--
-- The value is @NSNotFound@ if there’s no current preview item.
--
-- ObjC selector: @- setCurrentPreviewItemIndex:@
setCurrentPreviewItemIndex :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> CLong -> IO ()
setCurrentPreviewItemIndex qlPreviewPanel  value =
    sendMsg qlPreviewPanel (mkSelector "setCurrentPreviewItemIndex:") retVoid [argCLong value]

-- | The currently previewed item.
--
-- The value is @nil@ if there’s no current preview item.
--
-- ObjC selector: @- currentPreviewItem@
currentPreviewItem :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO RawId
currentPreviewItem qlPreviewPanel  =
    fmap (RawId . castPtr) $ sendMsg qlPreviewPanel (mkSelector "currentPreviewItem") (retPtr retVoid) []

-- | The preview panel’s display state.
--
-- This property is an opaque object that Quick Look uses to get and set the current display state of the preview. The display state could be, for example, the currently displayed page, the zoom factor on an image, or the position in a movie.
--
-- You can use this property to get and save the current display state of the preview before switching to another. This saving allows you to restore a preview later on when the user switches back to it.
--
-- ObjC selector: @- displayState@
displayState :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO RawId
displayState qlPreviewPanel  =
    fmap (RawId . castPtr) $ sendMsg qlPreviewPanel (mkSelector "displayState") (retPtr retVoid) []

-- | The preview panel’s display state.
--
-- This property is an opaque object that Quick Look uses to get and set the current display state of the preview. The display state could be, for example, the currently displayed page, the zoom factor on an image, or the position in a movie.
--
-- You can use this property to get and save the current display state of the preview before switching to another. This saving allows you to restore a preview later on when the user switches back to it.
--
-- ObjC selector: @- setDisplayState:@
setDisplayState :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> RawId -> IO ()
setDisplayState qlPreviewPanel  value =
    sendMsg qlPreviewPanel (mkSelector "setDisplayState:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The delegate object that controls the preview panel’s behavior.
--
-- The class assigned as the delegate should conform to the ``QuickLookUI/QLPreviewPanelDelegate`` protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO RawId
delegate qlPreviewPanel  =
    fmap (RawId . castPtr) $ sendMsg qlPreviewPanel (mkSelector "delegate") (retPtr retVoid) []

-- | The delegate object that controls the preview panel’s behavior.
--
-- The class assigned as the delegate should conform to the ``QuickLookUI/QLPreviewPanelDelegate`` protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> RawId -> IO ()
setDelegate qlPreviewPanel  value =
    sendMsg qlPreviewPanel (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The property that indicates whether the panel is in full screen mode.
--
-- The value is <doc://com.apple.documentation/documentation/objectivec/yes> if the panel is currently open and in full screen mode; otherwise it’s <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- inFullScreenMode@
inFullScreenMode :: IsQLPreviewPanel qlPreviewPanel => qlPreviewPanel -> IO Bool
inFullScreenMode qlPreviewPanel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qlPreviewPanel (mkSelector "inFullScreenMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedPreviewPanel@
sharedPreviewPanelSelector :: Selector
sharedPreviewPanelSelector = mkSelector "sharedPreviewPanel"

-- | @Selector@ for @sharedPreviewPanelExists@
sharedPreviewPanelExistsSelector :: Selector
sharedPreviewPanelExistsSelector = mkSelector "sharedPreviewPanelExists"

-- | @Selector@ for @updateController@
updateControllerSelector :: Selector
updateControllerSelector = mkSelector "updateController"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @refreshCurrentPreviewItem@
refreshCurrentPreviewItemSelector :: Selector
refreshCurrentPreviewItemSelector = mkSelector "refreshCurrentPreviewItem"

-- | @Selector@ for @enterFullScreenMode:withOptions:@
enterFullScreenMode_withOptionsSelector :: Selector
enterFullScreenMode_withOptionsSelector = mkSelector "enterFullScreenMode:withOptions:"

-- | @Selector@ for @exitFullScreenModeWithOptions:@
exitFullScreenModeWithOptionsSelector :: Selector
exitFullScreenModeWithOptionsSelector = mkSelector "exitFullScreenModeWithOptions:"

-- | @Selector@ for @currentController@
currentControllerSelector :: Selector
currentControllerSelector = mkSelector "currentController"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @currentPreviewItemIndex@
currentPreviewItemIndexSelector :: Selector
currentPreviewItemIndexSelector = mkSelector "currentPreviewItemIndex"

-- | @Selector@ for @setCurrentPreviewItemIndex:@
setCurrentPreviewItemIndexSelector :: Selector
setCurrentPreviewItemIndexSelector = mkSelector "setCurrentPreviewItemIndex:"

-- | @Selector@ for @currentPreviewItem@
currentPreviewItemSelector :: Selector
currentPreviewItemSelector = mkSelector "currentPreviewItem"

-- | @Selector@ for @displayState@
displayStateSelector :: Selector
displayStateSelector = mkSelector "displayState"

-- | @Selector@ for @setDisplayState:@
setDisplayStateSelector :: Selector
setDisplayStateSelector = mkSelector "setDisplayState:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @inFullScreenMode@
inFullScreenModeSelector :: Selector
inFullScreenModeSelector = mkSelector "inFullScreenMode"

