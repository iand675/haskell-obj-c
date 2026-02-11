{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKCameraDeviceView
--
-- IKCameraDeviceView displays content of a Image Capture supported camera.
--
-- Generated bindings for @IKCameraDeviceView@.
module ObjC.Quartz.IKCameraDeviceView
  ( IKCameraDeviceView
  , IsIKCameraDeviceView(..)
  , selectedIndexes
  , selectIndexes_byExtendingSelection
  , rotateLeft
  , rotateRight
  , deleteSelectedItems
  , downloadSelectedItems
  , downloadAllItems
  , setCustomIconSizeSlider
  , setCustomModeControl
  , setCustomActionControl
  , setCustomRotateControl
  , setCustomDeleteControl
  , setShowStatusInfoAsWindowSubtitle
  , delegate
  , setDelegate
  , mode
  , setMode
  , hasDisplayModeTable
  , setHasDisplayModeTable
  , hasDisplayModeIcon
  , setHasDisplayModeIcon
  , downloadAllControlLabel
  , setDownloadAllControlLabel
  , downloadSelectedControlLabel
  , setDownloadSelectedControlLabel
  , iconSize
  , setIconSize
  , transferMode
  , setTransferMode
  , displaysDownloadsDirectoryControl
  , setDisplaysDownloadsDirectoryControl
  , downloadsDirectory
  , setDownloadsDirectory
  , displaysPostProcessApplicationControl
  , setDisplaysPostProcessApplicationControl
  , postProcessApplication
  , setPostProcessApplication
  , canRotateSelectedItemsLeft
  , canRotateSelectedItemsRight
  , canDeleteSelectedItems
  , canDownloadSelectedItems
  , selectedIndexesSelector
  , selectIndexes_byExtendingSelectionSelector
  , rotateLeftSelector
  , rotateRightSelector
  , deleteSelectedItemsSelector
  , downloadSelectedItemsSelector
  , downloadAllItemsSelector
  , setCustomIconSizeSliderSelector
  , setCustomModeControlSelector
  , setCustomActionControlSelector
  , setCustomRotateControlSelector
  , setCustomDeleteControlSelector
  , setShowStatusInfoAsWindowSubtitleSelector
  , delegateSelector
  , setDelegateSelector
  , modeSelector
  , setModeSelector
  , hasDisplayModeTableSelector
  , setHasDisplayModeTableSelector
  , hasDisplayModeIconSelector
  , setHasDisplayModeIconSelector
  , downloadAllControlLabelSelector
  , setDownloadAllControlLabelSelector
  , downloadSelectedControlLabelSelector
  , setDownloadSelectedControlLabelSelector
  , iconSizeSelector
  , setIconSizeSelector
  , transferModeSelector
  , setTransferModeSelector
  , displaysDownloadsDirectoryControlSelector
  , setDisplaysDownloadsDirectoryControlSelector
  , downloadsDirectorySelector
  , setDownloadsDirectorySelector
  , displaysPostProcessApplicationControlSelector
  , setDisplaysPostProcessApplicationControlSelector
  , postProcessApplicationSelector
  , setPostProcessApplicationSelector
  , canRotateSelectedItemsLeftSelector
  , canRotateSelectedItemsRightSelector
  , canDeleteSelectedItemsSelector
  , canDownloadSelectedItemsSelector

  -- * Enum types
  , IKCameraDeviceViewDisplayMode(IKCameraDeviceViewDisplayMode)
  , pattern IKCameraDeviceViewDisplayModeNone
  , pattern IKCameraDeviceViewDisplayModeTable
  , pattern IKCameraDeviceViewDisplayModeIcon
  , IKCameraDeviceViewTransferMode(IKCameraDeviceViewTransferMode)
  , pattern IKCameraDeviceViewTransferModeFileBased
  , pattern IKCameraDeviceViewTransferModeMemoryBased

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

import ObjC.Quartz.Internal.Classes
import ObjC.Quartz.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | selectedIndexes
--
-- current user selection.
--
-- ObjC selector: @- selectedIndexes@
selectedIndexes :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSIndexSet)
selectedIndexes ikCameraDeviceView  =
    sendMsg ikCameraDeviceView (mkSelector "selectedIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | selectIndexes:byExtendingSelection:
--
-- setting current user selection.
--
-- ObjC selector: @- selectIndexes:byExtendingSelection:@
selectIndexes_byExtendingSelection :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSIndexSet indexes) => ikCameraDeviceView -> indexes -> Bool -> IO ()
selectIndexes_byExtendingSelection ikCameraDeviceView  indexes extend =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg ikCameraDeviceView (mkSelector "selectIndexes:byExtendingSelection:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (if extend then 1 else 0)]

-- | rotateLeft:
--
-- rotate selected items left.
--
-- ObjC selector: @- rotateLeft:@
rotateLeft :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
rotateLeft ikCameraDeviceView  sender =
    sendMsg ikCameraDeviceView (mkSelector "rotateLeft:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | rotateRight:
--
-- rotate selected items right.
--
-- ObjC selector: @- rotateRight:@
rotateRight :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
rotateRight ikCameraDeviceView  sender =
    sendMsg ikCameraDeviceView (mkSelector "rotateRight:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | deleteSelectedItems:
--
-- delete selected items.
--
-- ObjC selector: @- deleteSelectedItems:@
deleteSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
deleteSelectedItems ikCameraDeviceView  sender =
    sendMsg ikCameraDeviceView (mkSelector "deleteSelectedItems:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | downloadSelectedItems:
--
-- download selected items.
--
-- ObjC selector: @- downloadSelectedItems:@
downloadSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
downloadSelectedItems ikCameraDeviceView  sender =
    sendMsg ikCameraDeviceView (mkSelector "downloadSelectedItems:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | downloadAllItems:
--
-- download all items.
--
-- ObjC selector: @- downloadAllItems:@
downloadAllItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
downloadAllItems ikCameraDeviceView  sender =
    sendMsg ikCameraDeviceView (mkSelector "downloadAllItems:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | setCustomIconSizeSlider:
--
-- provide your own NSSlider to resize item thumbnails
--
-- ObjC selector: @- setCustomIconSizeSlider:@
setCustomIconSizeSlider :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSlider slider) => ikCameraDeviceView -> slider -> IO ()
setCustomIconSizeSlider ikCameraDeviceView  slider =
  withObjCPtr slider $ \raw_slider ->
      sendMsg ikCameraDeviceView (mkSelector "setCustomIconSizeSlider:") retVoid [argPtr (castPtr raw_slider :: Ptr ())]

-- | setCustomModeControl:
--
-- provide your own control to toggle between IKCameraDeviceViewDisplayMode table / icon
--
-- ObjC selector: @- setCustomModeControl:@
setCustomModeControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomModeControl ikCameraDeviceView  control =
  withObjCPtr control $ \raw_control ->
      sendMsg ikCameraDeviceView (mkSelector "setCustomModeControl:") retVoid [argPtr (castPtr raw_control :: Ptr ())]

-- | setCustomActionButton:
--
-- provide your own control to toggle between IKCameraDeviceViewDisplayMode table / icon
--
-- ObjC selector: @- setCustomActionControl:@
setCustomActionControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomActionControl ikCameraDeviceView  control =
  withObjCPtr control $ \raw_control ->
      sendMsg ikCameraDeviceView (mkSelector "setCustomActionControl:") retVoid [argPtr (castPtr raw_control :: Ptr ())]

-- | setCustomRotateButton:
--
-- provide your own control to rotate items (multiple of 90º)
--
-- ObjC selector: @- setCustomRotateControl:@
setCustomRotateControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomRotateControl ikCameraDeviceView  control =
  withObjCPtr control $ \raw_control ->
      sendMsg ikCameraDeviceView (mkSelector "setCustomRotateControl:") retVoid [argPtr (castPtr raw_control :: Ptr ())]

-- | setCustomDeleteControl:
--
-- provide your own control to delete selected items
--
-- ObjC selector: @- setCustomDeleteControl:@
setCustomDeleteControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomDeleteControl ikCameraDeviceView  control =
  withObjCPtr control $ \raw_control ->
      sendMsg ikCameraDeviceView (mkSelector "setCustomDeleteControl:") retVoid [argPtr (castPtr raw_control :: Ptr ())]

-- | setShowStatusInfoAsWindowSubtitle:
--
-- display status info as window subtitle
--
-- ObjC selector: @- setShowStatusInfoAsWindowSubtitle:@
setShowStatusInfoAsWindowSubtitle :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setShowStatusInfoAsWindowSubtitle ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setShowStatusInfoAsWindowSubtitle:") retVoid [argCULong (if value then 1 else 0)]

-- | delegate
--
-- Delegate of the IKCameraDeviceView.
--
-- ObjC selector: @- delegate@
delegate :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO RawId
delegate ikCameraDeviceView  =
    fmap (RawId . castPtr) $ sendMsg ikCameraDeviceView (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- Delegate of the IKCameraDeviceView.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
setDelegate ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- mode@
mode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO IKCameraDeviceViewDisplayMode
mode ikCameraDeviceView  =
    fmap (coerce :: CLong -> IKCameraDeviceViewDisplayMode) $ sendMsg ikCameraDeviceView (mkSelector "mode") retCLong []

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- setMode:@
setMode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IKCameraDeviceViewDisplayMode -> IO ()
setMode ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- | hasDisplayModeTable
--
-- support table view display mode.
--
-- ObjC selector: @- hasDisplayModeTable@
hasDisplayModeTable :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
hasDisplayModeTable ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "hasDisplayModeTable") retCULong []

-- | hasDisplayModeTable
--
-- support table view display mode.
--
-- ObjC selector: @- setHasDisplayModeTable:@
setHasDisplayModeTable :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setHasDisplayModeTable ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setHasDisplayModeTable:") retVoid [argCULong (if value then 1 else 0)]

-- | hasDisplayModeIcon
--
-- support icon view display mode.
--
-- ObjC selector: @- hasDisplayModeIcon@
hasDisplayModeIcon :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
hasDisplayModeIcon ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "hasDisplayModeIcon") retCULong []

-- | hasDisplayModeIcon
--
-- support icon view display mode.
--
-- ObjC selector: @- setHasDisplayModeIcon:@
setHasDisplayModeIcon :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setHasDisplayModeIcon ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setHasDisplayModeIcon:") retVoid [argCULong (if value then 1 else 0)]

-- | downloadAllControlLabel
--
-- label for the 'Download All' control - allows for example renaming to 'Import All'.
--
-- ObjC selector: @- downloadAllControlLabel@
downloadAllControlLabel :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSString)
downloadAllControlLabel ikCameraDeviceView  =
    sendMsg ikCameraDeviceView (mkSelector "downloadAllControlLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | downloadAllControlLabel
--
-- label for the 'Download All' control - allows for example renaming to 'Import All'.
--
-- ObjC selector: @- setDownloadAllControlLabel:@
setDownloadAllControlLabel :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSString value) => ikCameraDeviceView -> value -> IO ()
setDownloadAllControlLabel ikCameraDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikCameraDeviceView (mkSelector "setDownloadAllControlLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | downloadSelectedControlLabel
--
-- label for the 'Download Selected' control.
--
-- ObjC selector: @- downloadSelectedControlLabel@
downloadSelectedControlLabel :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSString)
downloadSelectedControlLabel ikCameraDeviceView  =
    sendMsg ikCameraDeviceView (mkSelector "downloadSelectedControlLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | downloadSelectedControlLabel
--
-- label for the 'Download Selected' control.
--
-- ObjC selector: @- setDownloadSelectedControlLabel:@
setDownloadSelectedControlLabel :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSString value) => ikCameraDeviceView -> value -> IO ()
setDownloadSelectedControlLabel ikCameraDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikCameraDeviceView (mkSelector "setDownloadSelectedControlLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | iconSize
--
-- in icon mode: size of the image thumbnails.
--
-- ObjC selector: @- iconSize@
iconSize :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO CULong
iconSize ikCameraDeviceView  =
    sendMsg ikCameraDeviceView (mkSelector "iconSize") retCULong []

-- | iconSize
--
-- in icon mode: size of the image thumbnails.
--
-- ObjC selector: @- setIconSize:@
setIconSize :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> CULong -> IO ()
setIconSize ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setIconSize:") retVoid [argCULong value]

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- transferMode@
transferMode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO IKCameraDeviceViewTransferMode
transferMode ikCameraDeviceView  =
    fmap (coerce :: CLong -> IKCameraDeviceViewTransferMode) $ sendMsg ikCameraDeviceView (mkSelector "transferMode") retCLong []

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- setTransferMode:@
setTransferMode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IKCameraDeviceViewTransferMode -> IO ()
setTransferMode ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setTransferMode:") retVoid [argCLong (coerce value)]

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- displaysDownloadsDirectoryControl@
displaysDownloadsDirectoryControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
displaysDownloadsDirectoryControl ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "displaysDownloadsDirectoryControl") retCULong []

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- setDisplaysDownloadsDirectoryControl:@
setDisplaysDownloadsDirectoryControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setDisplaysDownloadsDirectoryControl ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setDisplaysDownloadsDirectoryControl:") retVoid [argCULong (if value then 1 else 0)]

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- downloadsDirectory@
downloadsDirectory :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSURL)
downloadsDirectory ikCameraDeviceView  =
    sendMsg ikCameraDeviceView (mkSelector "downloadsDirectory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- setDownloadsDirectory:@
setDownloadsDirectory :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSURL value) => ikCameraDeviceView -> value -> IO ()
setDownloadsDirectory ikCameraDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikCameraDeviceView (mkSelector "setDownloadsDirectory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- displaysPostProcessApplicationControl@
displaysPostProcessApplicationControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
displaysPostProcessApplicationControl ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "displaysPostProcessApplicationControl") retCULong []

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- setDisplaysPostProcessApplicationControl:@
setDisplaysPostProcessApplicationControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setDisplaysPostProcessApplicationControl ikCameraDeviceView  value =
    sendMsg ikCameraDeviceView (mkSelector "setDisplaysPostProcessApplicationControl:") retVoid [argCULong (if value then 1 else 0)]

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- postProcessApplication@
postProcessApplication :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSURL)
postProcessApplication ikCameraDeviceView  =
    sendMsg ikCameraDeviceView (mkSelector "postProcessApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- setPostProcessApplication:@
setPostProcessApplication :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSURL value) => ikCameraDeviceView -> value -> IO ()
setPostProcessApplication ikCameraDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikCameraDeviceView (mkSelector "setPostProcessApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | canRotateSelectedItemsLeft
--
-- indicates if the user selected items can be rotated left.
--
-- ObjC selector: @- canRotateSelectedItemsLeft@
canRotateSelectedItemsLeft :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canRotateSelectedItemsLeft ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "canRotateSelectedItemsLeft") retCULong []

-- | canRotateSelectedItemsRight
--
-- indicates if the user selected items can be rotated right.
--
-- ObjC selector: @- canRotateSelectedItemsRight@
canRotateSelectedItemsRight :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canRotateSelectedItemsRight ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "canRotateSelectedItemsRight") retCULong []

-- | canDeleteSelectedItems
--
-- indicates if the user selected items can be deleted.
--
-- ObjC selector: @- canDeleteSelectedItems@
canDeleteSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canDeleteSelectedItems ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "canDeleteSelectedItems") retCULong []

-- | canDownloadSelectedItems
--
-- indicates if the user selected items can be downloaded.
--
-- ObjC selector: @- canDownloadSelectedItems@
canDownloadSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canDownloadSelectedItems ikCameraDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikCameraDeviceView (mkSelector "canDownloadSelectedItems") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectedIndexes@
selectedIndexesSelector :: Selector
selectedIndexesSelector = mkSelector "selectedIndexes"

-- | @Selector@ for @selectIndexes:byExtendingSelection:@
selectIndexes_byExtendingSelectionSelector :: Selector
selectIndexes_byExtendingSelectionSelector = mkSelector "selectIndexes:byExtendingSelection:"

-- | @Selector@ for @rotateLeft:@
rotateLeftSelector :: Selector
rotateLeftSelector = mkSelector "rotateLeft:"

-- | @Selector@ for @rotateRight:@
rotateRightSelector :: Selector
rotateRightSelector = mkSelector "rotateRight:"

-- | @Selector@ for @deleteSelectedItems:@
deleteSelectedItemsSelector :: Selector
deleteSelectedItemsSelector = mkSelector "deleteSelectedItems:"

-- | @Selector@ for @downloadSelectedItems:@
downloadSelectedItemsSelector :: Selector
downloadSelectedItemsSelector = mkSelector "downloadSelectedItems:"

-- | @Selector@ for @downloadAllItems:@
downloadAllItemsSelector :: Selector
downloadAllItemsSelector = mkSelector "downloadAllItems:"

-- | @Selector@ for @setCustomIconSizeSlider:@
setCustomIconSizeSliderSelector :: Selector
setCustomIconSizeSliderSelector = mkSelector "setCustomIconSizeSlider:"

-- | @Selector@ for @setCustomModeControl:@
setCustomModeControlSelector :: Selector
setCustomModeControlSelector = mkSelector "setCustomModeControl:"

-- | @Selector@ for @setCustomActionControl:@
setCustomActionControlSelector :: Selector
setCustomActionControlSelector = mkSelector "setCustomActionControl:"

-- | @Selector@ for @setCustomRotateControl:@
setCustomRotateControlSelector :: Selector
setCustomRotateControlSelector = mkSelector "setCustomRotateControl:"

-- | @Selector@ for @setCustomDeleteControl:@
setCustomDeleteControlSelector :: Selector
setCustomDeleteControlSelector = mkSelector "setCustomDeleteControl:"

-- | @Selector@ for @setShowStatusInfoAsWindowSubtitle:@
setShowStatusInfoAsWindowSubtitleSelector :: Selector
setShowStatusInfoAsWindowSubtitleSelector = mkSelector "setShowStatusInfoAsWindowSubtitle:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @hasDisplayModeTable@
hasDisplayModeTableSelector :: Selector
hasDisplayModeTableSelector = mkSelector "hasDisplayModeTable"

-- | @Selector@ for @setHasDisplayModeTable:@
setHasDisplayModeTableSelector :: Selector
setHasDisplayModeTableSelector = mkSelector "setHasDisplayModeTable:"

-- | @Selector@ for @hasDisplayModeIcon@
hasDisplayModeIconSelector :: Selector
hasDisplayModeIconSelector = mkSelector "hasDisplayModeIcon"

-- | @Selector@ for @setHasDisplayModeIcon:@
setHasDisplayModeIconSelector :: Selector
setHasDisplayModeIconSelector = mkSelector "setHasDisplayModeIcon:"

-- | @Selector@ for @downloadAllControlLabel@
downloadAllControlLabelSelector :: Selector
downloadAllControlLabelSelector = mkSelector "downloadAllControlLabel"

-- | @Selector@ for @setDownloadAllControlLabel:@
setDownloadAllControlLabelSelector :: Selector
setDownloadAllControlLabelSelector = mkSelector "setDownloadAllControlLabel:"

-- | @Selector@ for @downloadSelectedControlLabel@
downloadSelectedControlLabelSelector :: Selector
downloadSelectedControlLabelSelector = mkSelector "downloadSelectedControlLabel"

-- | @Selector@ for @setDownloadSelectedControlLabel:@
setDownloadSelectedControlLabelSelector :: Selector
setDownloadSelectedControlLabelSelector = mkSelector "setDownloadSelectedControlLabel:"

-- | @Selector@ for @iconSize@
iconSizeSelector :: Selector
iconSizeSelector = mkSelector "iconSize"

-- | @Selector@ for @setIconSize:@
setIconSizeSelector :: Selector
setIconSizeSelector = mkSelector "setIconSize:"

-- | @Selector@ for @transferMode@
transferModeSelector :: Selector
transferModeSelector = mkSelector "transferMode"

-- | @Selector@ for @setTransferMode:@
setTransferModeSelector :: Selector
setTransferModeSelector = mkSelector "setTransferMode:"

-- | @Selector@ for @displaysDownloadsDirectoryControl@
displaysDownloadsDirectoryControlSelector :: Selector
displaysDownloadsDirectoryControlSelector = mkSelector "displaysDownloadsDirectoryControl"

-- | @Selector@ for @setDisplaysDownloadsDirectoryControl:@
setDisplaysDownloadsDirectoryControlSelector :: Selector
setDisplaysDownloadsDirectoryControlSelector = mkSelector "setDisplaysDownloadsDirectoryControl:"

-- | @Selector@ for @downloadsDirectory@
downloadsDirectorySelector :: Selector
downloadsDirectorySelector = mkSelector "downloadsDirectory"

-- | @Selector@ for @setDownloadsDirectory:@
setDownloadsDirectorySelector :: Selector
setDownloadsDirectorySelector = mkSelector "setDownloadsDirectory:"

-- | @Selector@ for @displaysPostProcessApplicationControl@
displaysPostProcessApplicationControlSelector :: Selector
displaysPostProcessApplicationControlSelector = mkSelector "displaysPostProcessApplicationControl"

-- | @Selector@ for @setDisplaysPostProcessApplicationControl:@
setDisplaysPostProcessApplicationControlSelector :: Selector
setDisplaysPostProcessApplicationControlSelector = mkSelector "setDisplaysPostProcessApplicationControl:"

-- | @Selector@ for @postProcessApplication@
postProcessApplicationSelector :: Selector
postProcessApplicationSelector = mkSelector "postProcessApplication"

-- | @Selector@ for @setPostProcessApplication:@
setPostProcessApplicationSelector :: Selector
setPostProcessApplicationSelector = mkSelector "setPostProcessApplication:"

-- | @Selector@ for @canRotateSelectedItemsLeft@
canRotateSelectedItemsLeftSelector :: Selector
canRotateSelectedItemsLeftSelector = mkSelector "canRotateSelectedItemsLeft"

-- | @Selector@ for @canRotateSelectedItemsRight@
canRotateSelectedItemsRightSelector :: Selector
canRotateSelectedItemsRightSelector = mkSelector "canRotateSelectedItemsRight"

-- | @Selector@ for @canDeleteSelectedItems@
canDeleteSelectedItemsSelector :: Selector
canDeleteSelectedItemsSelector = mkSelector "canDeleteSelectedItems"

-- | @Selector@ for @canDownloadSelectedItems@
canDownloadSelectedItemsSelector :: Selector
canDownloadSelectedItemsSelector = mkSelector "canDownloadSelectedItems"

