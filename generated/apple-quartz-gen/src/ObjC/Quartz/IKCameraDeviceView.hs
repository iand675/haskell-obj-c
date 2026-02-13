{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , canDeleteSelectedItemsSelector
  , canDownloadSelectedItemsSelector
  , canRotateSelectedItemsLeftSelector
  , canRotateSelectedItemsRightSelector
  , delegateSelector
  , deleteSelectedItemsSelector
  , displaysDownloadsDirectoryControlSelector
  , displaysPostProcessApplicationControlSelector
  , downloadAllControlLabelSelector
  , downloadAllItemsSelector
  , downloadSelectedControlLabelSelector
  , downloadSelectedItemsSelector
  , downloadsDirectorySelector
  , hasDisplayModeIconSelector
  , hasDisplayModeTableSelector
  , iconSizeSelector
  , modeSelector
  , postProcessApplicationSelector
  , rotateLeftSelector
  , rotateRightSelector
  , selectIndexes_byExtendingSelectionSelector
  , selectedIndexesSelector
  , setCustomActionControlSelector
  , setCustomDeleteControlSelector
  , setCustomIconSizeSliderSelector
  , setCustomModeControlSelector
  , setCustomRotateControlSelector
  , setDelegateSelector
  , setDisplaysDownloadsDirectoryControlSelector
  , setDisplaysPostProcessApplicationControlSelector
  , setDownloadAllControlLabelSelector
  , setDownloadSelectedControlLabelSelector
  , setDownloadsDirectorySelector
  , setHasDisplayModeIconSelector
  , setHasDisplayModeTableSelector
  , setIconSizeSelector
  , setModeSelector
  , setPostProcessApplicationSelector
  , setShowStatusInfoAsWindowSubtitleSelector
  , setTransferModeSelector
  , transferModeSelector

  -- * Enum types
  , IKCameraDeviceViewDisplayMode(IKCameraDeviceViewDisplayMode)
  , pattern IKCameraDeviceViewDisplayModeNone
  , pattern IKCameraDeviceViewDisplayModeTable
  , pattern IKCameraDeviceViewDisplayModeIcon
  , IKCameraDeviceViewTransferMode(IKCameraDeviceViewTransferMode)
  , pattern IKCameraDeviceViewTransferModeFileBased
  , pattern IKCameraDeviceViewTransferModeMemoryBased

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
selectedIndexes ikCameraDeviceView =
  sendMessage ikCameraDeviceView selectedIndexesSelector

-- | selectIndexes:byExtendingSelection:
--
-- setting current user selection.
--
-- ObjC selector: @- selectIndexes:byExtendingSelection:@
selectIndexes_byExtendingSelection :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSIndexSet indexes) => ikCameraDeviceView -> indexes -> Bool -> IO ()
selectIndexes_byExtendingSelection ikCameraDeviceView indexes extend =
  sendMessage ikCameraDeviceView selectIndexes_byExtendingSelectionSelector (toNSIndexSet indexes) extend

-- | rotateLeft:
--
-- rotate selected items left.
--
-- ObjC selector: @- rotateLeft:@
rotateLeft :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
rotateLeft ikCameraDeviceView sender =
  sendMessage ikCameraDeviceView rotateLeftSelector sender

-- | rotateRight:
--
-- rotate selected items right.
--
-- ObjC selector: @- rotateRight:@
rotateRight :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
rotateRight ikCameraDeviceView sender =
  sendMessage ikCameraDeviceView rotateRightSelector sender

-- | deleteSelectedItems:
--
-- delete selected items.
--
-- ObjC selector: @- deleteSelectedItems:@
deleteSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
deleteSelectedItems ikCameraDeviceView sender =
  sendMessage ikCameraDeviceView deleteSelectedItemsSelector sender

-- | downloadSelectedItems:
--
-- download selected items.
--
-- ObjC selector: @- downloadSelectedItems:@
downloadSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
downloadSelectedItems ikCameraDeviceView sender =
  sendMessage ikCameraDeviceView downloadSelectedItemsSelector sender

-- | downloadAllItems:
--
-- download all items.
--
-- ObjC selector: @- downloadAllItems:@
downloadAllItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
downloadAllItems ikCameraDeviceView sender =
  sendMessage ikCameraDeviceView downloadAllItemsSelector sender

-- | setCustomIconSizeSlider:
--
-- provide your own NSSlider to resize item thumbnails
--
-- ObjC selector: @- setCustomIconSizeSlider:@
setCustomIconSizeSlider :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSlider slider) => ikCameraDeviceView -> slider -> IO ()
setCustomIconSizeSlider ikCameraDeviceView slider =
  sendMessage ikCameraDeviceView setCustomIconSizeSliderSelector (toNSSlider slider)

-- | setCustomModeControl:
--
-- provide your own control to toggle between IKCameraDeviceViewDisplayMode table / icon
--
-- ObjC selector: @- setCustomModeControl:@
setCustomModeControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomModeControl ikCameraDeviceView control =
  sendMessage ikCameraDeviceView setCustomModeControlSelector (toNSSegmentedControl control)

-- | setCustomActionButton:
--
-- provide your own control to toggle between IKCameraDeviceViewDisplayMode table / icon
--
-- ObjC selector: @- setCustomActionControl:@
setCustomActionControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomActionControl ikCameraDeviceView control =
  sendMessage ikCameraDeviceView setCustomActionControlSelector (toNSSegmentedControl control)

-- | setCustomRotateButton:
--
-- provide your own control to rotate items (multiple of 90º)
--
-- ObjC selector: @- setCustomRotateControl:@
setCustomRotateControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomRotateControl ikCameraDeviceView control =
  sendMessage ikCameraDeviceView setCustomRotateControlSelector (toNSSegmentedControl control)

-- | setCustomDeleteControl:
--
-- provide your own control to delete selected items
--
-- ObjC selector: @- setCustomDeleteControl:@
setCustomDeleteControl :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSSegmentedControl control) => ikCameraDeviceView -> control -> IO ()
setCustomDeleteControl ikCameraDeviceView control =
  sendMessage ikCameraDeviceView setCustomDeleteControlSelector (toNSSegmentedControl control)

-- | setShowStatusInfoAsWindowSubtitle:
--
-- display status info as window subtitle
--
-- ObjC selector: @- setShowStatusInfoAsWindowSubtitle:@
setShowStatusInfoAsWindowSubtitle :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setShowStatusInfoAsWindowSubtitle ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setShowStatusInfoAsWindowSubtitleSelector value

-- | delegate
--
-- Delegate of the IKCameraDeviceView.
--
-- ObjC selector: @- delegate@
delegate :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO RawId
delegate ikCameraDeviceView =
  sendMessage ikCameraDeviceView delegateSelector

-- | delegate
--
-- Delegate of the IKCameraDeviceView.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> RawId -> IO ()
setDelegate ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setDelegateSelector value

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- mode@
mode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO IKCameraDeviceViewDisplayMode
mode ikCameraDeviceView =
  sendMessage ikCameraDeviceView modeSelector

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- setMode:@
setMode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IKCameraDeviceViewDisplayMode -> IO ()
setMode ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setModeSelector value

-- | hasDisplayModeTable
--
-- support table view display mode.
--
-- ObjC selector: @- hasDisplayModeTable@
hasDisplayModeTable :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
hasDisplayModeTable ikCameraDeviceView =
  sendMessage ikCameraDeviceView hasDisplayModeTableSelector

-- | hasDisplayModeTable
--
-- support table view display mode.
--
-- ObjC selector: @- setHasDisplayModeTable:@
setHasDisplayModeTable :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setHasDisplayModeTable ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setHasDisplayModeTableSelector value

-- | hasDisplayModeIcon
--
-- support icon view display mode.
--
-- ObjC selector: @- hasDisplayModeIcon@
hasDisplayModeIcon :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
hasDisplayModeIcon ikCameraDeviceView =
  sendMessage ikCameraDeviceView hasDisplayModeIconSelector

-- | hasDisplayModeIcon
--
-- support icon view display mode.
--
-- ObjC selector: @- setHasDisplayModeIcon:@
setHasDisplayModeIcon :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setHasDisplayModeIcon ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setHasDisplayModeIconSelector value

-- | downloadAllControlLabel
--
-- label for the 'Download All' control - allows for example renaming to 'Import All'.
--
-- ObjC selector: @- downloadAllControlLabel@
downloadAllControlLabel :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSString)
downloadAllControlLabel ikCameraDeviceView =
  sendMessage ikCameraDeviceView downloadAllControlLabelSelector

-- | downloadAllControlLabel
--
-- label for the 'Download All' control - allows for example renaming to 'Import All'.
--
-- ObjC selector: @- setDownloadAllControlLabel:@
setDownloadAllControlLabel :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSString value) => ikCameraDeviceView -> value -> IO ()
setDownloadAllControlLabel ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setDownloadAllControlLabelSelector (toNSString value)

-- | downloadSelectedControlLabel
--
-- label for the 'Download Selected' control.
--
-- ObjC selector: @- downloadSelectedControlLabel@
downloadSelectedControlLabel :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSString)
downloadSelectedControlLabel ikCameraDeviceView =
  sendMessage ikCameraDeviceView downloadSelectedControlLabelSelector

-- | downloadSelectedControlLabel
--
-- label for the 'Download Selected' control.
--
-- ObjC selector: @- setDownloadSelectedControlLabel:@
setDownloadSelectedControlLabel :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSString value) => ikCameraDeviceView -> value -> IO ()
setDownloadSelectedControlLabel ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setDownloadSelectedControlLabelSelector (toNSString value)

-- | iconSize
--
-- in icon mode: size of the image thumbnails.
--
-- ObjC selector: @- iconSize@
iconSize :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO CULong
iconSize ikCameraDeviceView =
  sendMessage ikCameraDeviceView iconSizeSelector

-- | iconSize
--
-- in icon mode: size of the image thumbnails.
--
-- ObjC selector: @- setIconSize:@
setIconSize :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> CULong -> IO ()
setIconSize ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setIconSizeSelector value

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- transferMode@
transferMode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO IKCameraDeviceViewTransferMode
transferMode ikCameraDeviceView =
  sendMessage ikCameraDeviceView transferModeSelector

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- setTransferMode:@
setTransferMode :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IKCameraDeviceViewTransferMode -> IO ()
setTransferMode ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setTransferModeSelector value

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- displaysDownloadsDirectoryControl@
displaysDownloadsDirectoryControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
displaysDownloadsDirectoryControl ikCameraDeviceView =
  sendMessage ikCameraDeviceView displaysDownloadsDirectoryControlSelector

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- setDisplaysDownloadsDirectoryControl:@
setDisplaysDownloadsDirectoryControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setDisplaysDownloadsDirectoryControl ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setDisplaysDownloadsDirectoryControlSelector value

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- downloadsDirectory@
downloadsDirectory :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSURL)
downloadsDirectory ikCameraDeviceView =
  sendMessage ikCameraDeviceView downloadsDirectorySelector

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- setDownloadsDirectory:@
setDownloadsDirectory :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSURL value) => ikCameraDeviceView -> value -> IO ()
setDownloadsDirectory ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setDownloadsDirectorySelector (toNSURL value)

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- displaysPostProcessApplicationControl@
displaysPostProcessApplicationControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
displaysPostProcessApplicationControl ikCameraDeviceView =
  sendMessage ikCameraDeviceView displaysPostProcessApplicationControlSelector

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- setDisplaysPostProcessApplicationControl:@
setDisplaysPostProcessApplicationControl :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> Bool -> IO ()
setDisplaysPostProcessApplicationControl ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setDisplaysPostProcessApplicationControlSelector value

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- postProcessApplication@
postProcessApplication :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO (Id NSURL)
postProcessApplication ikCameraDeviceView =
  sendMessage ikCameraDeviceView postProcessApplicationSelector

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- setPostProcessApplication:@
setPostProcessApplication :: (IsIKCameraDeviceView ikCameraDeviceView, IsNSURL value) => ikCameraDeviceView -> value -> IO ()
setPostProcessApplication ikCameraDeviceView value =
  sendMessage ikCameraDeviceView setPostProcessApplicationSelector (toNSURL value)

-- | canRotateSelectedItemsLeft
--
-- indicates if the user selected items can be rotated left.
--
-- ObjC selector: @- canRotateSelectedItemsLeft@
canRotateSelectedItemsLeft :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canRotateSelectedItemsLeft ikCameraDeviceView =
  sendMessage ikCameraDeviceView canRotateSelectedItemsLeftSelector

-- | canRotateSelectedItemsRight
--
-- indicates if the user selected items can be rotated right.
--
-- ObjC selector: @- canRotateSelectedItemsRight@
canRotateSelectedItemsRight :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canRotateSelectedItemsRight ikCameraDeviceView =
  sendMessage ikCameraDeviceView canRotateSelectedItemsRightSelector

-- | canDeleteSelectedItems
--
-- indicates if the user selected items can be deleted.
--
-- ObjC selector: @- canDeleteSelectedItems@
canDeleteSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canDeleteSelectedItems ikCameraDeviceView =
  sendMessage ikCameraDeviceView canDeleteSelectedItemsSelector

-- | canDownloadSelectedItems
--
-- indicates if the user selected items can be downloaded.
--
-- ObjC selector: @- canDownloadSelectedItems@
canDownloadSelectedItems :: IsIKCameraDeviceView ikCameraDeviceView => ikCameraDeviceView -> IO Bool
canDownloadSelectedItems ikCameraDeviceView =
  sendMessage ikCameraDeviceView canDownloadSelectedItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectedIndexes@
selectedIndexesSelector :: Selector '[] (Id NSIndexSet)
selectedIndexesSelector = mkSelector "selectedIndexes"

-- | @Selector@ for @selectIndexes:byExtendingSelection:@
selectIndexes_byExtendingSelectionSelector :: Selector '[Id NSIndexSet, Bool] ()
selectIndexes_byExtendingSelectionSelector = mkSelector "selectIndexes:byExtendingSelection:"

-- | @Selector@ for @rotateLeft:@
rotateLeftSelector :: Selector '[RawId] ()
rotateLeftSelector = mkSelector "rotateLeft:"

-- | @Selector@ for @rotateRight:@
rotateRightSelector :: Selector '[RawId] ()
rotateRightSelector = mkSelector "rotateRight:"

-- | @Selector@ for @deleteSelectedItems:@
deleteSelectedItemsSelector :: Selector '[RawId] ()
deleteSelectedItemsSelector = mkSelector "deleteSelectedItems:"

-- | @Selector@ for @downloadSelectedItems:@
downloadSelectedItemsSelector :: Selector '[RawId] ()
downloadSelectedItemsSelector = mkSelector "downloadSelectedItems:"

-- | @Selector@ for @downloadAllItems:@
downloadAllItemsSelector :: Selector '[RawId] ()
downloadAllItemsSelector = mkSelector "downloadAllItems:"

-- | @Selector@ for @setCustomIconSizeSlider:@
setCustomIconSizeSliderSelector :: Selector '[Id NSSlider] ()
setCustomIconSizeSliderSelector = mkSelector "setCustomIconSizeSlider:"

-- | @Selector@ for @setCustomModeControl:@
setCustomModeControlSelector :: Selector '[Id NSSegmentedControl] ()
setCustomModeControlSelector = mkSelector "setCustomModeControl:"

-- | @Selector@ for @setCustomActionControl:@
setCustomActionControlSelector :: Selector '[Id NSSegmentedControl] ()
setCustomActionControlSelector = mkSelector "setCustomActionControl:"

-- | @Selector@ for @setCustomRotateControl:@
setCustomRotateControlSelector :: Selector '[Id NSSegmentedControl] ()
setCustomRotateControlSelector = mkSelector "setCustomRotateControl:"

-- | @Selector@ for @setCustomDeleteControl:@
setCustomDeleteControlSelector :: Selector '[Id NSSegmentedControl] ()
setCustomDeleteControlSelector = mkSelector "setCustomDeleteControl:"

-- | @Selector@ for @setShowStatusInfoAsWindowSubtitle:@
setShowStatusInfoAsWindowSubtitleSelector :: Selector '[Bool] ()
setShowStatusInfoAsWindowSubtitleSelector = mkSelector "setShowStatusInfoAsWindowSubtitle:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] IKCameraDeviceViewDisplayMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[IKCameraDeviceViewDisplayMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @hasDisplayModeTable@
hasDisplayModeTableSelector :: Selector '[] Bool
hasDisplayModeTableSelector = mkSelector "hasDisplayModeTable"

-- | @Selector@ for @setHasDisplayModeTable:@
setHasDisplayModeTableSelector :: Selector '[Bool] ()
setHasDisplayModeTableSelector = mkSelector "setHasDisplayModeTable:"

-- | @Selector@ for @hasDisplayModeIcon@
hasDisplayModeIconSelector :: Selector '[] Bool
hasDisplayModeIconSelector = mkSelector "hasDisplayModeIcon"

-- | @Selector@ for @setHasDisplayModeIcon:@
setHasDisplayModeIconSelector :: Selector '[Bool] ()
setHasDisplayModeIconSelector = mkSelector "setHasDisplayModeIcon:"

-- | @Selector@ for @downloadAllControlLabel@
downloadAllControlLabelSelector :: Selector '[] (Id NSString)
downloadAllControlLabelSelector = mkSelector "downloadAllControlLabel"

-- | @Selector@ for @setDownloadAllControlLabel:@
setDownloadAllControlLabelSelector :: Selector '[Id NSString] ()
setDownloadAllControlLabelSelector = mkSelector "setDownloadAllControlLabel:"

-- | @Selector@ for @downloadSelectedControlLabel@
downloadSelectedControlLabelSelector :: Selector '[] (Id NSString)
downloadSelectedControlLabelSelector = mkSelector "downloadSelectedControlLabel"

-- | @Selector@ for @setDownloadSelectedControlLabel:@
setDownloadSelectedControlLabelSelector :: Selector '[Id NSString] ()
setDownloadSelectedControlLabelSelector = mkSelector "setDownloadSelectedControlLabel:"

-- | @Selector@ for @iconSize@
iconSizeSelector :: Selector '[] CULong
iconSizeSelector = mkSelector "iconSize"

-- | @Selector@ for @setIconSize:@
setIconSizeSelector :: Selector '[CULong] ()
setIconSizeSelector = mkSelector "setIconSize:"

-- | @Selector@ for @transferMode@
transferModeSelector :: Selector '[] IKCameraDeviceViewTransferMode
transferModeSelector = mkSelector "transferMode"

-- | @Selector@ for @setTransferMode:@
setTransferModeSelector :: Selector '[IKCameraDeviceViewTransferMode] ()
setTransferModeSelector = mkSelector "setTransferMode:"

-- | @Selector@ for @displaysDownloadsDirectoryControl@
displaysDownloadsDirectoryControlSelector :: Selector '[] Bool
displaysDownloadsDirectoryControlSelector = mkSelector "displaysDownloadsDirectoryControl"

-- | @Selector@ for @setDisplaysDownloadsDirectoryControl:@
setDisplaysDownloadsDirectoryControlSelector :: Selector '[Bool] ()
setDisplaysDownloadsDirectoryControlSelector = mkSelector "setDisplaysDownloadsDirectoryControl:"

-- | @Selector@ for @downloadsDirectory@
downloadsDirectorySelector :: Selector '[] (Id NSURL)
downloadsDirectorySelector = mkSelector "downloadsDirectory"

-- | @Selector@ for @setDownloadsDirectory:@
setDownloadsDirectorySelector :: Selector '[Id NSURL] ()
setDownloadsDirectorySelector = mkSelector "setDownloadsDirectory:"

-- | @Selector@ for @displaysPostProcessApplicationControl@
displaysPostProcessApplicationControlSelector :: Selector '[] Bool
displaysPostProcessApplicationControlSelector = mkSelector "displaysPostProcessApplicationControl"

-- | @Selector@ for @setDisplaysPostProcessApplicationControl:@
setDisplaysPostProcessApplicationControlSelector :: Selector '[Bool] ()
setDisplaysPostProcessApplicationControlSelector = mkSelector "setDisplaysPostProcessApplicationControl:"

-- | @Selector@ for @postProcessApplication@
postProcessApplicationSelector :: Selector '[] (Id NSURL)
postProcessApplicationSelector = mkSelector "postProcessApplication"

-- | @Selector@ for @setPostProcessApplication:@
setPostProcessApplicationSelector :: Selector '[Id NSURL] ()
setPostProcessApplicationSelector = mkSelector "setPostProcessApplication:"

-- | @Selector@ for @canRotateSelectedItemsLeft@
canRotateSelectedItemsLeftSelector :: Selector '[] Bool
canRotateSelectedItemsLeftSelector = mkSelector "canRotateSelectedItemsLeft"

-- | @Selector@ for @canRotateSelectedItemsRight@
canRotateSelectedItemsRightSelector :: Selector '[] Bool
canRotateSelectedItemsRightSelector = mkSelector "canRotateSelectedItemsRight"

-- | @Selector@ for @canDeleteSelectedItems@
canDeleteSelectedItemsSelector :: Selector '[] Bool
canDeleteSelectedItemsSelector = mkSelector "canDeleteSelectedItems"

-- | @Selector@ for @canDownloadSelectedItems@
canDownloadSelectedItemsSelector :: Selector '[] Bool
canDownloadSelectedItemsSelector = mkSelector "canDownloadSelectedItems"

