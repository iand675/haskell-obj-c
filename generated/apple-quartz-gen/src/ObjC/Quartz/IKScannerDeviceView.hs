{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKScannerDeviceView
--
-- IKScannerDeviceView displays a UI to work with Image Capture supported scanners.
--
-- Generated bindings for @IKScannerDeviceView@.
module ObjC.Quartz.IKScannerDeviceView
  ( IKScannerDeviceView
  , IsIKScannerDeviceView(..)
  , delegate
  , setDelegate
  , mode
  , setMode
  , hasDisplayModeSimple
  , setHasDisplayModeSimple
  , hasDisplayModeAdvanced
  , setHasDisplayModeAdvanced
  , transferMode
  , setTransferMode
  , scanControlLabel
  , setScanControlLabel
  , overviewControlLabel
  , setOverviewControlLabel
  , displaysDownloadsDirectoryControl
  , setDisplaysDownloadsDirectoryControl
  , downloadsDirectory
  , setDownloadsDirectory
  , documentName
  , setDocumentName
  , displaysPostProcessApplicationControl
  , setDisplaysPostProcessApplicationControl
  , postProcessApplication
  , setPostProcessApplication
  , delegateSelector
  , displaysDownloadsDirectoryControlSelector
  , displaysPostProcessApplicationControlSelector
  , documentNameSelector
  , downloadsDirectorySelector
  , hasDisplayModeAdvancedSelector
  , hasDisplayModeSimpleSelector
  , modeSelector
  , overviewControlLabelSelector
  , postProcessApplicationSelector
  , scanControlLabelSelector
  , setDelegateSelector
  , setDisplaysDownloadsDirectoryControlSelector
  , setDisplaysPostProcessApplicationControlSelector
  , setDocumentNameSelector
  , setDownloadsDirectorySelector
  , setHasDisplayModeAdvancedSelector
  , setHasDisplayModeSimpleSelector
  , setModeSelector
  , setOverviewControlLabelSelector
  , setPostProcessApplicationSelector
  , setScanControlLabelSelector
  , setTransferModeSelector
  , transferModeSelector

  -- * Enum types
  , IKScannerDeviceViewDisplayMode(IKScannerDeviceViewDisplayMode)
  , pattern IKScannerDeviceViewDisplayModeNone
  , pattern IKScannerDeviceViewDisplayModeSimple
  , pattern IKScannerDeviceViewDisplayModeAdvanced
  , IKScannerDeviceViewTransferMode(IKScannerDeviceViewTransferMode)
  , pattern IKScannerDeviceViewTransferModeFileBased
  , pattern IKScannerDeviceViewTransferModeMemoryBased

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

-- | delegate
--
-- delegate of the IKScannerDeviceView.
--
-- ObjC selector: @- delegate@
delegate :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO RawId
delegate ikScannerDeviceView =
  sendMessage ikScannerDeviceView delegateSelector

-- | delegate
--
-- delegate of the IKScannerDeviceView.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> RawId -> IO ()
setDelegate ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setDelegateSelector value

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- mode@
mode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO IKScannerDeviceViewDisplayMode
mode ikScannerDeviceView =
  sendMessage ikScannerDeviceView modeSelector

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- setMode:@
setMode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IKScannerDeviceViewDisplayMode -> IO ()
setMode ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setModeSelector value

-- | hasDisplayModeSimple
--
-- support a simple scanning UI.
--
-- ObjC selector: @- hasDisplayModeSimple@
hasDisplayModeSimple :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
hasDisplayModeSimple ikScannerDeviceView =
  sendMessage ikScannerDeviceView hasDisplayModeSimpleSelector

-- | hasDisplayModeSimple
--
-- support a simple scanning UI.
--
-- ObjC selector: @- setHasDisplayModeSimple:@
setHasDisplayModeSimple :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setHasDisplayModeSimple ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setHasDisplayModeSimpleSelector value

-- | hasDisplayModeAdvanced
--
-- support advanced scanning UI.
--
-- ObjC selector: @- hasDisplayModeAdvanced@
hasDisplayModeAdvanced :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
hasDisplayModeAdvanced ikScannerDeviceView =
  sendMessage ikScannerDeviceView hasDisplayModeAdvancedSelector

-- | hasDisplayModeAdvanced
--
-- support advanced scanning UI.
--
-- ObjC selector: @- setHasDisplayModeAdvanced:@
setHasDisplayModeAdvanced :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setHasDisplayModeAdvanced ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setHasDisplayModeAdvancedSelector value

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- transferMode@
transferMode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO IKScannerDeviceViewTransferMode
transferMode ikScannerDeviceView =
  sendMessage ikScannerDeviceView transferModeSelector

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- setTransferMode:@
setTransferMode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IKScannerDeviceViewTransferMode -> IO ()
setTransferMode ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setTransferModeSelector value

-- | scanControlLabel
--
-- label for the 'Scan' control.
--
-- ObjC selector: @- scanControlLabel@
scanControlLabel :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSString)
scanControlLabel ikScannerDeviceView =
  sendMessage ikScannerDeviceView scanControlLabelSelector

-- | scanControlLabel
--
-- label for the 'Scan' control.
--
-- ObjC selector: @- setScanControlLabel:@
setScanControlLabel :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSString value) => ikScannerDeviceView -> value -> IO ()
setScanControlLabel ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setScanControlLabelSelector (toNSString value)

-- | overviewControlLabel
--
-- label for the 'Overview' control.
--
-- ObjC selector: @- overviewControlLabel@
overviewControlLabel :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSString)
overviewControlLabel ikScannerDeviceView =
  sendMessage ikScannerDeviceView overviewControlLabelSelector

-- | overviewControlLabel
--
-- label for the 'Overview' control.
--
-- ObjC selector: @- setOverviewControlLabel:@
setOverviewControlLabel :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSString value) => ikScannerDeviceView -> value -> IO ()
setOverviewControlLabel ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setOverviewControlLabelSelector (toNSString value)

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- displaysDownloadsDirectoryControl@
displaysDownloadsDirectoryControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
displaysDownloadsDirectoryControl ikScannerDeviceView =
  sendMessage ikScannerDeviceView displaysDownloadsDirectoryControlSelector

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- setDisplaysDownloadsDirectoryControl:@
setDisplaysDownloadsDirectoryControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setDisplaysDownloadsDirectoryControl ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setDisplaysDownloadsDirectoryControlSelector value

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- downloadsDirectory@
downloadsDirectory :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSURL)
downloadsDirectory ikScannerDeviceView =
  sendMessage ikScannerDeviceView downloadsDirectorySelector

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- setDownloadsDirectory:@
setDownloadsDirectory :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSURL value) => ikScannerDeviceView -> value -> IO ()
setDownloadsDirectory ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setDownloadsDirectorySelector (toNSURL value)

-- | documentName
--
-- document name.
--
-- ObjC selector: @- documentName@
documentName :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSString)
documentName ikScannerDeviceView =
  sendMessage ikScannerDeviceView documentNameSelector

-- | documentName
--
-- document name.
--
-- ObjC selector: @- setDocumentName:@
setDocumentName :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSString value) => ikScannerDeviceView -> value -> IO ()
setDocumentName ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setDocumentNameSelector (toNSString value)

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- displaysPostProcessApplicationControl@
displaysPostProcessApplicationControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
displaysPostProcessApplicationControl ikScannerDeviceView =
  sendMessage ikScannerDeviceView displaysPostProcessApplicationControlSelector

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- setDisplaysPostProcessApplicationControl:@
setDisplaysPostProcessApplicationControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setDisplaysPostProcessApplicationControl ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setDisplaysPostProcessApplicationControlSelector value

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- postProcessApplication@
postProcessApplication :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSURL)
postProcessApplication ikScannerDeviceView =
  sendMessage ikScannerDeviceView postProcessApplicationSelector

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- setPostProcessApplication:@
setPostProcessApplication :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSURL value) => ikScannerDeviceView -> value -> IO ()
setPostProcessApplication ikScannerDeviceView value =
  sendMessage ikScannerDeviceView setPostProcessApplicationSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] IKScannerDeviceViewDisplayMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[IKScannerDeviceViewDisplayMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @hasDisplayModeSimple@
hasDisplayModeSimpleSelector :: Selector '[] Bool
hasDisplayModeSimpleSelector = mkSelector "hasDisplayModeSimple"

-- | @Selector@ for @setHasDisplayModeSimple:@
setHasDisplayModeSimpleSelector :: Selector '[Bool] ()
setHasDisplayModeSimpleSelector = mkSelector "setHasDisplayModeSimple:"

-- | @Selector@ for @hasDisplayModeAdvanced@
hasDisplayModeAdvancedSelector :: Selector '[] Bool
hasDisplayModeAdvancedSelector = mkSelector "hasDisplayModeAdvanced"

-- | @Selector@ for @setHasDisplayModeAdvanced:@
setHasDisplayModeAdvancedSelector :: Selector '[Bool] ()
setHasDisplayModeAdvancedSelector = mkSelector "setHasDisplayModeAdvanced:"

-- | @Selector@ for @transferMode@
transferModeSelector :: Selector '[] IKScannerDeviceViewTransferMode
transferModeSelector = mkSelector "transferMode"

-- | @Selector@ for @setTransferMode:@
setTransferModeSelector :: Selector '[IKScannerDeviceViewTransferMode] ()
setTransferModeSelector = mkSelector "setTransferMode:"

-- | @Selector@ for @scanControlLabel@
scanControlLabelSelector :: Selector '[] (Id NSString)
scanControlLabelSelector = mkSelector "scanControlLabel"

-- | @Selector@ for @setScanControlLabel:@
setScanControlLabelSelector :: Selector '[Id NSString] ()
setScanControlLabelSelector = mkSelector "setScanControlLabel:"

-- | @Selector@ for @overviewControlLabel@
overviewControlLabelSelector :: Selector '[] (Id NSString)
overviewControlLabelSelector = mkSelector "overviewControlLabel"

-- | @Selector@ for @setOverviewControlLabel:@
setOverviewControlLabelSelector :: Selector '[Id NSString] ()
setOverviewControlLabelSelector = mkSelector "setOverviewControlLabel:"

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

-- | @Selector@ for @documentName@
documentNameSelector :: Selector '[] (Id NSString)
documentNameSelector = mkSelector "documentName"

-- | @Selector@ for @setDocumentName:@
setDocumentNameSelector :: Selector '[Id NSString] ()
setDocumentNameSelector = mkSelector "setDocumentName:"

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

