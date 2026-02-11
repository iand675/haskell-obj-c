{-# LANGUAGE PatternSynonyms #-}
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
  , setDelegateSelector
  , modeSelector
  , setModeSelector
  , hasDisplayModeSimpleSelector
  , setHasDisplayModeSimpleSelector
  , hasDisplayModeAdvancedSelector
  , setHasDisplayModeAdvancedSelector
  , transferModeSelector
  , setTransferModeSelector
  , scanControlLabelSelector
  , setScanControlLabelSelector
  , overviewControlLabelSelector
  , setOverviewControlLabelSelector
  , displaysDownloadsDirectoryControlSelector
  , setDisplaysDownloadsDirectoryControlSelector
  , downloadsDirectorySelector
  , setDownloadsDirectorySelector
  , documentNameSelector
  , setDocumentNameSelector
  , displaysPostProcessApplicationControlSelector
  , setDisplaysPostProcessApplicationControlSelector
  , postProcessApplicationSelector
  , setPostProcessApplicationSelector

  -- * Enum types
  , IKScannerDeviceViewDisplayMode(IKScannerDeviceViewDisplayMode)
  , pattern IKScannerDeviceViewDisplayModeNone
  , pattern IKScannerDeviceViewDisplayModeSimple
  , pattern IKScannerDeviceViewDisplayModeAdvanced
  , IKScannerDeviceViewTransferMode(IKScannerDeviceViewTransferMode)
  , pattern IKScannerDeviceViewTransferModeFileBased
  , pattern IKScannerDeviceViewTransferModeMemoryBased

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

-- | delegate
--
-- delegate of the IKScannerDeviceView.
--
-- ObjC selector: @- delegate@
delegate :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO RawId
delegate ikScannerDeviceView  =
    fmap (RawId . castPtr) $ sendMsg ikScannerDeviceView (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- delegate of the IKScannerDeviceView.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> RawId -> IO ()
setDelegate ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- mode@
mode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO IKScannerDeviceViewDisplayMode
mode ikScannerDeviceView  =
    fmap (coerce :: CLong -> IKScannerDeviceViewDisplayMode) $ sendMsg ikScannerDeviceView (mkSelector "mode") retCLong []

-- | mode
--
-- current display mode.
--
-- ObjC selector: @- setMode:@
setMode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IKScannerDeviceViewDisplayMode -> IO ()
setMode ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- | hasDisplayModeSimple
--
-- support a simple scanning UI.
--
-- ObjC selector: @- hasDisplayModeSimple@
hasDisplayModeSimple :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
hasDisplayModeSimple ikScannerDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikScannerDeviceView (mkSelector "hasDisplayModeSimple") retCULong []

-- | hasDisplayModeSimple
--
-- support a simple scanning UI.
--
-- ObjC selector: @- setHasDisplayModeSimple:@
setHasDisplayModeSimple :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setHasDisplayModeSimple ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setHasDisplayModeSimple:") retVoid [argCULong (if value then 1 else 0)]

-- | hasDisplayModeAdvanced
--
-- support advanced scanning UI.
--
-- ObjC selector: @- hasDisplayModeAdvanced@
hasDisplayModeAdvanced :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
hasDisplayModeAdvanced ikScannerDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikScannerDeviceView (mkSelector "hasDisplayModeAdvanced") retCULong []

-- | hasDisplayModeAdvanced
--
-- support advanced scanning UI.
--
-- ObjC selector: @- setHasDisplayModeAdvanced:@
setHasDisplayModeAdvanced :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setHasDisplayModeAdvanced ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setHasDisplayModeAdvanced:") retVoid [argCULong (if value then 1 else 0)]

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- transferMode@
transferMode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO IKScannerDeviceViewTransferMode
transferMode ikScannerDeviceView  =
    fmap (coerce :: CLong -> IKScannerDeviceViewTransferMode) $ sendMsg ikScannerDeviceView (mkSelector "transferMode") retCLong []

-- | transferMode
--
-- transfer mode either file based - or - in memory.
--
-- ObjC selector: @- setTransferMode:@
setTransferMode :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IKScannerDeviceViewTransferMode -> IO ()
setTransferMode ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setTransferMode:") retVoid [argCLong (coerce value)]

-- | scanControlLabel
--
-- label for the 'Scan' control.
--
-- ObjC selector: @- scanControlLabel@
scanControlLabel :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSString)
scanControlLabel ikScannerDeviceView  =
    sendMsg ikScannerDeviceView (mkSelector "scanControlLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | scanControlLabel
--
-- label for the 'Scan' control.
--
-- ObjC selector: @- setScanControlLabel:@
setScanControlLabel :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSString value) => ikScannerDeviceView -> value -> IO ()
setScanControlLabel ikScannerDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikScannerDeviceView (mkSelector "setScanControlLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | overviewControlLabel
--
-- label for the 'Overview' control.
--
-- ObjC selector: @- overviewControlLabel@
overviewControlLabel :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSString)
overviewControlLabel ikScannerDeviceView  =
    sendMsg ikScannerDeviceView (mkSelector "overviewControlLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | overviewControlLabel
--
-- label for the 'Overview' control.
--
-- ObjC selector: @- setOverviewControlLabel:@
setOverviewControlLabel :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSString value) => ikScannerDeviceView -> value -> IO ()
setOverviewControlLabel ikScannerDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikScannerDeviceView (mkSelector "setOverviewControlLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- displaysDownloadsDirectoryControl@
displaysDownloadsDirectoryControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
displaysDownloadsDirectoryControl ikScannerDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikScannerDeviceView (mkSelector "displaysDownloadsDirectoryControl") retCULong []

-- | displaysDownloadsDirectoryControl
--
-- show a downloads directory control.
--
-- ObjC selector: @- setDisplaysDownloadsDirectoryControl:@
setDisplaysDownloadsDirectoryControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setDisplaysDownloadsDirectoryControl ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setDisplaysDownloadsDirectoryControl:") retVoid [argCULong (if value then 1 else 0)]

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- downloadsDirectory@
downloadsDirectory :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSURL)
downloadsDirectory ikScannerDeviceView  =
    sendMsg ikScannerDeviceView (mkSelector "downloadsDirectory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | downloadsDirectory
--
-- downloads directory.
--
-- ObjC selector: @- setDownloadsDirectory:@
setDownloadsDirectory :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSURL value) => ikScannerDeviceView -> value -> IO ()
setDownloadsDirectory ikScannerDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikScannerDeviceView (mkSelector "setDownloadsDirectory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | documentName
--
-- document name.
--
-- ObjC selector: @- documentName@
documentName :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSString)
documentName ikScannerDeviceView  =
    sendMsg ikScannerDeviceView (mkSelector "documentName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | documentName
--
-- document name.
--
-- ObjC selector: @- setDocumentName:@
setDocumentName :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSString value) => ikScannerDeviceView -> value -> IO ()
setDocumentName ikScannerDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikScannerDeviceView (mkSelector "setDocumentName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- displaysPostProcessApplicationControl@
displaysPostProcessApplicationControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO Bool
displaysPostProcessApplicationControl ikScannerDeviceView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikScannerDeviceView (mkSelector "displaysPostProcessApplicationControl") retCULong []

-- | displaysPostProcessApplicationControl
--
-- show a postprocessing application control.
--
-- ObjC selector: @- setDisplaysPostProcessApplicationControl:@
setDisplaysPostProcessApplicationControl :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> Bool -> IO ()
setDisplaysPostProcessApplicationControl ikScannerDeviceView  value =
    sendMsg ikScannerDeviceView (mkSelector "setDisplaysPostProcessApplicationControl:") retVoid [argCULong (if value then 1 else 0)]

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- postProcessApplication@
postProcessApplication :: IsIKScannerDeviceView ikScannerDeviceView => ikScannerDeviceView -> IO (Id NSURL)
postProcessApplication ikScannerDeviceView  =
    sendMsg ikScannerDeviceView (mkSelector "postProcessApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | postProcessApplication
--
-- postprocessing application.
--
-- ObjC selector: @- setPostProcessApplication:@
setPostProcessApplication :: (IsIKScannerDeviceView ikScannerDeviceView, IsNSURL value) => ikScannerDeviceView -> value -> IO ()
setPostProcessApplication ikScannerDeviceView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ikScannerDeviceView (mkSelector "setPostProcessApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @hasDisplayModeSimple@
hasDisplayModeSimpleSelector :: Selector
hasDisplayModeSimpleSelector = mkSelector "hasDisplayModeSimple"

-- | @Selector@ for @setHasDisplayModeSimple:@
setHasDisplayModeSimpleSelector :: Selector
setHasDisplayModeSimpleSelector = mkSelector "setHasDisplayModeSimple:"

-- | @Selector@ for @hasDisplayModeAdvanced@
hasDisplayModeAdvancedSelector :: Selector
hasDisplayModeAdvancedSelector = mkSelector "hasDisplayModeAdvanced"

-- | @Selector@ for @setHasDisplayModeAdvanced:@
setHasDisplayModeAdvancedSelector :: Selector
setHasDisplayModeAdvancedSelector = mkSelector "setHasDisplayModeAdvanced:"

-- | @Selector@ for @transferMode@
transferModeSelector :: Selector
transferModeSelector = mkSelector "transferMode"

-- | @Selector@ for @setTransferMode:@
setTransferModeSelector :: Selector
setTransferModeSelector = mkSelector "setTransferMode:"

-- | @Selector@ for @scanControlLabel@
scanControlLabelSelector :: Selector
scanControlLabelSelector = mkSelector "scanControlLabel"

-- | @Selector@ for @setScanControlLabel:@
setScanControlLabelSelector :: Selector
setScanControlLabelSelector = mkSelector "setScanControlLabel:"

-- | @Selector@ for @overviewControlLabel@
overviewControlLabelSelector :: Selector
overviewControlLabelSelector = mkSelector "overviewControlLabel"

-- | @Selector@ for @setOverviewControlLabel:@
setOverviewControlLabelSelector :: Selector
setOverviewControlLabelSelector = mkSelector "setOverviewControlLabel:"

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

-- | @Selector@ for @documentName@
documentNameSelector :: Selector
documentNameSelector = mkSelector "documentName"

-- | @Selector@ for @setDocumentName:@
setDocumentNameSelector :: Selector
setDocumentNameSelector = mkSelector "setDocumentName:"

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

