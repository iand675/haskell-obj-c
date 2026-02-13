{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerDevice
--
-- ICScannerDevice is a concrete subclass of ICDevice class. ICDeviceBrowser creates instances of this class.
--
-- In this release, an instance of ICScannerDevice class is intended to be used by the ICScannerDeviceView object. The ICScannerDeviceView class encapsulates the complexities of setting scan parameters, performing scans and saving the result. The developer should consider using ICScannerDeviceView instead of building their own views using the ICScannerDevice object.
--
-- Generated bindings for @ICScannerDevice@.
module ObjC.ImageCaptureCore.ICScannerDevice
  ( ICScannerDevice
  , IsICScannerDevice(..)
  , requestOpenSessionWithCredentials_password
  , requestSelectFunctionalUnit
  , requestOverviewScan
  , requestScan
  , cancelScan
  , selectedFunctionalUnit
  , transferMode
  , setTransferMode
  , maxMemoryBandSize
  , setMaxMemoryBandSize
  , downloadsDirectory
  , setDownloadsDirectory
  , documentName
  , setDocumentName
  , documentUTI
  , setDocumentUTI
  , defaultUsername
  , setDefaultUsername
  , cancelScanSelector
  , defaultUsernameSelector
  , documentNameSelector
  , documentUTISelector
  , downloadsDirectorySelector
  , maxMemoryBandSizeSelector
  , requestOpenSessionWithCredentials_passwordSelector
  , requestOverviewScanSelector
  , requestScanSelector
  , requestSelectFunctionalUnitSelector
  , selectedFunctionalUnitSelector
  , setDefaultUsernameSelector
  , setDocumentNameSelector
  , setDocumentUTISelector
  , setDownloadsDirectorySelector
  , setMaxMemoryBandSizeSelector
  , setTransferModeSelector
  , transferModeSelector

  -- * Enum types
  , ICScannerFunctionalUnitType(ICScannerFunctionalUnitType)
  , pattern ICScannerFunctionalUnitTypeFlatbed
  , pattern ICScannerFunctionalUnitTypePositiveTransparency
  , pattern ICScannerFunctionalUnitTypeNegativeTransparency
  , pattern ICScannerFunctionalUnitTypeDocumentFeeder
  , ICScannerTransferMode(ICScannerTransferMode)
  , pattern ICScannerTransferModeFileBased
  , pattern ICScannerTransferModeMemoryBased

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | requestOpenSessionWithCredentials:
--
-- This message requests to open a session on the protected device with the authorized username and           passcode.  If the device reports back a failure of credentials, they can be provided here for the           launch.           A client MUST open a session on a device in order to use the device.
--
-- Make sure the receiver's delegate is set prior to sending this message; otherwise this message will be ignored. This request is completed when the delegate receives a "device:didOpenSessionWithError:" message. No more messages will be sent to the delegate if this request fails.
--
-- ObjC selector: @- requestOpenSessionWithCredentials:password:@
requestOpenSessionWithCredentials_password :: (IsICScannerDevice icScannerDevice, IsNSString username, IsNSString password) => icScannerDevice -> username -> password -> IO ()
requestOpenSessionWithCredentials_password icScannerDevice username password =
  sendMessage icScannerDevice requestOpenSessionWithCredentials_passwordSelector (toNSString username) (toNSString password)

-- | requestSelectFunctionalUnit:delegate:selector:contextInfo:
--
-- Requests the scanner device to select a functional unit.
--
-- When this request is completed, the delegate will be notified using the 'scannerDevice:didSelectFunctionalUnit:error:' message.
--
-- ObjC selector: @- requestSelectFunctionalUnit:@
requestSelectFunctionalUnit :: IsICScannerDevice icScannerDevice => icScannerDevice -> ICScannerFunctionalUnitType -> IO ()
requestSelectFunctionalUnit icScannerDevice type_ =
  sendMessage icScannerDevice requestSelectFunctionalUnitSelector type_

-- | requestOverviewScan
--
-- Starts an overview scan on selectedFunctionalUnit.
--
-- When this request is completed, the delegate will be notified using the 'scannerDevice:didCompleteOverviewScanWithError:' message. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestOverviewScan@
requestOverviewScan :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ()
requestOverviewScan icScannerDevice =
  sendMessage icScannerDevice requestOverviewScanSelector

-- | requestScan
--
-- Starts a scan on selectedFunctionalUnit.
--
-- When this request is completed, the delegate will be notified using the 'scannerDevice:didCompleteScanWithError:' message. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestScan@
requestScan :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ()
requestScan icScannerDevice =
  sendMessage icScannerDevice requestScanSelector

-- | cancelScan
--
-- Cancels the current scan operation started by sending a 'requestOverviewScan' or 'requestScan'.
--
-- ObjC selector: @- cancelScan@
cancelScan :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ()
cancelScan icScannerDevice =
  sendMessage icScannerDevice cancelScanSelector

-- | selectedFunctionalUnit
--
-- ￼The currently selected functional unit on the scanner device.
--
-- ObjC selector: @- selectedFunctionalUnit@
selectedFunctionalUnit :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO RawId
selectedFunctionalUnit icScannerDevice =
  sendMessage icScannerDevice selectedFunctionalUnitSelector

-- | transferMode
--
-- ￼The transfer mode for scanned document.
--
-- ObjC selector: @- transferMode@
transferMode :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ICScannerTransferMode
transferMode icScannerDevice =
  sendMessage icScannerDevice transferModeSelector

-- | transferMode
--
-- ￼The transfer mode for scanned document.
--
-- ObjC selector: @- setTransferMode:@
setTransferMode :: IsICScannerDevice icScannerDevice => icScannerDevice -> ICScannerTransferMode -> IO ()
setTransferMode icScannerDevice value =
  sendMessage icScannerDevice setTransferModeSelector value

-- | maxMemoryBandSize
--
-- ￼The total maximum band size requested when performing a ICScannerTransferModeMemoryBased.
--
-- ObjC selector: @- maxMemoryBandSize@
maxMemoryBandSize :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO CUInt
maxMemoryBandSize icScannerDevice =
  sendMessage icScannerDevice maxMemoryBandSizeSelector

-- | maxMemoryBandSize
--
-- ￼The total maximum band size requested when performing a ICScannerTransferModeMemoryBased.
--
-- ObjC selector: @- setMaxMemoryBandSize:@
setMaxMemoryBandSize :: IsICScannerDevice icScannerDevice => icScannerDevice -> CUInt -> IO ()
setMaxMemoryBandSize icScannerDevice value =
  sendMessage icScannerDevice setMaxMemoryBandSizeSelector value

-- | downloadsDirectory
--
-- ￼The downloads directory.
--
-- ObjC selector: @- downloadsDirectory@
downloadsDirectory :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO RawId
downloadsDirectory icScannerDevice =
  sendMessage icScannerDevice downloadsDirectorySelector

-- | downloadsDirectory
--
-- ￼The downloads directory.
--
-- ObjC selector: @- setDownloadsDirectory:@
setDownloadsDirectory :: IsICScannerDevice icScannerDevice => icScannerDevice -> RawId -> IO ()
setDownloadsDirectory icScannerDevice value =
  sendMessage icScannerDevice setDownloadsDirectorySelector value

-- | documentName
--
-- ￼The document name.
--
-- ObjC selector: @- documentName@
documentName :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO RawId
documentName icScannerDevice =
  sendMessage icScannerDevice documentNameSelector

-- | documentName
--
-- ￼The document name.
--
-- ObjC selector: @- setDocumentName:@
setDocumentName :: IsICScannerDevice icScannerDevice => icScannerDevice -> RawId -> IO ()
setDocumentName icScannerDevice value =
  sendMessage icScannerDevice setDocumentNameSelector value

-- | documentUTI
--
-- ￼The document UTI. Currently supported UTIs are: kUTTypeJPEG, kUTTypeJPEG2000, kUTTypeTIFF, kUTTypePNG etc.
--
-- ObjC selector: @- documentUTI@
documentUTI :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO RawId
documentUTI icScannerDevice =
  sendMessage icScannerDevice documentUTISelector

-- | documentUTI
--
-- ￼The document UTI. Currently supported UTIs are: kUTTypeJPEG, kUTTypeJPEG2000, kUTTypeTIFF, kUTTypePNG etc.
--
-- ObjC selector: @- setDocumentUTI:@
setDocumentUTI :: IsICScannerDevice icScannerDevice => icScannerDevice -> RawId -> IO ()
setDocumentUTI icScannerDevice value =
  sendMessage icScannerDevice setDocumentUTISelector value

-- | defaultUsername
--
-- If the device is protected, instead of prompting the user for a username, this property        can be set to default to a specific username as a convience.  The value will persist until        reset by setting it to nil.
--
-- ObjC selector: @- defaultUsername@
defaultUsername :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO RawId
defaultUsername icScannerDevice =
  sendMessage icScannerDevice defaultUsernameSelector

-- | defaultUsername
--
-- If the device is protected, instead of prompting the user for a username, this property        can be set to default to a specific username as a convience.  The value will persist until        reset by setting it to nil.
--
-- ObjC selector: @- setDefaultUsername:@
setDefaultUsername :: IsICScannerDevice icScannerDevice => icScannerDevice -> RawId -> IO ()
setDefaultUsername icScannerDevice value =
  sendMessage icScannerDevice setDefaultUsernameSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestOpenSessionWithCredentials:password:@
requestOpenSessionWithCredentials_passwordSelector :: Selector '[Id NSString, Id NSString] ()
requestOpenSessionWithCredentials_passwordSelector = mkSelector "requestOpenSessionWithCredentials:password:"

-- | @Selector@ for @requestSelectFunctionalUnit:@
requestSelectFunctionalUnitSelector :: Selector '[ICScannerFunctionalUnitType] ()
requestSelectFunctionalUnitSelector = mkSelector "requestSelectFunctionalUnit:"

-- | @Selector@ for @requestOverviewScan@
requestOverviewScanSelector :: Selector '[] ()
requestOverviewScanSelector = mkSelector "requestOverviewScan"

-- | @Selector@ for @requestScan@
requestScanSelector :: Selector '[] ()
requestScanSelector = mkSelector "requestScan"

-- | @Selector@ for @cancelScan@
cancelScanSelector :: Selector '[] ()
cancelScanSelector = mkSelector "cancelScan"

-- | @Selector@ for @selectedFunctionalUnit@
selectedFunctionalUnitSelector :: Selector '[] RawId
selectedFunctionalUnitSelector = mkSelector "selectedFunctionalUnit"

-- | @Selector@ for @transferMode@
transferModeSelector :: Selector '[] ICScannerTransferMode
transferModeSelector = mkSelector "transferMode"

-- | @Selector@ for @setTransferMode:@
setTransferModeSelector :: Selector '[ICScannerTransferMode] ()
setTransferModeSelector = mkSelector "setTransferMode:"

-- | @Selector@ for @maxMemoryBandSize@
maxMemoryBandSizeSelector :: Selector '[] CUInt
maxMemoryBandSizeSelector = mkSelector "maxMemoryBandSize"

-- | @Selector@ for @setMaxMemoryBandSize:@
setMaxMemoryBandSizeSelector :: Selector '[CUInt] ()
setMaxMemoryBandSizeSelector = mkSelector "setMaxMemoryBandSize:"

-- | @Selector@ for @downloadsDirectory@
downloadsDirectorySelector :: Selector '[] RawId
downloadsDirectorySelector = mkSelector "downloadsDirectory"

-- | @Selector@ for @setDownloadsDirectory:@
setDownloadsDirectorySelector :: Selector '[RawId] ()
setDownloadsDirectorySelector = mkSelector "setDownloadsDirectory:"

-- | @Selector@ for @documentName@
documentNameSelector :: Selector '[] RawId
documentNameSelector = mkSelector "documentName"

-- | @Selector@ for @setDocumentName:@
setDocumentNameSelector :: Selector '[RawId] ()
setDocumentNameSelector = mkSelector "setDocumentName:"

-- | @Selector@ for @documentUTI@
documentUTISelector :: Selector '[] RawId
documentUTISelector = mkSelector "documentUTI"

-- | @Selector@ for @setDocumentUTI:@
setDocumentUTISelector :: Selector '[RawId] ()
setDocumentUTISelector = mkSelector "setDocumentUTI:"

-- | @Selector@ for @defaultUsername@
defaultUsernameSelector :: Selector '[] RawId
defaultUsernameSelector = mkSelector "defaultUsername"

-- | @Selector@ for @setDefaultUsername:@
setDefaultUsernameSelector :: Selector '[RawId] ()
setDefaultUsernameSelector = mkSelector "setDefaultUsername:"

