{-# LANGUAGE PatternSynonyms #-}
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
  , transferMode
  , setTransferMode
  , maxMemoryBandSize
  , setMaxMemoryBandSize
  , requestOpenSessionWithCredentials_passwordSelector
  , requestSelectFunctionalUnitSelector
  , requestOverviewScanSelector
  , requestScanSelector
  , cancelScanSelector
  , transferModeSelector
  , setTransferModeSelector
  , maxMemoryBandSizeSelector
  , setMaxMemoryBandSizeSelector

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
requestOpenSessionWithCredentials_password icScannerDevice  username password =
withObjCPtr username $ \raw_username ->
  withObjCPtr password $ \raw_password ->
      sendMsg icScannerDevice (mkSelector "requestOpenSessionWithCredentials:password:") retVoid [argPtr (castPtr raw_username :: Ptr ()), argPtr (castPtr raw_password :: Ptr ())]

-- | requestSelectFunctionalUnit:delegate:selector:contextInfo:
--
-- Requests the scanner device to select a functional unit.
--
-- When this request is completed, the delegate will be notified using the 'scannerDevice:didSelectFunctionalUnit:error:' message.
--
-- ObjC selector: @- requestSelectFunctionalUnit:@
requestSelectFunctionalUnit :: IsICScannerDevice icScannerDevice => icScannerDevice -> ICScannerFunctionalUnitType -> IO ()
requestSelectFunctionalUnit icScannerDevice  type_ =
  sendMsg icScannerDevice (mkSelector "requestSelectFunctionalUnit:") retVoid [argCULong (coerce type_)]

-- | requestOverviewScan
--
-- Starts an overview scan on selectedFunctionalUnit.
--
-- When this request is completed, the delegate will be notified using the 'scannerDevice:didCompleteOverviewScanWithError:' message. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestOverviewScan@
requestOverviewScan :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ()
requestOverviewScan icScannerDevice  =
  sendMsg icScannerDevice (mkSelector "requestOverviewScan") retVoid []

-- | requestScan
--
-- Starts a scan on selectedFunctionalUnit.
--
-- When this request is completed, the delegate will be notified using the 'scannerDevice:didCompleteScanWithError:' message. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestScan@
requestScan :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ()
requestScan icScannerDevice  =
  sendMsg icScannerDevice (mkSelector "requestScan") retVoid []

-- | cancelScan
--
-- Cancels the current scan operation started by sending a 'requestOverviewScan' or 'requestScan'.
--
-- ObjC selector: @- cancelScan@
cancelScan :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ()
cancelScan icScannerDevice  =
  sendMsg icScannerDevice (mkSelector "cancelScan") retVoid []

-- | transferMode
--
-- ￼The transfer mode for scanned document.
--
-- ObjC selector: @- transferMode@
transferMode :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO ICScannerTransferMode
transferMode icScannerDevice  =
  fmap (coerce :: CULong -> ICScannerTransferMode) $ sendMsg icScannerDevice (mkSelector "transferMode") retCULong []

-- | transferMode
--
-- ￼The transfer mode for scanned document.
--
-- ObjC selector: @- setTransferMode:@
setTransferMode :: IsICScannerDevice icScannerDevice => icScannerDevice -> ICScannerTransferMode -> IO ()
setTransferMode icScannerDevice  value =
  sendMsg icScannerDevice (mkSelector "setTransferMode:") retVoid [argCULong (coerce value)]

-- | maxMemoryBandSize
--
-- ￼The total maximum band size requested when performing a ICScannerTransferModeMemoryBased.
--
-- ObjC selector: @- maxMemoryBandSize@
maxMemoryBandSize :: IsICScannerDevice icScannerDevice => icScannerDevice -> IO CUInt
maxMemoryBandSize icScannerDevice  =
  sendMsg icScannerDevice (mkSelector "maxMemoryBandSize") retCUInt []

-- | maxMemoryBandSize
--
-- ￼The total maximum band size requested when performing a ICScannerTransferModeMemoryBased.
--
-- ObjC selector: @- setMaxMemoryBandSize:@
setMaxMemoryBandSize :: IsICScannerDevice icScannerDevice => icScannerDevice -> CUInt -> IO ()
setMaxMemoryBandSize icScannerDevice  value =
  sendMsg icScannerDevice (mkSelector "setMaxMemoryBandSize:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestOpenSessionWithCredentials:password:@
requestOpenSessionWithCredentials_passwordSelector :: Selector
requestOpenSessionWithCredentials_passwordSelector = mkSelector "requestOpenSessionWithCredentials:password:"

-- | @Selector@ for @requestSelectFunctionalUnit:@
requestSelectFunctionalUnitSelector :: Selector
requestSelectFunctionalUnitSelector = mkSelector "requestSelectFunctionalUnit:"

-- | @Selector@ for @requestOverviewScan@
requestOverviewScanSelector :: Selector
requestOverviewScanSelector = mkSelector "requestOverviewScan"

-- | @Selector@ for @requestScan@
requestScanSelector :: Selector
requestScanSelector = mkSelector "requestScan"

-- | @Selector@ for @cancelScan@
cancelScanSelector :: Selector
cancelScanSelector = mkSelector "cancelScan"

-- | @Selector@ for @transferMode@
transferModeSelector :: Selector
transferModeSelector = mkSelector "transferMode"

-- | @Selector@ for @setTransferMode:@
setTransferModeSelector :: Selector
setTransferModeSelector = mkSelector "setTransferMode:"

-- | @Selector@ for @maxMemoryBandSize@
maxMemoryBandSizeSelector :: Selector
maxMemoryBandSizeSelector = mkSelector "maxMemoryBandSize"

-- | @Selector@ for @setMaxMemoryBandSize:@
setMaxMemoryBandSizeSelector :: Selector
setMaxMemoryBandSizeSelector = mkSelector "setMaxMemoryBandSize:"

