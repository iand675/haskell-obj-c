{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXNetworkTransferMetric
--
-- An MXMetric subclass that encapsulates network transfer metrics
--
-- Generated bindings for @MXNetworkTransferMetric@.
module ObjC.MetricKit.MXNetworkTransferMetric
  ( MXNetworkTransferMetric
  , IsMXNetworkTransferMetric(..)
  , cumulativeWifiUpload
  , cumulativeWifiDownload
  , cumulativeCellularUpload
  , cumulativeCellularDownload
  , cumulativeWifiUploadSelector
  , cumulativeWifiDownloadSelector
  , cumulativeCellularUploadSelector
  , cumulativeCellularDownloadSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cumulativeWifiUpload
--
-- Cumulative amount of data uploaded over WiFi.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeWifiUpload@
cumulativeWifiUpload :: IsMXNetworkTransferMetric mxNetworkTransferMetric => mxNetworkTransferMetric -> IO (Id NSMeasurement)
cumulativeWifiUpload mxNetworkTransferMetric  =
  sendMsg mxNetworkTransferMetric (mkSelector "cumulativeWifiUpload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeWifiDownload
--
-- Cumulative amount of data downloaded over WiFi.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeWifiDownload@
cumulativeWifiDownload :: IsMXNetworkTransferMetric mxNetworkTransferMetric => mxNetworkTransferMetric -> IO (Id NSMeasurement)
cumulativeWifiDownload mxNetworkTransferMetric  =
  sendMsg mxNetworkTransferMetric (mkSelector "cumulativeWifiDownload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeCellularUpload
--
-- Cumulative amount of data uploaded over cellular networks.
--
-- This data is radio access technology agnostic.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeCellularUpload@
cumulativeCellularUpload :: IsMXNetworkTransferMetric mxNetworkTransferMetric => mxNetworkTransferMetric -> IO (Id NSMeasurement)
cumulativeCellularUpload mxNetworkTransferMetric  =
  sendMsg mxNetworkTransferMetric (mkSelector "cumulativeCellularUpload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeCellularDownload
--
-- Cumulative amount of data downloaded over cellular networks.
--
-- This data is radio access technology agnostic.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeCellularDownload@
cumulativeCellularDownload :: IsMXNetworkTransferMetric mxNetworkTransferMetric => mxNetworkTransferMetric -> IO (Id NSMeasurement)
cumulativeCellularDownload mxNetworkTransferMetric  =
  sendMsg mxNetworkTransferMetric (mkSelector "cumulativeCellularDownload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeWifiUpload@
cumulativeWifiUploadSelector :: Selector
cumulativeWifiUploadSelector = mkSelector "cumulativeWifiUpload"

-- | @Selector@ for @cumulativeWifiDownload@
cumulativeWifiDownloadSelector :: Selector
cumulativeWifiDownloadSelector = mkSelector "cumulativeWifiDownload"

-- | @Selector@ for @cumulativeCellularUpload@
cumulativeCellularUploadSelector :: Selector
cumulativeCellularUploadSelector = mkSelector "cumulativeCellularUpload"

-- | @Selector@ for @cumulativeCellularDownload@
cumulativeCellularDownloadSelector :: Selector
cumulativeCellularDownloadSelector = mkSelector "cumulativeCellularDownload"

