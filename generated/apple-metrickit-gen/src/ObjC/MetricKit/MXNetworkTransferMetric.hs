{-# LANGUAGE DataKinds #-}
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
  , cumulativeCellularDownloadSelector
  , cumulativeCellularUploadSelector
  , cumulativeWifiDownloadSelector
  , cumulativeWifiUploadSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
cumulativeWifiUpload mxNetworkTransferMetric =
  sendMessage mxNetworkTransferMetric cumulativeWifiUploadSelector

-- | cumulativeWifiDownload
--
-- Cumulative amount of data downloaded over WiFi.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeWifiDownload@
cumulativeWifiDownload :: IsMXNetworkTransferMetric mxNetworkTransferMetric => mxNetworkTransferMetric -> IO (Id NSMeasurement)
cumulativeWifiDownload mxNetworkTransferMetric =
  sendMessage mxNetworkTransferMetric cumulativeWifiDownloadSelector

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
cumulativeCellularUpload mxNetworkTransferMetric =
  sendMessage mxNetworkTransferMetric cumulativeCellularUploadSelector

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
cumulativeCellularDownload mxNetworkTransferMetric =
  sendMessage mxNetworkTransferMetric cumulativeCellularDownloadSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeWifiUpload@
cumulativeWifiUploadSelector :: Selector '[] (Id NSMeasurement)
cumulativeWifiUploadSelector = mkSelector "cumulativeWifiUpload"

-- | @Selector@ for @cumulativeWifiDownload@
cumulativeWifiDownloadSelector :: Selector '[] (Id NSMeasurement)
cumulativeWifiDownloadSelector = mkSelector "cumulativeWifiDownload"

-- | @Selector@ for @cumulativeCellularUpload@
cumulativeCellularUploadSelector :: Selector '[] (Id NSMeasurement)
cumulativeCellularUploadSelector = mkSelector "cumulativeCellularUpload"

-- | @Selector@ for @cumulativeCellularDownload@
cumulativeCellularDownloadSelector :: Selector '[] (Id NSMeasurement)
cumulativeCellularDownloadSelector = mkSelector "cumulativeCellularDownload"

