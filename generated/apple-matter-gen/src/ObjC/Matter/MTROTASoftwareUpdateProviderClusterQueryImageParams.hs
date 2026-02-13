{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateProviderClusterQueryImageParams@.
module ObjC.Matter.MTROTASoftwareUpdateProviderClusterQueryImageParams
  ( MTROTASoftwareUpdateProviderClusterQueryImageParams
  , IsMTROTASoftwareUpdateProviderClusterQueryImageParams(..)
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , softwareVersion
  , setSoftwareVersion
  , protocolsSupported
  , setProtocolsSupported
  , hardwareVersion
  , setHardwareVersion
  , location
  , setLocation
  , requestorCanConsent
  , setRequestorCanConsent
  , metadataForProvider
  , setMetadataForProvider
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , vendorId
  , setVendorId
  , productId
  , setProductId
  , hardwareVersionSelector
  , locationSelector
  , metadataForProviderSelector
  , productIDSelector
  , productIdSelector
  , protocolsSupportedSelector
  , requestorCanConsentSelector
  , serverSideProcessingTimeoutSelector
  , setHardwareVersionSelector
  , setLocationSelector
  , setMetadataForProviderSelector
  , setProductIDSelector
  , setProductIdSelector
  , setProtocolsSupportedSelector
  , setRequestorCanConsentSelector
  , setServerSideProcessingTimeoutSelector
  , setSoftwareVersionSelector
  , setTimedInvokeTimeoutMsSelector
  , setVendorIDSelector
  , setVendorIdSelector
  , softwareVersionSelector
  , timedInvokeTimeoutMsSelector
  , vendorIDSelector
  , vendorIdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- vendorID@
vendorID :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
vendorID mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setVendorID mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setVendorIDSelector (toNSNumber value)

-- | @- productID@
productID :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
productID mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams productIDSelector

-- | @- setProductID:@
setProductID :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setProductID mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setProductIDSelector (toNSNumber value)

-- | @- softwareVersion@
softwareVersion :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
softwareVersion mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setSoftwareVersion mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setSoftwareVersionSelector (toNSNumber value)

-- | @- protocolsSupported@
protocolsSupported :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSArray)
protocolsSupported mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams protocolsSupportedSelector

-- | @- setProtocolsSupported:@
setProtocolsSupported :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSArray value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setProtocolsSupported mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setProtocolsSupportedSelector (toNSArray value)

-- | @- hardwareVersion@
hardwareVersion :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
hardwareVersion mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams hardwareVersionSelector

-- | @- setHardwareVersion:@
setHardwareVersion :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setHardwareVersion mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setHardwareVersionSelector (toNSNumber value)

-- | @- location@
location :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSString)
location mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams locationSelector

-- | @- setLocation:@
setLocation :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSString value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setLocation mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setLocationSelector (toNSString value)

-- | @- requestorCanConsent@
requestorCanConsent :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
requestorCanConsent mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams requestorCanConsentSelector

-- | @- setRequestorCanConsent:@
setRequestorCanConsent :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setRequestorCanConsent mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setRequestorCanConsentSelector (toNSNumber value)

-- | @- metadataForProvider@
metadataForProvider :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSData)
metadataForProvider mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams metadataForProviderSelector

-- | @- setMetadataForProvider:@
setMetadataForProvider :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSData value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setMetadataForProvider mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setMetadataForProviderSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setServerSideProcessingTimeout mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- | @- vendorId@
vendorId :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
vendorId mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams vendorIdSelector

-- | @- setVendorId:@
setVendorId :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setVendorId mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setVendorIdSelector (toNSNumber value)

-- | @- productId@
productId :: IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> IO (Id NSNumber)
productId mtrotaSoftwareUpdateProviderClusterQueryImageParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams productIdSelector

-- | @- setProductId:@
setProductId :: (IsMTROTASoftwareUpdateProviderClusterQueryImageParams mtrotaSoftwareUpdateProviderClusterQueryImageParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageParams -> value -> IO ()
setProductId mtrotaSoftwareUpdateProviderClusterQueryImageParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageParams setProductIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector '[Id NSNumber] ()
setProductIDSelector = mkSelector "setProductID:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @protocolsSupported@
protocolsSupportedSelector :: Selector '[] (Id NSArray)
protocolsSupportedSelector = mkSelector "protocolsSupported"

-- | @Selector@ for @setProtocolsSupported:@
setProtocolsSupportedSelector :: Selector '[Id NSArray] ()
setProtocolsSupportedSelector = mkSelector "setProtocolsSupported:"

-- | @Selector@ for @hardwareVersion@
hardwareVersionSelector :: Selector '[] (Id NSNumber)
hardwareVersionSelector = mkSelector "hardwareVersion"

-- | @Selector@ for @setHardwareVersion:@
setHardwareVersionSelector :: Selector '[Id NSNumber] ()
setHardwareVersionSelector = mkSelector "setHardwareVersion:"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id NSString)
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector '[Id NSString] ()
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @requestorCanConsent@
requestorCanConsentSelector :: Selector '[] (Id NSNumber)
requestorCanConsentSelector = mkSelector "requestorCanConsent"

-- | @Selector@ for @setRequestorCanConsent:@
setRequestorCanConsentSelector :: Selector '[Id NSNumber] ()
setRequestorCanConsentSelector = mkSelector "setRequestorCanConsent:"

-- | @Selector@ for @metadataForProvider@
metadataForProviderSelector :: Selector '[] (Id NSData)
metadataForProviderSelector = mkSelector "metadataForProvider"

-- | @Selector@ for @setMetadataForProvider:@
setMetadataForProviderSelector :: Selector '[Id NSData] ()
setMetadataForProviderSelector = mkSelector "setMetadataForProvider:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

-- | @Selector@ for @vendorId@
vendorIdSelector :: Selector '[] (Id NSNumber)
vendorIdSelector = mkSelector "vendorId"

-- | @Selector@ for @setVendorId:@
setVendorIdSelector :: Selector '[Id NSNumber] ()
setVendorIdSelector = mkSelector "setVendorId:"

-- | @Selector@ for @productId@
productIdSelector :: Selector '[] (Id NSNumber)
productIdSelector = mkSelector "productId"

-- | @Selector@ for @setProductId:@
setProductIdSelector :: Selector '[Id NSNumber] ()
setProductIdSelector = mkSelector "setProductId:"

