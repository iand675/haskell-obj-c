{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTAHeader@.
module ObjC.Matter.MTROTAHeader
  ( MTROTAHeader
  , IsMTROTAHeader(..)
  , initWithData
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , payloadSize
  , setPayloadSize
  , softwareVersion
  , setSoftwareVersion
  , softwareVersionString
  , setSoftwareVersionString
  , releaseNotesURL
  , setReleaseNotesURL
  , imageDigest
  , setImageDigest
  , imageDigestType
  , setImageDigestType
  , minApplicableVersion
  , setMinApplicableVersion
  , maxApplicableVersion
  , setMaxApplicableVersion
  , imageDigestSelector
  , imageDigestTypeSelector
  , initWithDataSelector
  , maxApplicableVersionSelector
  , minApplicableVersionSelector
  , payloadSizeSelector
  , productIDSelector
  , releaseNotesURLSelector
  , setImageDigestSelector
  , setImageDigestTypeSelector
  , setMaxApplicableVersionSelector
  , setMinApplicableVersionSelector
  , setPayloadSizeSelector
  , setProductIDSelector
  , setReleaseNotesURLSelector
  , setSoftwareVersionSelector
  , setSoftwareVersionStringSelector
  , setVendorIDSelector
  , softwareVersionSelector
  , softwareVersionStringSelector
  , vendorIDSelector

  -- * Enum types
  , MTROTAImageDigestType(MTROTAImageDigestType)
  , pattern MTROTAImageDigestTypeSha256
  , pattern MTROTAImageDigestTypeSha256_128
  , pattern MTROTAImageDigestTypeSha256_120
  , pattern MTROTAImageDigestTypeSha256_96
  , pattern MTROTAImageDigestTypeSha256_64
  , pattern MTROTAImageDigestTypeSha256_32
  , pattern MTROTAImageDigestTypeSha384
  , pattern MTROTAImageDigestTypeSha512
  , pattern MTROTAImageDigestTypeSha3_224
  , pattern MTROTAImageDigestTypeSha3_256
  , pattern MTROTAImageDigestTypeSha3_384
  , pattern MTROTAImageDigestTypeSha3_512

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize the MTROTAHeader with the given Matter OTA software image data (as defined in the "Over-the-Air (OTA) Software Update File Format" section of the Matter specification).  The provided data is expected to point to a large enough initial chunk of an OTA software image that it includes the entire header (e.g. the entire image).
--
-- If the passed-in data is too small and does not contain the entire OTA image header, initWithData will return nil and the caller should try creating a new MTROTAHeader object and initializing it with a larger chunk of the image.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsMTROTAHeader mtrotaHeader, IsNSData data_) => mtrotaHeader -> data_ -> IO (Id MTROTAHeader)
initWithData mtrotaHeader data_ =
  sendOwnedMessage mtrotaHeader initWithDataSelector (toNSData data_)

-- | The identifier of the vendor whose product this image is meant for.
--
-- This field can be compared to the vendor id received in the Query Image command to determine whether an image matches.
--
-- This field may be 0, in which case the image might apply to products from more than one vendor.  If it's nonzero, it must match the vendor id in Query Image for this image to be considered.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
vendorID mtrotaHeader =
  sendMessage mtrotaHeader vendorIDSelector

-- | The identifier of the vendor whose product this image is meant for.
--
-- This field can be compared to the vendor id received in the Query Image command to determine whether an image matches.
--
-- This field may be 0, in which case the image might apply to products from more than one vendor.  If it's nonzero, it must match the vendor id in Query Image for this image to be considered.
--
-- ObjC selector: @- setVendorID:@
setVendorID :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setVendorID mtrotaHeader value =
  sendMessage mtrotaHeader setVendorIDSelector (toNSNumber value)

-- | The identifier of the specific product the image is meant for.  May be 0, if the image might apply to more than one product.  This is allowed, but not required, to be matched against the product id received in Query Image.
--
-- ObjC selector: @- productID@
productID :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
productID mtrotaHeader =
  sendMessage mtrotaHeader productIDSelector

-- | The identifier of the specific product the image is meant for.  May be 0, if the image might apply to more than one product.  This is allowed, but not required, to be matched against the product id received in Query Image.
--
-- ObjC selector: @- setProductID:@
setProductID :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setProductID mtrotaHeader value =
  sendMessage mtrotaHeader setProductIDSelector (toNSNumber value)

-- | The size of the actual image payload, which follows the header in the OTA file.
--
-- ObjC selector: @- payloadSize@
payloadSize :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
payloadSize mtrotaHeader =
  sendMessage mtrotaHeader payloadSizeSelector

-- | The size of the actual image payload, which follows the header in the OTA file.
--
-- ObjC selector: @- setPayloadSize:@
setPayloadSize :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setPayloadSize mtrotaHeader value =
  sendMessage mtrotaHeader setPayloadSizeSelector (toNSNumber value)

-- | The version of the software contained in this image.  This is the version the OTA requestor will be updated to if this image is installed.  This can be used to determine whether this image is newer than what the requestor is currently running, by comparing it to the SoftwareVersion in the Query Image command.
--
-- ObjC selector: @- softwareVersion@
softwareVersion :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
softwareVersion mtrotaHeader =
  sendMessage mtrotaHeader softwareVersionSelector

-- | The version of the software contained in this image.  This is the version the OTA requestor will be updated to if this image is installed.  This can be used to determine whether this image is newer than what the requestor is currently running, by comparing it to the SoftwareVersion in the Query Image command.
--
-- ObjC selector: @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setSoftwareVersion mtrotaHeader value =
  sendMessage mtrotaHeader setSoftwareVersionSelector (toNSNumber value)

-- | Human-readable version of softwareVersion.  This must not be used for deciding which versions are newer or older; use softwareVersion for that.
--
-- ObjC selector: @- softwareVersionString@
softwareVersionString :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSString)
softwareVersionString mtrotaHeader =
  sendMessage mtrotaHeader softwareVersionStringSelector

-- | Human-readable version of softwareVersion.  This must not be used for deciding which versions are newer or older; use softwareVersion for that.
--
-- ObjC selector: @- setSoftwareVersionString:@
setSoftwareVersionString :: (IsMTROTAHeader mtrotaHeader, IsNSString value) => mtrotaHeader -> value -> IO ()
setSoftwareVersionString mtrotaHeader value =
  sendMessage mtrotaHeader setSoftwareVersionStringSelector (toNSString value)

-- | If not nil a URL pointing to release notes for the software update represented by the image.
--
-- ObjC selector: @- releaseNotesURL@
releaseNotesURL :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSString)
releaseNotesURL mtrotaHeader =
  sendMessage mtrotaHeader releaseNotesURLSelector

-- | If not nil a URL pointing to release notes for the software update represented by the image.
--
-- ObjC selector: @- setReleaseNotesURL:@
setReleaseNotesURL :: (IsMTROTAHeader mtrotaHeader, IsNSString value) => mtrotaHeader -> value -> IO ()
setReleaseNotesURL mtrotaHeader value =
  sendMessage mtrotaHeader setReleaseNotesURLSelector (toNSString value)

-- | A digest of the payload that follows the header.  Can be used to verify that the payload is not truncated or corrupted.
--
-- ObjC selector: @- imageDigest@
imageDigest :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSData)
imageDigest mtrotaHeader =
  sendMessage mtrotaHeader imageDigestSelector

-- | A digest of the payload that follows the header.  Can be used to verify that the payload is not truncated or corrupted.
--
-- ObjC selector: @- setImageDigest:@
setImageDigest :: (IsMTROTAHeader mtrotaHeader, IsNSData value) => mtrotaHeader -> value -> IO ()
setImageDigest mtrotaHeader value =
  sendMessage mtrotaHeader setImageDigestSelector (toNSData value)

-- | The specific algorithm that was used to compute imageDigest.
--
-- ObjC selector: @- imageDigestType@
imageDigestType :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO MTROTAImageDigestType
imageDigestType mtrotaHeader =
  sendMessage mtrotaHeader imageDigestTypeSelector

-- | The specific algorithm that was used to compute imageDigest.
--
-- ObjC selector: @- setImageDigestType:@
setImageDigestType :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> MTROTAImageDigestType -> IO ()
setImageDigestType mtrotaHeader value =
  sendMessage mtrotaHeader setImageDigestTypeSelector value

-- | If not nil, specifies the smallest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- minApplicableVersion@
minApplicableVersion :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
minApplicableVersion mtrotaHeader =
  sendMessage mtrotaHeader minApplicableVersionSelector

-- | If not nil, specifies the smallest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- setMinApplicableVersion:@
setMinApplicableVersion :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setMinApplicableVersion mtrotaHeader value =
  sendMessage mtrotaHeader setMinApplicableVersionSelector (toNSNumber value)

-- | If not nil, specifies the largest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- maxApplicableVersion@
maxApplicableVersion :: IsMTROTAHeader mtrotaHeader => mtrotaHeader -> IO (Id NSNumber)
maxApplicableVersion mtrotaHeader =
  sendMessage mtrotaHeader maxApplicableVersionSelector

-- | If not nil, specifies the largest software version that this update can be applied on top of.  In that case, this value must be compared to the SoftwareVersion in the QueryImage command to check whether this image is valid for the OTA requestor.
--
-- ObjC selector: @- setMaxApplicableVersion:@
setMaxApplicableVersion :: (IsMTROTAHeader mtrotaHeader, IsNSNumber value) => mtrotaHeader -> value -> IO ()
setMaxApplicableVersion mtrotaHeader value =
  sendMessage mtrotaHeader setMaxApplicableVersionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id MTROTAHeader)
initWithDataSelector = mkSelector "initWithData:"

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

-- | @Selector@ for @payloadSize@
payloadSizeSelector :: Selector '[] (Id NSNumber)
payloadSizeSelector = mkSelector "payloadSize"

-- | @Selector@ for @setPayloadSize:@
setPayloadSizeSelector :: Selector '[Id NSNumber] ()
setPayloadSizeSelector = mkSelector "setPayloadSize:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @softwareVersionString@
softwareVersionStringSelector :: Selector '[] (Id NSString)
softwareVersionStringSelector = mkSelector "softwareVersionString"

-- | @Selector@ for @setSoftwareVersionString:@
setSoftwareVersionStringSelector :: Selector '[Id NSString] ()
setSoftwareVersionStringSelector = mkSelector "setSoftwareVersionString:"

-- | @Selector@ for @releaseNotesURL@
releaseNotesURLSelector :: Selector '[] (Id NSString)
releaseNotesURLSelector = mkSelector "releaseNotesURL"

-- | @Selector@ for @setReleaseNotesURL:@
setReleaseNotesURLSelector :: Selector '[Id NSString] ()
setReleaseNotesURLSelector = mkSelector "setReleaseNotesURL:"

-- | @Selector@ for @imageDigest@
imageDigestSelector :: Selector '[] (Id NSData)
imageDigestSelector = mkSelector "imageDigest"

-- | @Selector@ for @setImageDigest:@
setImageDigestSelector :: Selector '[Id NSData] ()
setImageDigestSelector = mkSelector "setImageDigest:"

-- | @Selector@ for @imageDigestType@
imageDigestTypeSelector :: Selector '[] MTROTAImageDigestType
imageDigestTypeSelector = mkSelector "imageDigestType"

-- | @Selector@ for @setImageDigestType:@
setImageDigestTypeSelector :: Selector '[MTROTAImageDigestType] ()
setImageDigestTypeSelector = mkSelector "setImageDigestType:"

-- | @Selector@ for @minApplicableVersion@
minApplicableVersionSelector :: Selector '[] (Id NSNumber)
minApplicableVersionSelector = mkSelector "minApplicableVersion"

-- | @Selector@ for @setMinApplicableVersion:@
setMinApplicableVersionSelector :: Selector '[Id NSNumber] ()
setMinApplicableVersionSelector = mkSelector "setMinApplicableVersion:"

-- | @Selector@ for @maxApplicableVersion@
maxApplicableVersionSelector :: Selector '[] (Id NSNumber)
maxApplicableVersionSelector = mkSelector "maxApplicableVersion"

-- | @Selector@ for @setMaxApplicableVersion:@
setMaxApplicableVersionSelector :: Selector '[Id NSNumber] ()
setMaxApplicableVersionSelector = mkSelector "setMaxApplicableVersion:"

