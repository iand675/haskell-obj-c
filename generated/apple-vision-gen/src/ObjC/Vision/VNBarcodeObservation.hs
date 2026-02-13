{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNBarcodeObservation
--
-- VNRectangleObservation
--
-- VNBarcodeObservation Describes an area containing a barcode detected by the VNRequestNameDetectBarcodes request.
--
-- Generated bindings for @VNBarcodeObservation@.
module ObjC.Vision.VNBarcodeObservation
  ( VNBarcodeObservation
  , IsVNBarcodeObservation(..)
  , symbology
  , barcodeDescriptor
  , payloadStringValue
  , payloadData
  , isGS1DataCarrier
  , isColorInverted
  , supplementalCompositeType
  , supplementalPayloadString
  , supplementalPayloadData
  , barcodeDescriptorSelector
  , isColorInvertedSelector
  , isGS1DataCarrierSelector
  , payloadDataSelector
  , payloadStringValueSelector
  , supplementalCompositeTypeSelector
  , supplementalPayloadDataSelector
  , supplementalPayloadStringSelector
  , symbologySelector

  -- * Enum types
  , VNBarcodeCompositeType(VNBarcodeCompositeType)
  , pattern VNBarcodeCompositeTypeNone
  , pattern VNBarcodeCompositeTypeLinked
  , pattern VNBarcodeCompositeTypeGS1TypeA
  , pattern VNBarcodeCompositeTypeGS1TypeB
  , pattern VNBarcodeCompositeTypeGS1TypeC

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The symbology of the detected barcode.
--
-- ObjC selector: @- symbology@
symbology :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSString)
symbology vnBarcodeObservation =
  sendMessage vnBarcodeObservation symbologySelector

-- | An object that provides symbology-specific data for the barcode.
--
-- ObjC selector: @- barcodeDescriptor@
barcodeDescriptor :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id CIBarcodeDescriptor)
barcodeDescriptor vnBarcodeObservation =
  sendMessage vnBarcodeObservation barcodeDescriptorSelector

-- | The string representation of the barcode's payload.  Depending on the symbology of the barcode and/or the payload data itself, a string representation of the payload may not be available.
--
-- ObjC selector: @- payloadStringValue@
payloadStringValue :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSString)
payloadStringValue vnBarcodeObservation =
  sendMessage vnBarcodeObservation payloadStringValueSelector

-- | The raw data representation of the barcode's payload if available.
--
-- ObjC selector: @- payloadData@
payloadData :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSData)
payloadData vnBarcodeObservation =
  sendMessage vnBarcodeObservation payloadDataSelector

-- | Boolean indicating if the barcode carries any GS1 application specific data
--
-- ObjC selector: @- isGS1DataCarrier@
isGS1DataCarrier :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO Bool
isGS1DataCarrier vnBarcodeObservation =
  sendMessage vnBarcodeObservation isGS1DataCarrierSelector

-- | A boolean indicating if the barcode is color inverted
--
-- ObjC selector: @- isColorInverted@
isColorInverted :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO Bool
isColorInverted vnBarcodeObservation =
  sendMessage vnBarcodeObservation isColorInvertedSelector

-- | Represents the supplemental composite type. Currently, this can only refer to the composite flag of the 2D symbology as part of a GS1 composite symbology. This attribute only exists when the primary descriptor is the 1D symbology of a GS1 composite symbology, and of which a valid 2D counterpart has been coalesced into.
--
-- ObjC selector: @- supplementalCompositeType@
supplementalCompositeType :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO VNBarcodeCompositeType
supplementalCompositeType vnBarcodeObservation =
  sendMessage vnBarcodeObservation supplementalCompositeTypeSelector

-- | Decode the supplemental code in the descriptor as a string value. Note: this property might be expensive the first time it is accessed When non-NULL, and if the descriptor has supplemental raw payload data, the pointee will be set to the decoded supplemental payload string value.
--
-- ObjC selector: @- supplementalPayloadString@
supplementalPayloadString :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSString)
supplementalPayloadString vnBarcodeObservation =
  sendMessage vnBarcodeObservation supplementalPayloadStringSelector

-- | Decode the supplemental code in the descriptor as a string value. Note: this property might be expensive the first time it is accessed When non-NULL, and if the descriptor has supplemental raw payload data, the pointee will be set to the decoded supplemental payload raw data value.
--
-- ObjC selector: @- supplementalPayloadData@
supplementalPayloadData :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSData)
supplementalPayloadData vnBarcodeObservation =
  sendMessage vnBarcodeObservation supplementalPayloadDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @symbology@
symbologySelector :: Selector '[] (Id NSString)
symbologySelector = mkSelector "symbology"

-- | @Selector@ for @barcodeDescriptor@
barcodeDescriptorSelector :: Selector '[] (Id CIBarcodeDescriptor)
barcodeDescriptorSelector = mkSelector "barcodeDescriptor"

-- | @Selector@ for @payloadStringValue@
payloadStringValueSelector :: Selector '[] (Id NSString)
payloadStringValueSelector = mkSelector "payloadStringValue"

-- | @Selector@ for @payloadData@
payloadDataSelector :: Selector '[] (Id NSData)
payloadDataSelector = mkSelector "payloadData"

-- | @Selector@ for @isGS1DataCarrier@
isGS1DataCarrierSelector :: Selector '[] Bool
isGS1DataCarrierSelector = mkSelector "isGS1DataCarrier"

-- | @Selector@ for @isColorInverted@
isColorInvertedSelector :: Selector '[] Bool
isColorInvertedSelector = mkSelector "isColorInverted"

-- | @Selector@ for @supplementalCompositeType@
supplementalCompositeTypeSelector :: Selector '[] VNBarcodeCompositeType
supplementalCompositeTypeSelector = mkSelector "supplementalCompositeType"

-- | @Selector@ for @supplementalPayloadString@
supplementalPayloadStringSelector :: Selector '[] (Id NSString)
supplementalPayloadStringSelector = mkSelector "supplementalPayloadString"

-- | @Selector@ for @supplementalPayloadData@
supplementalPayloadDataSelector :: Selector '[] (Id NSData)
supplementalPayloadDataSelector = mkSelector "supplementalPayloadData"

