{-# LANGUAGE PatternSynonyms #-}
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
  , symbologySelector
  , barcodeDescriptorSelector
  , payloadStringValueSelector
  , payloadDataSelector
  , isGS1DataCarrierSelector
  , isColorInvertedSelector
  , supplementalCompositeTypeSelector
  , supplementalPayloadStringSelector
  , supplementalPayloadDataSelector

  -- * Enum types
  , VNBarcodeCompositeType(VNBarcodeCompositeType)
  , pattern VNBarcodeCompositeTypeNone
  , pattern VNBarcodeCompositeTypeLinked
  , pattern VNBarcodeCompositeTypeGS1TypeA
  , pattern VNBarcodeCompositeTypeGS1TypeB
  , pattern VNBarcodeCompositeTypeGS1TypeC

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

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The symbology of the detected barcode.
--
-- ObjC selector: @- symbology@
symbology :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSString)
symbology vnBarcodeObservation  =
  sendMsg vnBarcodeObservation (mkSelector "symbology") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An object that provides symbology-specific data for the barcode.
--
-- ObjC selector: @- barcodeDescriptor@
barcodeDescriptor :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id CIBarcodeDescriptor)
barcodeDescriptor vnBarcodeObservation  =
  sendMsg vnBarcodeObservation (mkSelector "barcodeDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The string representation of the barcode's payload.  Depending on the symbology of the barcode and/or the payload data itself, a string representation of the payload may not be available.
--
-- ObjC selector: @- payloadStringValue@
payloadStringValue :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSString)
payloadStringValue vnBarcodeObservation  =
  sendMsg vnBarcodeObservation (mkSelector "payloadStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The raw data representation of the barcode's payload if available.
--
-- ObjC selector: @- payloadData@
payloadData :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSData)
payloadData vnBarcodeObservation  =
  sendMsg vnBarcodeObservation (mkSelector "payloadData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Boolean indicating if the barcode carries any GS1 application specific data
--
-- ObjC selector: @- isGS1DataCarrier@
isGS1DataCarrier :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO Bool
isGS1DataCarrier vnBarcodeObservation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnBarcodeObservation (mkSelector "isGS1DataCarrier") retCULong []

-- | A boolean indicating if the barcode is color inverted
--
-- ObjC selector: @- isColorInverted@
isColorInverted :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO Bool
isColorInverted vnBarcodeObservation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnBarcodeObservation (mkSelector "isColorInverted") retCULong []

-- | Represents the supplemental composite type. Currently, this can only refer to the composite flag of the 2D symbology as part of a GS1 composite symbology. This attribute only exists when the primary descriptor is the 1D symbology of a GS1 composite symbology, and of which a valid 2D counterpart has been coalesced into.
--
-- ObjC selector: @- supplementalCompositeType@
supplementalCompositeType :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO VNBarcodeCompositeType
supplementalCompositeType vnBarcodeObservation  =
  fmap (coerce :: CLong -> VNBarcodeCompositeType) $ sendMsg vnBarcodeObservation (mkSelector "supplementalCompositeType") retCLong []

-- | Decode the supplemental code in the descriptor as a string value. Note: this property might be expensive the first time it is accessed When non-NULL, and if the descriptor has supplemental raw payload data, the pointee will be set to the decoded supplemental payload string value.
--
-- ObjC selector: @- supplementalPayloadString@
supplementalPayloadString :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSString)
supplementalPayloadString vnBarcodeObservation  =
  sendMsg vnBarcodeObservation (mkSelector "supplementalPayloadString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Decode the supplemental code in the descriptor as a string value. Note: this property might be expensive the first time it is accessed When non-NULL, and if the descriptor has supplemental raw payload data, the pointee will be set to the decoded supplemental payload raw data value.
--
-- ObjC selector: @- supplementalPayloadData@
supplementalPayloadData :: IsVNBarcodeObservation vnBarcodeObservation => vnBarcodeObservation -> IO (Id NSData)
supplementalPayloadData vnBarcodeObservation  =
  sendMsg vnBarcodeObservation (mkSelector "supplementalPayloadData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @symbology@
symbologySelector :: Selector
symbologySelector = mkSelector "symbology"

-- | @Selector@ for @barcodeDescriptor@
barcodeDescriptorSelector :: Selector
barcodeDescriptorSelector = mkSelector "barcodeDescriptor"

-- | @Selector@ for @payloadStringValue@
payloadStringValueSelector :: Selector
payloadStringValueSelector = mkSelector "payloadStringValue"

-- | @Selector@ for @payloadData@
payloadDataSelector :: Selector
payloadDataSelector = mkSelector "payloadData"

-- | @Selector@ for @isGS1DataCarrier@
isGS1DataCarrierSelector :: Selector
isGS1DataCarrierSelector = mkSelector "isGS1DataCarrier"

-- | @Selector@ for @isColorInverted@
isColorInvertedSelector :: Selector
isColorInvertedSelector = mkSelector "isColorInverted"

-- | @Selector@ for @supplementalCompositeType@
supplementalCompositeTypeSelector :: Selector
supplementalCompositeTypeSelector = mkSelector "supplementalCompositeType"

-- | @Selector@ for @supplementalPayloadString@
supplementalPayloadStringSelector :: Selector
supplementalPayloadStringSelector = mkSelector "supplementalPayloadString"

-- | @Selector@ for @supplementalPayloadData@
supplementalPayloadDataSelector :: Selector
supplementalPayloadDataSelector = mkSelector "supplementalPayloadData"

