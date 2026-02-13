{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete subclass the Core Image Barcode Descriptor that represents an Aztec code symbol.
--
-- An Aztec code symbol is a 2D barcode format defined by the ISO/IEC 24778:2008 standard.  It encodes data in concentric square rings around a central bullseye pattern.
--
-- Generated bindings for @CIAztecCodeDescriptor@.
module ObjC.CoreImage.CIAztecCodeDescriptor
  ( CIAztecCodeDescriptor
  , IsCIAztecCodeDescriptor(..)
  , initWithPayload_isCompact_layerCount_dataCodewordCount
  , descriptorWithPayload_isCompact_layerCount_dataCodewordCount
  , errorCorrectedPayload
  , isCompact
  , layerCount
  , dataCodewordCount
  , dataCodewordCountSelector
  , descriptorWithPayload_isCompact_layerCount_dataCodewordCountSelector
  , errorCorrectedPayloadSelector
  , initWithPayload_isCompact_layerCount_dataCodewordCountSelector
  , isCompactSelector
  , layerCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes an Aztec code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the Aztec code symbol.   - isCompact: A Boolean indicating whether or not the Aztec code is compact.   - layerCount: The number of layers in the Aztec code, from 1 to 32.   - dataCodewordCount: The number of codewords in the Aztec code, from 1 to 2048. - Returns:     An initialized ``CIAztecCodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @- initWithPayload:isCompact:layerCount:dataCodewordCount:@
initWithPayload_isCompact_layerCount_dataCodewordCount :: (IsCIAztecCodeDescriptor ciAztecCodeDescriptor, IsNSData errorCorrectedPayload) => ciAztecCodeDescriptor -> errorCorrectedPayload -> Bool -> CLong -> CLong -> IO (Id CIAztecCodeDescriptor)
initWithPayload_isCompact_layerCount_dataCodewordCount ciAztecCodeDescriptor errorCorrectedPayload isCompact layerCount dataCodewordCount =
  sendOwnedMessage ciAztecCodeDescriptor initWithPayload_isCompact_layerCount_dataCodewordCountSelector (toNSData errorCorrectedPayload) isCompact layerCount dataCodewordCount

-- | Creates an Aztec code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the Aztec code symbol.   - isCompact: A Boolean indicating whether or not the Aztec code is compact.   - layerCount: The number of layers in the Aztec code, from 1 to 32.   - dataCodewordCount: The number of codewords in the Aztec code, from 1 to 2048. - Returns:     An autoreleased ``CIAztecCodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @+ descriptorWithPayload:isCompact:layerCount:dataCodewordCount:@
descriptorWithPayload_isCompact_layerCount_dataCodewordCount :: IsNSData errorCorrectedPayload => errorCorrectedPayload -> Bool -> CLong -> CLong -> IO (Id CIAztecCodeDescriptor)
descriptorWithPayload_isCompact_layerCount_dataCodewordCount errorCorrectedPayload isCompact layerCount dataCodewordCount =
  do
    cls' <- getRequiredClass "CIAztecCodeDescriptor"
    sendClassMessage cls' descriptorWithPayload_isCompact_layerCount_dataCodewordCountSelector (toNSData errorCorrectedPayload) isCompact layerCount dataCodewordCount

-- | The error-corrected payload that comprises the the Aztec code symbol.
--
-- Aztec Codes are formally specified in ISO/IEC 24778:2008(E).
--
-- The error corrected payload consists of the 6-, 8-, 10-, or 12-bit message codewords produced  at the end of the step described in section 7.3.1.2 "Formation of data codewords", which exists immediately prior to adding error correction. These codewords have dummy bits inserted to ensure that an entire codeword isn't all 0's or all 1's. Clients will need to remove these extra bits as part of interpreting the payload.
--
-- ObjC selector: @- errorCorrectedPayload@
errorCorrectedPayload :: IsCIAztecCodeDescriptor ciAztecCodeDescriptor => ciAztecCodeDescriptor -> IO (Id NSData)
errorCorrectedPayload ciAztecCodeDescriptor =
  sendMessage ciAztecCodeDescriptor errorCorrectedPayloadSelector

-- | A Boolean value telling if the Aztec code is compact.
--
-- Compact Aztec symbols use one-fewer ring in the central finder pattern than full-range  Aztec symbols of the same number of data layers.
--
-- ObjC selector: @- isCompact@
isCompact :: IsCIAztecCodeDescriptor ciAztecCodeDescriptor => ciAztecCodeDescriptor -> IO Bool
isCompact ciAztecCodeDescriptor =
  sendMessage ciAztecCodeDescriptor isCompactSelector

-- | The number of data layers in the Aztec code symbol.
--
-- Combined with ``isCompact-property``, the number of data layers determines the number of   modules in the Aztec Code symbol. Valid values range from 1 to 32. Compact symbols can have  up to 4 data layers.
--
-- The number of data layers also determines the number of bits in each data codeword of the message carried by the Aztec Code symbol.
--
-- ObjC selector: @- layerCount@
layerCount :: IsCIAztecCodeDescriptor ciAztecCodeDescriptor => ciAztecCodeDescriptor -> IO CLong
layerCount ciAztecCodeDescriptor =
  sendMessage ciAztecCodeDescriptor layerCountSelector

-- | The number of non-error-correction codewords carried by the Aztec code symbol.
--
-- Used to determine the level of error correction in conjunction with the number of data layers.  Valid values are 1 to 2048. Compact symbols can have up to 64 message codewords.
--
-- > Note: this value can exceed the number of message codewords allowed by the number of data  layers in this symbol. In this case, the actual number of message codewords is 1024 fewer than this value and the message payload is to be interpreted in an application-defined manner.
--
-- ObjC selector: @- dataCodewordCount@
dataCodewordCount :: IsCIAztecCodeDescriptor ciAztecCodeDescriptor => ciAztecCodeDescriptor -> IO CLong
dataCodewordCount ciAztecCodeDescriptor =
  sendMessage ciAztecCodeDescriptor dataCodewordCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayload:isCompact:layerCount:dataCodewordCount:@
initWithPayload_isCompact_layerCount_dataCodewordCountSelector :: Selector '[Id NSData, Bool, CLong, CLong] (Id CIAztecCodeDescriptor)
initWithPayload_isCompact_layerCount_dataCodewordCountSelector = mkSelector "initWithPayload:isCompact:layerCount:dataCodewordCount:"

-- | @Selector@ for @descriptorWithPayload:isCompact:layerCount:dataCodewordCount:@
descriptorWithPayload_isCompact_layerCount_dataCodewordCountSelector :: Selector '[Id NSData, Bool, CLong, CLong] (Id CIAztecCodeDescriptor)
descriptorWithPayload_isCompact_layerCount_dataCodewordCountSelector = mkSelector "descriptorWithPayload:isCompact:layerCount:dataCodewordCount:"

-- | @Selector@ for @errorCorrectedPayload@
errorCorrectedPayloadSelector :: Selector '[] (Id NSData)
errorCorrectedPayloadSelector = mkSelector "errorCorrectedPayload"

-- | @Selector@ for @isCompact@
isCompactSelector :: Selector '[] Bool
isCompactSelector = mkSelector "isCompact"

-- | @Selector@ for @layerCount@
layerCountSelector :: Selector '[] CLong
layerCountSelector = mkSelector "layerCount"

-- | @Selector@ for @dataCodewordCount@
dataCodewordCountSelector :: Selector '[] CLong
dataCodewordCountSelector = mkSelector "dataCodewordCount"

