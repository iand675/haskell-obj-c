{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete subclass of the Core Image Barcode Descriptor that represents a square QR code symbol.
--
-- ISO/IEC 18004 defines versions from 1 to 40, where a higher symbol version indicates a  larger data-carrying capacity. QR Codes can encode text, vCard contact information, or Uniform Resource Identifiers (URI).
--
-- Generated bindings for @CIQRCodeDescriptor@.
module ObjC.CoreImage.CIQRCodeDescriptor
  ( CIQRCodeDescriptor
  , IsCIQRCodeDescriptor(..)
  , initWithPayload_symbolVersion_maskPattern_errorCorrectionLevel
  , descriptorWithPayload_symbolVersion_maskPattern_errorCorrectionLevel
  , errorCorrectedPayload
  , symbolVersion
  , maskPattern
  , errorCorrectionLevel
  , initWithPayload_symbolVersion_maskPattern_errorCorrectionLevelSelector
  , descriptorWithPayload_symbolVersion_maskPattern_errorCorrectionLevelSelector
  , errorCorrectedPayloadSelector
  , symbolVersionSelector
  , maskPatternSelector
  , errorCorrectionLevelSelector

  -- * Enum types
  , CIQRCodeErrorCorrectionLevel(CIQRCodeErrorCorrectionLevel)
  , pattern CIQRCodeErrorCorrectionLevelL
  , pattern CIQRCodeErrorCorrectionLevelM
  , pattern CIQRCodeErrorCorrectionLevelQ
  , pattern CIQRCodeErrorCorrectionLevelH

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

import ObjC.CoreImage.Internal.Classes
import ObjC.CoreImage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a QR code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the QR code symbol.   - symbolVersion: The symbol version, from 1 through 40.   - maskPattern: The mask pattern to use in the QR code, from 0 to 7.   - errorCorrectionLevel: The QR code's error correction level: L, M, Q, or H. - Returns:     An initialized ``CIAztecCodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @- initWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:@
initWithPayload_symbolVersion_maskPattern_errorCorrectionLevel :: (IsCIQRCodeDescriptor ciqrCodeDescriptor, IsNSData errorCorrectedPayload) => ciqrCodeDescriptor -> errorCorrectedPayload -> CLong -> CUChar -> CIQRCodeErrorCorrectionLevel -> IO (Id CIQRCodeDescriptor)
initWithPayload_symbolVersion_maskPattern_errorCorrectionLevel ciqrCodeDescriptor  errorCorrectedPayload symbolVersion maskPattern errorCorrectionLevel =
withObjCPtr errorCorrectedPayload $ \raw_errorCorrectedPayload ->
    sendMsg ciqrCodeDescriptor (mkSelector "initWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:") (retPtr retVoid) [argPtr (castPtr raw_errorCorrectedPayload :: Ptr ()), argCLong (fromIntegral symbolVersion), argCUChar (fromIntegral maskPattern), argCLong (coerce errorCorrectionLevel)] >>= ownedObject . castPtr

-- | Creates a QR code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the QR code symbol.   - symbolVersion: The symbol version, from 1 through 40.   - maskPattern: The mask pattern to use in the QR code, from 0 to 7.   - errorCorrectionLevel: The QR code's error correction level: L, M, Q, or H. - Returns:     An autoreleased ``CIAztecCodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @+ descriptorWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:@
descriptorWithPayload_symbolVersion_maskPattern_errorCorrectionLevel :: IsNSData errorCorrectedPayload => errorCorrectedPayload -> CLong -> CUChar -> CIQRCodeErrorCorrectionLevel -> IO (Id CIQRCodeDescriptor)
descriptorWithPayload_symbolVersion_maskPattern_errorCorrectionLevel errorCorrectedPayload symbolVersion maskPattern errorCorrectionLevel =
  do
    cls' <- getRequiredClass "CIQRCodeDescriptor"
    withObjCPtr errorCorrectedPayload $ \raw_errorCorrectedPayload ->
      sendClassMsg cls' (mkSelector "descriptorWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:") (retPtr retVoid) [argPtr (castPtr raw_errorCorrectedPayload :: Ptr ()), argCLong (fromIntegral symbolVersion), argCUChar (fromIntegral maskPattern), argCLong (coerce errorCorrectionLevel)] >>= retainedObject . castPtr

-- | The error-corrected codeword payload that comprises the QR code symbol.
--
-- QR Codes are formally specified in ISO/IEC 18004:2006(E).  Section 6.4.10 "Bitstream to codeword conversion" specifies the set of 8-bit codewords in the symbol  immediately prior to splitting the message into blocks and applying error correction.
--
-- During decode, error correction is applied and if successful, the message is re-ordered to the state immediately  following "Bitstream to codeword conversion."
--
-- The @errorCorrectedPayload@ corresponds to this sequence of 8-bit codewords.
--
-- ObjC selector: @- errorCorrectedPayload@
errorCorrectedPayload :: IsCIQRCodeDescriptor ciqrCodeDescriptor => ciqrCodeDescriptor -> IO (Id NSData)
errorCorrectedPayload ciqrCodeDescriptor  =
  sendMsg ciqrCodeDescriptor (mkSelector "errorCorrectedPayload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of the QR code which corresponds to the size of the QR code symbol.
--
-- ISO/IEC 18004 defines versions from 1 to 40, where a higher symbol version indicates a larger data-carrying capacity. This field is required in order to properly interpret the error corrected payload.
--
-- ObjC selector: @- symbolVersion@
symbolVersion :: IsCIQRCodeDescriptor ciqrCodeDescriptor => ciqrCodeDescriptor -> IO CLong
symbolVersion ciqrCodeDescriptor  =
  sendMsg ciqrCodeDescriptor (mkSelector "symbolVersion") retCLong []

-- | The data mask pattern for the QR code symbol.
--
-- QR Codes support eight data mask patterns, which are used to avoid large black or large white areas inside the symbol body.  Valid values range from 0 to 7.
--
-- ObjC selector: @- maskPattern@
maskPattern :: IsCIQRCodeDescriptor ciqrCodeDescriptor => ciqrCodeDescriptor -> IO CUChar
maskPattern ciqrCodeDescriptor  =
  sendMsg ciqrCodeDescriptor (mkSelector "maskPattern") retCUChar []

-- | The error correction level of the QR code symbol.
--
-- QR Codes support four levels of Reed-Solomon error correction.
--
-- The possible error correction levels are enumerated in ``CIDataMatrixCodeECCVersion``.
--
-- ObjC selector: @- errorCorrectionLevel@
errorCorrectionLevel :: IsCIQRCodeDescriptor ciqrCodeDescriptor => ciqrCodeDescriptor -> IO CIQRCodeErrorCorrectionLevel
errorCorrectionLevel ciqrCodeDescriptor  =
  fmap (coerce :: CLong -> CIQRCodeErrorCorrectionLevel) $ sendMsg ciqrCodeDescriptor (mkSelector "errorCorrectionLevel") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:@
initWithPayload_symbolVersion_maskPattern_errorCorrectionLevelSelector :: Selector
initWithPayload_symbolVersion_maskPattern_errorCorrectionLevelSelector = mkSelector "initWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:"

-- | @Selector@ for @descriptorWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:@
descriptorWithPayload_symbolVersion_maskPattern_errorCorrectionLevelSelector :: Selector
descriptorWithPayload_symbolVersion_maskPattern_errorCorrectionLevelSelector = mkSelector "descriptorWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:"

-- | @Selector@ for @errorCorrectedPayload@
errorCorrectedPayloadSelector :: Selector
errorCorrectedPayloadSelector = mkSelector "errorCorrectedPayload"

-- | @Selector@ for @symbolVersion@
symbolVersionSelector :: Selector
symbolVersionSelector = mkSelector "symbolVersion"

-- | @Selector@ for @maskPattern@
maskPatternSelector :: Selector
maskPatternSelector = mkSelector "maskPattern"

-- | @Selector@ for @errorCorrectionLevel@
errorCorrectionLevelSelector :: Selector
errorCorrectionLevelSelector = mkSelector "errorCorrectionLevel"

