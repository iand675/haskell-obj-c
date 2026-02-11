{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete subclass the Core Image Barcode Descriptor that represents an Data Matrix code symbol.
--
-- A Data Matrix code symbol is a 2D barcode format defined by the ISO/IEC 16022:2006(E) standard.  It encodes data in square or rectangular symbol with solid lines on the left and bottom sides
--
-- Generated bindings for @CIDataMatrixCodeDescriptor@.
module ObjC.CoreImage.CIDataMatrixCodeDescriptor
  ( CIDataMatrixCodeDescriptor
  , IsCIDataMatrixCodeDescriptor(..)
  , initWithPayload_rowCount_columnCount_eccVersion
  , descriptorWithPayload_rowCount_columnCount_eccVersion
  , errorCorrectedPayload
  , rowCount
  , columnCount
  , eccVersion
  , initWithPayload_rowCount_columnCount_eccVersionSelector
  , descriptorWithPayload_rowCount_columnCount_eccVersionSelector
  , errorCorrectedPayloadSelector
  , rowCountSelector
  , columnCountSelector
  , eccVersionSelector

  -- * Enum types
  , CIDataMatrixCodeECCVersion(CIDataMatrixCodeECCVersion)
  , pattern CIDataMatrixCodeECCVersion000
  , pattern CIDataMatrixCodeECCVersion050
  , pattern CIDataMatrixCodeECCVersion080
  , pattern CIDataMatrixCodeECCVersion100
  , pattern CIDataMatrixCodeECCVersion140
  , pattern CIDataMatrixCodeECCVersion200

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

-- | Initializes a Data Matrix code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the Data Matrix code symbol.   - rowCount: The number of rows in the Data Matrix code symbol.   - columnCount: The number of columns in the Data Matrix code symbol.   - eccVersion: The ``CIDataMatrixCodeECCVersion`` for the Data Matrix code symbol. - Returns:     An initialized ``CIAztecCodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @- initWithPayload:rowCount:columnCount:eccVersion:@
initWithPayload_rowCount_columnCount_eccVersion :: (IsCIDataMatrixCodeDescriptor ciDataMatrixCodeDescriptor, IsNSData errorCorrectedPayload) => ciDataMatrixCodeDescriptor -> errorCorrectedPayload -> CLong -> CLong -> CIDataMatrixCodeECCVersion -> IO (Id CIDataMatrixCodeDescriptor)
initWithPayload_rowCount_columnCount_eccVersion ciDataMatrixCodeDescriptor  errorCorrectedPayload rowCount columnCount eccVersion =
withObjCPtr errorCorrectedPayload $ \raw_errorCorrectedPayload ->
    sendMsg ciDataMatrixCodeDescriptor (mkSelector "initWithPayload:rowCount:columnCount:eccVersion:") (retPtr retVoid) [argPtr (castPtr raw_errorCorrectedPayload :: Ptr ()), argCLong (fromIntegral rowCount), argCLong (fromIntegral columnCount), argCLong (coerce eccVersion)] >>= ownedObject . castPtr

-- | Creates a Data Matrix code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the Data Matrix code symbol.   - rowCount: The number of rows in the Data Matrix code symbol.   - columnCount: The number of columns in the Data Matrix code symbol.   - eccVersion: The ``CIDataMatrixCodeECCVersion`` for the Data Matrix code symbol. - Returns:     An autoreleased ``CIAztecCodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @+ descriptorWithPayload:rowCount:columnCount:eccVersion:@
descriptorWithPayload_rowCount_columnCount_eccVersion :: IsNSData errorCorrectedPayload => errorCorrectedPayload -> CLong -> CLong -> CIDataMatrixCodeECCVersion -> IO (Id CIDataMatrixCodeDescriptor)
descriptorWithPayload_rowCount_columnCount_eccVersion errorCorrectedPayload rowCount columnCount eccVersion =
  do
    cls' <- getRequiredClass "CIDataMatrixCodeDescriptor"
    withObjCPtr errorCorrectedPayload $ \raw_errorCorrectedPayload ->
      sendClassMsg cls' (mkSelector "descriptorWithPayload:rowCount:columnCount:eccVersion:") (retPtr retVoid) [argPtr (castPtr raw_errorCorrectedPayload :: Ptr ()), argCLong (fromIntegral rowCount), argCLong (fromIntegral columnCount), argCLong (coerce eccVersion)] >>= retainedObject . castPtr

-- | The error-corrected payload containing the data encoded in the Data Matrix code symbol.
--
-- DataMatrix symbols are specified bn ISO/IEC 16022:2006(E). ECC 200-type symbols will always  have an even number of rows and columns.
--
-- For ECC 200-type symbols, the phases of encoding data into a symbol are described in  section 5.1 -- Encode procedure overview. The error corrected payload comprises the  de-interleaved bits of the message described at the end of Step 1: Data encodation.
--
-- ObjC selector: @- errorCorrectedPayload@
errorCorrectedPayload :: IsCIDataMatrixCodeDescriptor ciDataMatrixCodeDescriptor => ciDataMatrixCodeDescriptor -> IO (Id NSData)
errorCorrectedPayload ciDataMatrixCodeDescriptor  =
  sendMsg ciDataMatrixCodeDescriptor (mkSelector "errorCorrectedPayload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The number of rows in the Data Matrix code symbol.
--
-- Refer to ISO/IEC 16022:2006(E) for valid module row and column count combinations.
--
-- ObjC selector: @- rowCount@
rowCount :: IsCIDataMatrixCodeDescriptor ciDataMatrixCodeDescriptor => ciDataMatrixCodeDescriptor -> IO CLong
rowCount ciDataMatrixCodeDescriptor  =
  sendMsg ciDataMatrixCodeDescriptor (mkSelector "rowCount") retCLong []

-- | The number of columns in the Data Matrix code symbol.
--
-- Refer to ISO/IEC 16022:2006(E) for valid module row and column count combinations.
--
-- ObjC selector: @- columnCount@
columnCount :: IsCIDataMatrixCodeDescriptor ciDataMatrixCodeDescriptor => ciDataMatrixCodeDescriptor -> IO CLong
columnCount ciDataMatrixCodeDescriptor  =
  sendMsg ciDataMatrixCodeDescriptor (mkSelector "columnCount") retCLong []

-- | The error correction version of the Data Matrix code symbol.
--
-- The possible error correction version are enumerated in ``CIDataMatrixCodeECCVersion``. Any symbol with an even number of rows and columns will be ECC 200.
--
-- ObjC selector: @- eccVersion@
eccVersion :: IsCIDataMatrixCodeDescriptor ciDataMatrixCodeDescriptor => ciDataMatrixCodeDescriptor -> IO CIDataMatrixCodeECCVersion
eccVersion ciDataMatrixCodeDescriptor  =
  fmap (coerce :: CLong -> CIDataMatrixCodeECCVersion) $ sendMsg ciDataMatrixCodeDescriptor (mkSelector "eccVersion") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayload:rowCount:columnCount:eccVersion:@
initWithPayload_rowCount_columnCount_eccVersionSelector :: Selector
initWithPayload_rowCount_columnCount_eccVersionSelector = mkSelector "initWithPayload:rowCount:columnCount:eccVersion:"

-- | @Selector@ for @descriptorWithPayload:rowCount:columnCount:eccVersion:@
descriptorWithPayload_rowCount_columnCount_eccVersionSelector :: Selector
descriptorWithPayload_rowCount_columnCount_eccVersionSelector = mkSelector "descriptorWithPayload:rowCount:columnCount:eccVersion:"

-- | @Selector@ for @errorCorrectedPayload@
errorCorrectedPayloadSelector :: Selector
errorCorrectedPayloadSelector = mkSelector "errorCorrectedPayload"

-- | @Selector@ for @rowCount@
rowCountSelector :: Selector
rowCountSelector = mkSelector "rowCount"

-- | @Selector@ for @columnCount@
columnCountSelector :: Selector
columnCountSelector = mkSelector "columnCount"

-- | @Selector@ for @eccVersion@
eccVersionSelector :: Selector
eccVersionSelector = mkSelector "eccVersion"

