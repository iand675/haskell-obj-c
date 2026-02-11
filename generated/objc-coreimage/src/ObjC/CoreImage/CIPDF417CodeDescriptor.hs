{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete subclass of Core Image Barcode Descriptor that represents a PDF417 symbol.
--
-- PDF417 is a stacked linear barcode symbol format used predominantly in transport, ID cards,  and inventory management. Each pattern in the code comprises 4 bars and spaces, 17 units long.
--
-- Refer to the ISO/IEC 15438:2006(E) for the PDF417 symbol specification.
--
-- Generated bindings for @CIPDF417CodeDescriptor@.
module ObjC.CoreImage.CIPDF417CodeDescriptor
  ( CIPDF417CodeDescriptor
  , IsCIPDF417CodeDescriptor(..)
  , initWithPayload_isCompact_rowCount_columnCount
  , descriptorWithPayload_isCompact_rowCount_columnCount
  , errorCorrectedPayload
  , isCompact
  , rowCount
  , columnCount
  , initWithPayload_isCompact_rowCount_columnCountSelector
  , descriptorWithPayload_isCompact_rowCount_columnCountSelector
  , errorCorrectedPayloadSelector
  , isCompactSelector
  , rowCountSelector
  , columnCountSelector


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
import ObjC.Foundation.Internal.Classes

-- | Initializes an PDF417 code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the PDF417 code symbol.   - isCompact: A Boolean indicating whether or not the PDF417 code is compact.   - rowCount: The number of rows in the PDF417 code, from 3 to 90.   - columnCount: The number of columns in the Aztec code, from 1 to 30. - Returns:     An initialized ``CIPDF417CodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @- initWithPayload:isCompact:rowCount:columnCount:@
initWithPayload_isCompact_rowCount_columnCount :: (IsCIPDF417CodeDescriptor cipdF417CodeDescriptor, IsNSData errorCorrectedPayload) => cipdF417CodeDescriptor -> errorCorrectedPayload -> Bool -> CLong -> CLong -> IO (Id CIPDF417CodeDescriptor)
initWithPayload_isCompact_rowCount_columnCount cipdF417CodeDescriptor  errorCorrectedPayload isCompact rowCount columnCount =
withObjCPtr errorCorrectedPayload $ \raw_errorCorrectedPayload ->
    sendMsg cipdF417CodeDescriptor (mkSelector "initWithPayload:isCompact:rowCount:columnCount:") (retPtr retVoid) [argPtr (castPtr raw_errorCorrectedPayload :: Ptr ()), argCULong (if isCompact then 1 else 0), argCLong (fromIntegral rowCount), argCLong (fromIntegral columnCount)] >>= ownedObject . castPtr

-- | Creates an PDF417 code descriptor for the given payload and parameters.
--
-- - Parameters:   - errorCorrectedPayload: The data to encode in the PDF417 code symbol.   - isCompact: A Boolean indicating whether or not the PDF417 code is compact.   - rowCount: The number of rows in the PDF417 code, from 3 to 90.   - columnCount: The number of columns in the Aztec code, from 1 to 30. - Returns:     An autoreleased ``CIPDF417CodeDescriptor`` instance     or @nil@ if the parameters are invalid
--
-- ObjC selector: @+ descriptorWithPayload:isCompact:rowCount:columnCount:@
descriptorWithPayload_isCompact_rowCount_columnCount :: IsNSData errorCorrectedPayload => errorCorrectedPayload -> Bool -> CLong -> CLong -> IO (Id CIPDF417CodeDescriptor)
descriptorWithPayload_isCompact_rowCount_columnCount errorCorrectedPayload isCompact rowCount columnCount =
  do
    cls' <- getRequiredClass "CIPDF417CodeDescriptor"
    withObjCPtr errorCorrectedPayload $ \raw_errorCorrectedPayload ->
      sendClassMsg cls' (mkSelector "descriptorWithPayload:isCompact:rowCount:columnCount:") (retPtr retVoid) [argPtr (castPtr raw_errorCorrectedPayload :: Ptr ()), argCULong (if isCompact then 1 else 0), argCLong (fromIntegral rowCount), argCLong (fromIntegral columnCount)] >>= retainedObject . castPtr

-- | The error-corrected payload containing the data encoded in the PDF417 code symbol.
--
-- The first codeword indicates the number of data codewords in the errorCorrectedPayload.
--
-- PDF417 codes are comprised of a start character on the left and a stop character on the right.  Each row begins and ends with special characters indicating the current row as well as information about the dimensions of the PDF417 symbol. The errorCorrectedPayload represents the sequence of PDF417 codewords that make up the body of the message. The first codeword indicates the number  of codewords in the message. This count includes the "count" codeword and any padding codewords,  but does not include the error correction codewords. Each codeword is a 16-bit value in the range  of 0...928. The sequence is to be interpreted as described in the PDF417 bar code symbology  specification -- ISO/IEC 15438:2006(E).
--
-- ObjC selector: @- errorCorrectedPayload@
errorCorrectedPayload :: IsCIPDF417CodeDescriptor cipdF417CodeDescriptor => cipdF417CodeDescriptor -> IO (Id NSData)
errorCorrectedPayload cipdF417CodeDescriptor  =
  sendMsg cipdF417CodeDescriptor (mkSelector "errorCorrectedPayload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A boolean value telling if the PDF417 code is compact.
--
-- Compact PDF417 symbols have abbreviated right-side guard bars.
--
-- ObjC selector: @- isCompact@
isCompact :: IsCIPDF417CodeDescriptor cipdF417CodeDescriptor => cipdF417CodeDescriptor -> IO Bool
isCompact cipdF417CodeDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cipdF417CodeDescriptor (mkSelector "isCompact") retCULong []

-- | The number of rows in the PDF417 code symbol.
--
-- Valid row count values are from 3 to 90.
--
-- ObjC selector: @- rowCount@
rowCount :: IsCIPDF417CodeDescriptor cipdF417CodeDescriptor => cipdF417CodeDescriptor -> IO CLong
rowCount cipdF417CodeDescriptor  =
  sendMsg cipdF417CodeDescriptor (mkSelector "rowCount") retCLong []

-- | The number of columns in the PDF417 code symbol.
--
-- Valid column count values are from 1 to 30. This count excluded the columns used to indicate the symbol structure.
--
-- ObjC selector: @- columnCount@
columnCount :: IsCIPDF417CodeDescriptor cipdF417CodeDescriptor => cipdF417CodeDescriptor -> IO CLong
columnCount cipdF417CodeDescriptor  =
  sendMsg cipdF417CodeDescriptor (mkSelector "columnCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayload:isCompact:rowCount:columnCount:@
initWithPayload_isCompact_rowCount_columnCountSelector :: Selector
initWithPayload_isCompact_rowCount_columnCountSelector = mkSelector "initWithPayload:isCompact:rowCount:columnCount:"

-- | @Selector@ for @descriptorWithPayload:isCompact:rowCount:columnCount:@
descriptorWithPayload_isCompact_rowCount_columnCountSelector :: Selector
descriptorWithPayload_isCompact_rowCount_columnCountSelector = mkSelector "descriptorWithPayload:isCompact:rowCount:columnCount:"

-- | @Selector@ for @errorCorrectedPayload@
errorCorrectedPayloadSelector :: Selector
errorCorrectedPayloadSelector = mkSelector "errorCorrectedPayload"

-- | @Selector@ for @isCompact@
isCompactSelector :: Selector
isCompactSelector = mkSelector "isCompact"

-- | @Selector@ for @rowCount@
rowCountSelector :: Selector
rowCountSelector = mkSelector "rowCount"

-- | @Selector@ for @columnCount@
columnCountSelector :: Selector
columnCountSelector = mkSelector "columnCount"

