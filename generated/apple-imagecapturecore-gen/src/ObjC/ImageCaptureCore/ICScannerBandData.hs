{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ICScannerBandData@.
module ObjC.ImageCaptureCore.ICScannerBandData
  ( ICScannerBandData
  , IsICScannerBandData(..)
  , fullImageWidth
  , fullImageHeight
  , bitsPerPixel
  , bitsPerComponent
  , numComponents
  , bigEndian
  , pixelDataType
  , colorSyncProfilePath
  , bytesPerRow
  , dataStartRow
  , dataNumRows
  , dataSize
  , dataBuffer
  , fullImageWidthSelector
  , fullImageHeightSelector
  , bitsPerPixelSelector
  , bitsPerComponentSelector
  , numComponentsSelector
  , bigEndianSelector
  , pixelDataTypeSelector
  , colorSyncProfilePathSelector
  , bytesPerRowSelector
  , dataStartRowSelector
  , dataNumRowsSelector
  , dataSizeSelector
  , dataBufferSelector

  -- * Enum types
  , ICScannerPixelDataType(ICScannerPixelDataType)
  , pattern ICScannerPixelDataTypeBW
  , pattern ICScannerPixelDataTypeGray
  , pattern ICScannerPixelDataTypeRGB
  , pattern ICScannerPixelDataTypePalette
  , pattern ICScannerPixelDataTypeCMY
  , pattern ICScannerPixelDataTypeCMYK
  , pattern ICScannerPixelDataTypeYUV
  , pattern ICScannerPixelDataTypeYUVK
  , pattern ICScannerPixelDataTypeCIEXYZ

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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | fullImageWidth
--
-- Describes the full image width of the banded image.
--
-- ObjC selector: @- fullImageWidth@
fullImageWidth :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
fullImageWidth icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "fullImageWidth") retCULong []

-- | fullImageHeight
--
-- Describes the full image height of the banded image.
--
-- ObjC selector: @- fullImageHeight@
fullImageHeight :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
fullImageHeight icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "fullImageHeight") retCULong []

-- | bitsPerPixel
--
-- Describes the number of bits per pixel for banded the image.
--
-- ObjC selector: @- bitsPerPixel@
bitsPerPixel :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
bitsPerPixel icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "bitsPerPixel") retCULong []

-- | bitsPerComponent
--
-- Describes the number of bits per component for the banded image.
--
-- ObjC selector: @- bitsPerComponent@
bitsPerComponent :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
bitsPerComponent icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "bitsPerComponent") retCULong []

-- | numComponents
--
-- Describes how many components are contained within the banded image.
--
-- ObjC selector: @- numComponents@
numComponents :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
numComponents icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "numComponents") retCULong []

-- | bigEndian
--
-- Describes if the banded image data is reported in big endian.
--
-- ObjC selector: @- bigEndian@
bigEndian :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO Bool
bigEndian icScannerBandData  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerBandData (mkSelector "bigEndian") retCULong []

-- | pixelDataType
--
-- Type of pixel data that is contained in the band.
--
-- ObjC selector: @- pixelDataType@
pixelDataType :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO ICScannerPixelDataType
pixelDataType icScannerBandData  =
    fmap (coerce :: CULong -> ICScannerPixelDataType) $ sendMsg icScannerBandData (mkSelector "pixelDataType") retCULong []

-- | colorSyncProfilePath
--
-- Returns the path to the color profile matching the banded data.
--
-- ObjC selector: @- colorSyncProfilePath@
colorSyncProfilePath :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO RawId
colorSyncProfilePath icScannerBandData  =
    fmap (RawId . castPtr) $ sendMsg icScannerBandData (mkSelector "colorSyncProfilePath") (retPtr retVoid) []

-- | bytesPerRow
--
-- Descries how many bytes are in each image band row.
--
-- ObjC selector: @- bytesPerRow@
bytesPerRow :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
bytesPerRow icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "bytesPerRow") retCULong []

-- | dataStartRow
--
-- Describes the start row of the image band.
--
-- ObjC selector: @- dataStartRow@
dataStartRow :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
dataStartRow icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "dataStartRow") retCULong []

-- | dataNumRows
--
-- Describes the number of rows contained in the image band.
--
-- ObjC selector: @- dataNumRows@
dataNumRows :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
dataNumRows icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "dataNumRows") retCULong []

-- | dataSize
--
-- Describes the actual data size of the image band buffer.
--
-- ObjC selector: @- dataSize@
dataSize :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
dataSize icScannerBandData  =
    sendMsg icScannerBandData (mkSelector "dataSize") retCULong []

-- | dataBuffer
--
-- The pointer to the data buffer object.
--
-- ObjC selector: @- dataBuffer@
dataBuffer :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO RawId
dataBuffer icScannerBandData  =
    fmap (RawId . castPtr) $ sendMsg icScannerBandData (mkSelector "dataBuffer") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fullImageWidth@
fullImageWidthSelector :: Selector
fullImageWidthSelector = mkSelector "fullImageWidth"

-- | @Selector@ for @fullImageHeight@
fullImageHeightSelector :: Selector
fullImageHeightSelector = mkSelector "fullImageHeight"

-- | @Selector@ for @bitsPerPixel@
bitsPerPixelSelector :: Selector
bitsPerPixelSelector = mkSelector "bitsPerPixel"

-- | @Selector@ for @bitsPerComponent@
bitsPerComponentSelector :: Selector
bitsPerComponentSelector = mkSelector "bitsPerComponent"

-- | @Selector@ for @numComponents@
numComponentsSelector :: Selector
numComponentsSelector = mkSelector "numComponents"

-- | @Selector@ for @bigEndian@
bigEndianSelector :: Selector
bigEndianSelector = mkSelector "bigEndian"

-- | @Selector@ for @pixelDataType@
pixelDataTypeSelector :: Selector
pixelDataTypeSelector = mkSelector "pixelDataType"

-- | @Selector@ for @colorSyncProfilePath@
colorSyncProfilePathSelector :: Selector
colorSyncProfilePathSelector = mkSelector "colorSyncProfilePath"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @dataStartRow@
dataStartRowSelector :: Selector
dataStartRowSelector = mkSelector "dataStartRow"

-- | @Selector@ for @dataNumRows@
dataNumRowsSelector :: Selector
dataNumRowsSelector = mkSelector "dataNumRows"

-- | @Selector@ for @dataSize@
dataSizeSelector :: Selector
dataSizeSelector = mkSelector "dataSize"

-- | @Selector@ for @dataBuffer@
dataBufferSelector :: Selector
dataBufferSelector = mkSelector "dataBuffer"

