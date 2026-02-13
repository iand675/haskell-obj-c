{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bigEndianSelector
  , bitsPerComponentSelector
  , bitsPerPixelSelector
  , bytesPerRowSelector
  , colorSyncProfilePathSelector
  , dataBufferSelector
  , dataNumRowsSelector
  , dataSizeSelector
  , dataStartRowSelector
  , fullImageHeightSelector
  , fullImageWidthSelector
  , numComponentsSelector
  , pixelDataTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
fullImageWidth icScannerBandData =
  sendMessage icScannerBandData fullImageWidthSelector

-- | fullImageHeight
--
-- Describes the full image height of the banded image.
--
-- ObjC selector: @- fullImageHeight@
fullImageHeight :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
fullImageHeight icScannerBandData =
  sendMessage icScannerBandData fullImageHeightSelector

-- | bitsPerPixel
--
-- Describes the number of bits per pixel for banded the image.
--
-- ObjC selector: @- bitsPerPixel@
bitsPerPixel :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
bitsPerPixel icScannerBandData =
  sendMessage icScannerBandData bitsPerPixelSelector

-- | bitsPerComponent
--
-- Describes the number of bits per component for the banded image.
--
-- ObjC selector: @- bitsPerComponent@
bitsPerComponent :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
bitsPerComponent icScannerBandData =
  sendMessage icScannerBandData bitsPerComponentSelector

-- | numComponents
--
-- Describes how many components are contained within the banded image.
--
-- ObjC selector: @- numComponents@
numComponents :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
numComponents icScannerBandData =
  sendMessage icScannerBandData numComponentsSelector

-- | bigEndian
--
-- Describes if the banded image data is reported in big endian.
--
-- ObjC selector: @- bigEndian@
bigEndian :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO Bool
bigEndian icScannerBandData =
  sendMessage icScannerBandData bigEndianSelector

-- | pixelDataType
--
-- Type of pixel data that is contained in the band.
--
-- ObjC selector: @- pixelDataType@
pixelDataType :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO ICScannerPixelDataType
pixelDataType icScannerBandData =
  sendMessage icScannerBandData pixelDataTypeSelector

-- | colorSyncProfilePath
--
-- Returns the path to the color profile matching the banded data.
--
-- ObjC selector: @- colorSyncProfilePath@
colorSyncProfilePath :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO RawId
colorSyncProfilePath icScannerBandData =
  sendMessage icScannerBandData colorSyncProfilePathSelector

-- | bytesPerRow
--
-- Descries how many bytes are in each image band row.
--
-- ObjC selector: @- bytesPerRow@
bytesPerRow :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
bytesPerRow icScannerBandData =
  sendMessage icScannerBandData bytesPerRowSelector

-- | dataStartRow
--
-- Describes the start row of the image band.
--
-- ObjC selector: @- dataStartRow@
dataStartRow :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
dataStartRow icScannerBandData =
  sendMessage icScannerBandData dataStartRowSelector

-- | dataNumRows
--
-- Describes the number of rows contained in the image band.
--
-- ObjC selector: @- dataNumRows@
dataNumRows :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
dataNumRows icScannerBandData =
  sendMessage icScannerBandData dataNumRowsSelector

-- | dataSize
--
-- Describes the actual data size of the image band buffer.
--
-- ObjC selector: @- dataSize@
dataSize :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO CULong
dataSize icScannerBandData =
  sendMessage icScannerBandData dataSizeSelector

-- | dataBuffer
--
-- The pointer to the data buffer object.
--
-- ObjC selector: @- dataBuffer@
dataBuffer :: IsICScannerBandData icScannerBandData => icScannerBandData -> IO RawId
dataBuffer icScannerBandData =
  sendMessage icScannerBandData dataBufferSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fullImageWidth@
fullImageWidthSelector :: Selector '[] CULong
fullImageWidthSelector = mkSelector "fullImageWidth"

-- | @Selector@ for @fullImageHeight@
fullImageHeightSelector :: Selector '[] CULong
fullImageHeightSelector = mkSelector "fullImageHeight"

-- | @Selector@ for @bitsPerPixel@
bitsPerPixelSelector :: Selector '[] CULong
bitsPerPixelSelector = mkSelector "bitsPerPixel"

-- | @Selector@ for @bitsPerComponent@
bitsPerComponentSelector :: Selector '[] CULong
bitsPerComponentSelector = mkSelector "bitsPerComponent"

-- | @Selector@ for @numComponents@
numComponentsSelector :: Selector '[] CULong
numComponentsSelector = mkSelector "numComponents"

-- | @Selector@ for @bigEndian@
bigEndianSelector :: Selector '[] Bool
bigEndianSelector = mkSelector "bigEndian"

-- | @Selector@ for @pixelDataType@
pixelDataTypeSelector :: Selector '[] ICScannerPixelDataType
pixelDataTypeSelector = mkSelector "pixelDataType"

-- | @Selector@ for @colorSyncProfilePath@
colorSyncProfilePathSelector :: Selector '[] RawId
colorSyncProfilePathSelector = mkSelector "colorSyncProfilePath"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector '[] CULong
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @dataStartRow@
dataStartRowSelector :: Selector '[] CULong
dataStartRowSelector = mkSelector "dataStartRow"

-- | @Selector@ for @dataNumRows@
dataNumRowsSelector :: Selector '[] CULong
dataNumRowsSelector = mkSelector "dataNumRows"

-- | @Selector@ for @dataSize@
dataSizeSelector :: Selector '[] CULong
dataSizeSelector = mkSelector "dataSize"

-- | @Selector@ for @dataBuffer@
dataBufferSelector :: Selector '[] RawId
dataBufferSelector = mkSelector "dataBuffer"

