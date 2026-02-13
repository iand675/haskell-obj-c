{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifies PIN formatting properties.
--
-- Generated bindings for @TKSmartCardPINFormat@.
module ObjC.CryptoTokenKit.TKSmartCardPINFormat
  ( TKSmartCardPINFormat
  , IsTKSmartCardPINFormat(..)
  , charset
  , setCharset
  , encoding
  , setEncoding
  , minPINLength
  , setMinPINLength
  , maxPINLength
  , setMaxPINLength
  , pinBlockByteLength
  , setPINBlockByteLength
  , pinJustification
  , setPINJustification
  , pinBitOffset
  , setPINBitOffset
  , pinLengthBitOffset
  , setPINLengthBitOffset
  , pinLengthBitSize
  , setPINLengthBitSize
  , charsetSelector
  , encodingSelector
  , maxPINLengthSelector
  , minPINLengthSelector
  , pinBitOffsetSelector
  , pinBlockByteLengthSelector
  , pinJustificationSelector
  , pinLengthBitOffsetSelector
  , pinLengthBitSizeSelector
  , setCharsetSelector
  , setEncodingSelector
  , setMaxPINLengthSelector
  , setMinPINLengthSelector
  , setPINBitOffsetSelector
  , setPINBlockByteLengthSelector
  , setPINJustificationSelector
  , setPINLengthBitOffsetSelector
  , setPINLengthBitSizeSelector

  -- * Enum types
  , TKSmartCardPINCharset(TKSmartCardPINCharset)
  , pattern TKSmartCardPINCharsetNumeric
  , pattern TKSmartCardPINCharsetAlphanumeric
  , pattern TKSmartCardPINCharsetUpperAlphanumeric
  , TKSmartCardPINEncoding(TKSmartCardPINEncoding)
  , pattern TKSmartCardPINEncodingBinary
  , pattern TKSmartCardPINEncodingASCII
  , pattern TKSmartCardPINEncodingBCD
  , TKSmartCardPINJustification(TKSmartCardPINJustification)
  , pattern TKSmartCardPINJustificationLeft
  , pattern TKSmartCardPINJustificationRight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Format of PIN characters.
--
-- Note: Default value: TKSmartCardPINCharsetNumeric
--
-- ObjC selector: @- charset@
charset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO TKSmartCardPINCharset
charset tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat charsetSelector

-- | Format of PIN characters.
--
-- Note: Default value: TKSmartCardPINCharsetNumeric
--
-- ObjC selector: @- setCharset:@
setCharset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> TKSmartCardPINCharset -> IO ()
setCharset tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setCharsetSelector value

-- | Encoding of PIN characters.
--
-- Note: Default value: TKSmartCardPINEncodingASCII
--
-- ObjC selector: @- encoding@
encoding :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO TKSmartCardPINEncoding
encoding tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat encodingSelector

-- | Encoding of PIN characters.
--
-- Note: Default value: TKSmartCardPINEncodingASCII
--
-- ObjC selector: @- setEncoding:@
setEncoding :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> TKSmartCardPINEncoding -> IO ()
setEncoding tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setEncodingSelector value

-- | Minimum number of characters to form a valid PIN.
--
-- Note: Default value: 4
--
-- ObjC selector: @- minPINLength@
minPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
minPINLength tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat minPINLengthSelector

-- | Minimum number of characters to form a valid PIN.
--
-- Note: Default value: 4
--
-- ObjC selector: @- setMinPINLength:@
setMinPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setMinPINLength tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setMinPINLengthSelector value

-- | Maximum number of characters to form a valid PIN.
--
-- Note: Default value: 8
--
-- ObjC selector: @- maxPINLength@
maxPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
maxPINLength tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat maxPINLengthSelector

-- | Maximum number of characters to form a valid PIN.
--
-- Note: Default value: 8
--
-- ObjC selector: @- setMaxPINLength:@
setMaxPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setMaxPINLength tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setMaxPINLengthSelector value

-- | Total length of the PIN block in bytes.
--
-- Note: Default value: 8
--
-- ObjC selector: @- PINBlockByteLength@
pinBlockByteLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinBlockByteLength tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat pinBlockByteLengthSelector

-- | Total length of the PIN block in bytes.
--
-- Note: Default value: 8
--
-- ObjC selector: @- setPINBlockByteLength:@
setPINBlockByteLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINBlockByteLength tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setPINBlockByteLengthSelector value

-- | PIN justification within the PIN block.
--
-- Note: Default value: TKSmartCardPINJustificationLeft
--
-- ObjC selector: @- PINJustification@
pinJustification :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO TKSmartCardPINJustification
pinJustification tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat pinJustificationSelector

-- | PIN justification within the PIN block.
--
-- Note: Default value: TKSmartCardPINJustificationLeft
--
-- ObjC selector: @- setPINJustification:@
setPINJustification :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> TKSmartCardPINJustification -> IO ()
setPINJustification tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setPINJustificationSelector value

-- | Offset in bits within the PIN block to mark a location for filling in the formatted PIN (justified with respect to PINJustification).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the formatted PIN, which is justified with respect to the PINJustification property value.
--
-- ObjC selector: @- PINBitOffset@
pinBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinBitOffset tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat pinBitOffsetSelector

-- | Offset in bits within the PIN block to mark a location for filling in the formatted PIN (justified with respect to PINJustification).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the formatted PIN, which is justified with respect to the PINJustification property value.
--
-- ObjC selector: @- setPINBitOffset:@
setPINBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINBitOffset tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setPINBitOffsetSelector value

-- | Offset in bits within the PIN block to mark a location for filling in the PIN length (always left justified).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the PIN length, which is always left justified.
--
-- ObjC selector: @- PINLengthBitOffset@
pinLengthBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinLengthBitOffset tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat pinLengthBitOffsetSelector

-- | Offset in bits within the PIN block to mark a location for filling in the PIN length (always left justified).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the PIN length, which is always left justified.
--
-- ObjC selector: @- setPINLengthBitOffset:@
setPINLengthBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINLengthBitOffset tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setPINLengthBitOffsetSelector value

-- | Size in bits of the PIN length field. If set to 0, PIN length is not written.
--
-- Note: Default value: 0
--
-- ObjC selector: @- PINLengthBitSize@
pinLengthBitSize :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinLengthBitSize tkSmartCardPINFormat =
  sendMessage tkSmartCardPINFormat pinLengthBitSizeSelector

-- | Size in bits of the PIN length field. If set to 0, PIN length is not written.
--
-- Note: Default value: 0
--
-- ObjC selector: @- setPINLengthBitSize:@
setPINLengthBitSize :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINLengthBitSize tkSmartCardPINFormat value =
  sendMessage tkSmartCardPINFormat setPINLengthBitSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @charset@
charsetSelector :: Selector '[] TKSmartCardPINCharset
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector '[TKSmartCardPINCharset] ()
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @encoding@
encodingSelector :: Selector '[] TKSmartCardPINEncoding
encodingSelector = mkSelector "encoding"

-- | @Selector@ for @setEncoding:@
setEncodingSelector :: Selector '[TKSmartCardPINEncoding] ()
setEncodingSelector = mkSelector "setEncoding:"

-- | @Selector@ for @minPINLength@
minPINLengthSelector :: Selector '[] CLong
minPINLengthSelector = mkSelector "minPINLength"

-- | @Selector@ for @setMinPINLength:@
setMinPINLengthSelector :: Selector '[CLong] ()
setMinPINLengthSelector = mkSelector "setMinPINLength:"

-- | @Selector@ for @maxPINLength@
maxPINLengthSelector :: Selector '[] CLong
maxPINLengthSelector = mkSelector "maxPINLength"

-- | @Selector@ for @setMaxPINLength:@
setMaxPINLengthSelector :: Selector '[CLong] ()
setMaxPINLengthSelector = mkSelector "setMaxPINLength:"

-- | @Selector@ for @PINBlockByteLength@
pinBlockByteLengthSelector :: Selector '[] CLong
pinBlockByteLengthSelector = mkSelector "PINBlockByteLength"

-- | @Selector@ for @setPINBlockByteLength:@
setPINBlockByteLengthSelector :: Selector '[CLong] ()
setPINBlockByteLengthSelector = mkSelector "setPINBlockByteLength:"

-- | @Selector@ for @PINJustification@
pinJustificationSelector :: Selector '[] TKSmartCardPINJustification
pinJustificationSelector = mkSelector "PINJustification"

-- | @Selector@ for @setPINJustification:@
setPINJustificationSelector :: Selector '[TKSmartCardPINJustification] ()
setPINJustificationSelector = mkSelector "setPINJustification:"

-- | @Selector@ for @PINBitOffset@
pinBitOffsetSelector :: Selector '[] CLong
pinBitOffsetSelector = mkSelector "PINBitOffset"

-- | @Selector@ for @setPINBitOffset:@
setPINBitOffsetSelector :: Selector '[CLong] ()
setPINBitOffsetSelector = mkSelector "setPINBitOffset:"

-- | @Selector@ for @PINLengthBitOffset@
pinLengthBitOffsetSelector :: Selector '[] CLong
pinLengthBitOffsetSelector = mkSelector "PINLengthBitOffset"

-- | @Selector@ for @setPINLengthBitOffset:@
setPINLengthBitOffsetSelector :: Selector '[CLong] ()
setPINLengthBitOffsetSelector = mkSelector "setPINLengthBitOffset:"

-- | @Selector@ for @PINLengthBitSize@
pinLengthBitSizeSelector :: Selector '[] CLong
pinLengthBitSizeSelector = mkSelector "PINLengthBitSize"

-- | @Selector@ for @setPINLengthBitSize:@
setPINLengthBitSizeSelector :: Selector '[CLong] ()
setPINLengthBitSizeSelector = mkSelector "setPINLengthBitSize:"

