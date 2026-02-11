{-# LANGUAGE PatternSynonyms #-}
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
  , setCharsetSelector
  , encodingSelector
  , setEncodingSelector
  , minPINLengthSelector
  , setMinPINLengthSelector
  , maxPINLengthSelector
  , setMaxPINLengthSelector
  , pinBlockByteLengthSelector
  , setPINBlockByteLengthSelector
  , pinJustificationSelector
  , setPINJustificationSelector
  , pinBitOffsetSelector
  , setPINBitOffsetSelector
  , pinLengthBitOffsetSelector
  , setPINLengthBitOffsetSelector
  , pinLengthBitSizeSelector
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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Format of PIN characters.
--
-- Note: Default value: TKSmartCardPINCharsetNumeric
--
-- ObjC selector: @- charset@
charset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO TKSmartCardPINCharset
charset tkSmartCardPINFormat  =
  fmap (coerce :: CLong -> TKSmartCardPINCharset) $ sendMsg tkSmartCardPINFormat (mkSelector "charset") retCLong []

-- | Format of PIN characters.
--
-- Note: Default value: TKSmartCardPINCharsetNumeric
--
-- ObjC selector: @- setCharset:@
setCharset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> TKSmartCardPINCharset -> IO ()
setCharset tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setCharset:") retVoid [argCLong (coerce value)]

-- | Encoding of PIN characters.
--
-- Note: Default value: TKSmartCardPINEncodingASCII
--
-- ObjC selector: @- encoding@
encoding :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO TKSmartCardPINEncoding
encoding tkSmartCardPINFormat  =
  fmap (coerce :: CLong -> TKSmartCardPINEncoding) $ sendMsg tkSmartCardPINFormat (mkSelector "encoding") retCLong []

-- | Encoding of PIN characters.
--
-- Note: Default value: TKSmartCardPINEncodingASCII
--
-- ObjC selector: @- setEncoding:@
setEncoding :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> TKSmartCardPINEncoding -> IO ()
setEncoding tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setEncoding:") retVoid [argCLong (coerce value)]

-- | Minimum number of characters to form a valid PIN.
--
-- Note: Default value: 4
--
-- ObjC selector: @- minPINLength@
minPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
minPINLength tkSmartCardPINFormat  =
  sendMsg tkSmartCardPINFormat (mkSelector "minPINLength") retCLong []

-- | Minimum number of characters to form a valid PIN.
--
-- Note: Default value: 4
--
-- ObjC selector: @- setMinPINLength:@
setMinPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setMinPINLength tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setMinPINLength:") retVoid [argCLong (fromIntegral value)]

-- | Maximum number of characters to form a valid PIN.
--
-- Note: Default value: 8
--
-- ObjC selector: @- maxPINLength@
maxPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
maxPINLength tkSmartCardPINFormat  =
  sendMsg tkSmartCardPINFormat (mkSelector "maxPINLength") retCLong []

-- | Maximum number of characters to form a valid PIN.
--
-- Note: Default value: 8
--
-- ObjC selector: @- setMaxPINLength:@
setMaxPINLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setMaxPINLength tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setMaxPINLength:") retVoid [argCLong (fromIntegral value)]

-- | Total length of the PIN block in bytes.
--
-- Note: Default value: 8
--
-- ObjC selector: @- PINBlockByteLength@
pinBlockByteLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinBlockByteLength tkSmartCardPINFormat  =
  sendMsg tkSmartCardPINFormat (mkSelector "PINBlockByteLength") retCLong []

-- | Total length of the PIN block in bytes.
--
-- Note: Default value: 8
--
-- ObjC selector: @- setPINBlockByteLength:@
setPINBlockByteLength :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINBlockByteLength tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setPINBlockByteLength:") retVoid [argCLong (fromIntegral value)]

-- | PIN justification within the PIN block.
--
-- Note: Default value: TKSmartCardPINJustificationLeft
--
-- ObjC selector: @- PINJustification@
pinJustification :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO TKSmartCardPINJustification
pinJustification tkSmartCardPINFormat  =
  fmap (coerce :: CLong -> TKSmartCardPINJustification) $ sendMsg tkSmartCardPINFormat (mkSelector "PINJustification") retCLong []

-- | PIN justification within the PIN block.
--
-- Note: Default value: TKSmartCardPINJustificationLeft
--
-- ObjC selector: @- setPINJustification:@
setPINJustification :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> TKSmartCardPINJustification -> IO ()
setPINJustification tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setPINJustification:") retVoid [argCLong (coerce value)]

-- | Offset in bits within the PIN block to mark a location for filling in the formatted PIN (justified with respect to PINJustification).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the formatted PIN, which is justified with respect to the PINJustification property value.
--
-- ObjC selector: @- PINBitOffset@
pinBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinBitOffset tkSmartCardPINFormat  =
  sendMsg tkSmartCardPINFormat (mkSelector "PINBitOffset") retCLong []

-- | Offset in bits within the PIN block to mark a location for filling in the formatted PIN (justified with respect to PINJustification).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the formatted PIN, which is justified with respect to the PINJustification property value.
--
-- ObjC selector: @- setPINBitOffset:@
setPINBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINBitOffset tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setPINBitOffset:") retVoid [argCLong (fromIntegral value)]

-- | Offset in bits within the PIN block to mark a location for filling in the PIN length (always left justified).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the PIN length, which is always left justified.
--
-- ObjC selector: @- PINLengthBitOffset@
pinLengthBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinLengthBitOffset tkSmartCardPINFormat  =
  sendMsg tkSmartCardPINFormat (mkSelector "PINLengthBitOffset") retCLong []

-- | Offset in bits within the PIN block to mark a location for filling in the PIN length (always left justified).
--
-- Note: Default value: 0
--
-- The offset, in bits, within the PIN block to mark a location for filling in the PIN length, which is always left justified.
--
-- ObjC selector: @- setPINLengthBitOffset:@
setPINLengthBitOffset :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINLengthBitOffset tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setPINLengthBitOffset:") retVoid [argCLong (fromIntegral value)]

-- | Size in bits of the PIN length field. If set to 0, PIN length is not written.
--
-- Note: Default value: 0
--
-- ObjC selector: @- PINLengthBitSize@
pinLengthBitSize :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> IO CLong
pinLengthBitSize tkSmartCardPINFormat  =
  sendMsg tkSmartCardPINFormat (mkSelector "PINLengthBitSize") retCLong []

-- | Size in bits of the PIN length field. If set to 0, PIN length is not written.
--
-- Note: Default value: 0
--
-- ObjC selector: @- setPINLengthBitSize:@
setPINLengthBitSize :: IsTKSmartCardPINFormat tkSmartCardPINFormat => tkSmartCardPINFormat -> CLong -> IO ()
setPINLengthBitSize tkSmartCardPINFormat  value =
  sendMsg tkSmartCardPINFormat (mkSelector "setPINLengthBitSize:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @charset@
charsetSelector :: Selector
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @encoding@
encodingSelector :: Selector
encodingSelector = mkSelector "encoding"

-- | @Selector@ for @setEncoding:@
setEncodingSelector :: Selector
setEncodingSelector = mkSelector "setEncoding:"

-- | @Selector@ for @minPINLength@
minPINLengthSelector :: Selector
minPINLengthSelector = mkSelector "minPINLength"

-- | @Selector@ for @setMinPINLength:@
setMinPINLengthSelector :: Selector
setMinPINLengthSelector = mkSelector "setMinPINLength:"

-- | @Selector@ for @maxPINLength@
maxPINLengthSelector :: Selector
maxPINLengthSelector = mkSelector "maxPINLength"

-- | @Selector@ for @setMaxPINLength:@
setMaxPINLengthSelector :: Selector
setMaxPINLengthSelector = mkSelector "setMaxPINLength:"

-- | @Selector@ for @PINBlockByteLength@
pinBlockByteLengthSelector :: Selector
pinBlockByteLengthSelector = mkSelector "PINBlockByteLength"

-- | @Selector@ for @setPINBlockByteLength:@
setPINBlockByteLengthSelector :: Selector
setPINBlockByteLengthSelector = mkSelector "setPINBlockByteLength:"

-- | @Selector@ for @PINJustification@
pinJustificationSelector :: Selector
pinJustificationSelector = mkSelector "PINJustification"

-- | @Selector@ for @setPINJustification:@
setPINJustificationSelector :: Selector
setPINJustificationSelector = mkSelector "setPINJustification:"

-- | @Selector@ for @PINBitOffset@
pinBitOffsetSelector :: Selector
pinBitOffsetSelector = mkSelector "PINBitOffset"

-- | @Selector@ for @setPINBitOffset:@
setPINBitOffsetSelector :: Selector
setPINBitOffsetSelector = mkSelector "setPINBitOffset:"

-- | @Selector@ for @PINLengthBitOffset@
pinLengthBitOffsetSelector :: Selector
pinLengthBitOffsetSelector = mkSelector "PINLengthBitOffset"

-- | @Selector@ for @setPINLengthBitOffset:@
setPINLengthBitOffsetSelector :: Selector
setPINLengthBitOffsetSelector = mkSelector "setPINLengthBitOffset:"

-- | @Selector@ for @PINLengthBitSize@
pinLengthBitSizeSelector :: Selector
pinLengthBitSizeSelector = mkSelector "PINLengthBitSize"

-- | @Selector@ for @setPINLengthBitSize:@
setPINLengthBitSizeSelector :: Selector
setPINLengthBitSizeSelector = mkSelector "setPINLengthBitSize:"

