{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKSimpleTLVRecord implements Simple-TLV encoding according to ISO7816-4. Tag is number in range <1..254> encoded as single byte, length is either single byte specifying length 0-254 or 3 bytes encoded as 0xff followed by 2 bytes of big-endian encoded number.
--
-- Generated bindings for @TKSimpleTLVRecord@.
module ObjC.CryptoTokenKit.TKSimpleTLVRecord
  ( TKSimpleTLVRecord
  , IsTKSimpleTLVRecord(..)
  , initWithTag_value
  , initWithTag_valueSelector


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
import ObjC.Foundation.Internal.Classes

-- | Creates TLV record with specified tag and value.
--
-- @tag@ — Tag value for the new record.
--
-- @value@ — Value for the new record.
--
-- Returns: Newly created TLV record.
--
-- ObjC selector: @- initWithTag:value:@
initWithTag_value :: (IsTKSimpleTLVRecord tkSimpleTLVRecord, IsNSData value) => tkSimpleTLVRecord -> CUChar -> value -> IO (Id TKSimpleTLVRecord)
initWithTag_value tkSimpleTLVRecord  tag value =
withObjCPtr value $ \raw_value ->
    sendMsg tkSimpleTLVRecord (mkSelector "initWithTag:value:") (retPtr retVoid) [argCUChar (fromIntegral tag), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:value:@
initWithTag_valueSelector :: Selector
initWithTag_valueSelector = mkSelector "initWithTag:value:"

