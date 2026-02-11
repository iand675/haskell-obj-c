{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKCompactTLVRecord implements Compact-TLV encoding according to ISO7816-4 Tag is number in range <0..15> encoded as high 4 bits of initial byte, length is number in range <0..15> encoded as low 4 bits of initial byte.  Value immediatelly follows leading byte.
--
-- Generated bindings for @TKCompactTLVRecord@.
module ObjC.CryptoTokenKit.TKCompactTLVRecord
  ( TKCompactTLVRecord
  , IsTKCompactTLVRecord(..)
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
initWithTag_value :: (IsTKCompactTLVRecord tkCompactTLVRecord, IsNSData value) => tkCompactTLVRecord -> CUChar -> value -> IO (Id TKCompactTLVRecord)
initWithTag_value tkCompactTLVRecord  tag value =
withObjCPtr value $ \raw_value ->
    sendMsg tkCompactTLVRecord (mkSelector "initWithTag:value:") (retPtr retVoid) [argCUChar (fromIntegral tag), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:value:@
initWithTag_valueSelector :: Selector
initWithTag_valueSelector = mkSelector "initWithTag:value:"

