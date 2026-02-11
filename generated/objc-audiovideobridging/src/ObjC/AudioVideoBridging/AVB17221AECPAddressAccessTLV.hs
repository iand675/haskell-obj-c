{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPAddressAccessTLV
--
-- AVB17221AECPAddressAccessTLV encapsulates a TLV from an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Address Access message.
--
-- AVB17221AECPAddressAccessTLV encapsulates a TLV from an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Address Access message.
--
-- Generated bindings for @AVB17221AECPAddressAccessTLV@.
module ObjC.AudioVideoBridging.AVB17221AECPAddressAccessTLV
  ( AVB17221AECPAddressAccessTLV
  , IsAVB17221AECPAddressAccessTLV(..)
  , mode
  , setMode
  , address
  , setAddress
  , memoryData
  , setMemoryData
  , modeSelector
  , setModeSelector
  , addressSelector
  , setAddressSelector
  , memoryDataSelector
  , setMemoryDataSelector

  -- * Enum types
  , AVB17221AECPAddressAccessTLVMode(AVB17221AECPAddressAccessTLVMode)
  , pattern AVB17221AECPAddressAccessTLVModeRead
  , pattern AVB17221AECPAddressAccessTLVModeWrite
  , pattern AVB17221AECPAddressAccessTLVModeExecute

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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.AudioVideoBridging.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | mode
--
-- The mode field of the Address Access TLV.
--
-- ObjC selector: @- mode@
mode :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> IO AVB17221AECPAddressAccessTLVMode
mode avB17221AECPAddressAccessTLV  =
  fmap (coerce :: CUChar -> AVB17221AECPAddressAccessTLVMode) $ sendMsg avB17221AECPAddressAccessTLV (mkSelector "mode") retCUChar []

-- | mode
--
-- The mode field of the Address Access TLV.
--
-- ObjC selector: @- setMode:@
setMode :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> AVB17221AECPAddressAccessTLVMode -> IO ()
setMode avB17221AECPAddressAccessTLV  value =
  sendMsg avB17221AECPAddressAccessTLV (mkSelector "setMode:") retVoid [argCUChar (coerce value)]

-- | address
--
-- The address field of the Address Access TLV.
--
-- ObjC selector: @- address@
address :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> IO CULong
address avB17221AECPAddressAccessTLV  =
  sendMsg avB17221AECPAddressAccessTLV (mkSelector "address") retCULong []

-- | address
--
-- The address field of the Address Access TLV.
--
-- ObjC selector: @- setAddress:@
setAddress :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> CULong -> IO ()
setAddress avB17221AECPAddressAccessTLV  value =
  sendMsg avB17221AECPAddressAccessTLV (mkSelector "setAddress:") retVoid [argCULong (fromIntegral value)]

-- | memoryData
--
-- The memory_data field of the Address Access TLV.
--
-- ObjC selector: @- memoryData@
memoryData :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> IO (Id NSData)
memoryData avB17221AECPAddressAccessTLV  =
  sendMsg avB17221AECPAddressAccessTLV (mkSelector "memoryData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | memoryData
--
-- The memory_data field of the Address Access TLV.
--
-- ObjC selector: @- setMemoryData:@
setMemoryData :: (IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV, IsNSData value) => avB17221AECPAddressAccessTLV -> value -> IO ()
setMemoryData avB17221AECPAddressAccessTLV  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221AECPAddressAccessTLV (mkSelector "setMemoryData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @setAddress:@
setAddressSelector :: Selector
setAddressSelector = mkSelector "setAddress:"

-- | @Selector@ for @memoryData@
memoryDataSelector :: Selector
memoryDataSelector = mkSelector "memoryData"

-- | @Selector@ for @setMemoryData:@
setMemoryDataSelector :: Selector
setMemoryDataSelector = mkSelector "setMemoryData:"

