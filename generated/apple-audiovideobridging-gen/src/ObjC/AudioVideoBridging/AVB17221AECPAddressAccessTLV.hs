{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addressSelector
  , memoryDataSelector
  , modeSelector
  , setAddressSelector
  , setMemoryDataSelector
  , setModeSelector

  -- * Enum types
  , AVB17221AECPAddressAccessTLVMode(AVB17221AECPAddressAccessTLVMode)
  , pattern AVB17221AECPAddressAccessTLVModeRead
  , pattern AVB17221AECPAddressAccessTLVModeWrite
  , pattern AVB17221AECPAddressAccessTLVModeExecute

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
mode avB17221AECPAddressAccessTLV =
  sendMessage avB17221AECPAddressAccessTLV modeSelector

-- | mode
--
-- The mode field of the Address Access TLV.
--
-- ObjC selector: @- setMode:@
setMode :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> AVB17221AECPAddressAccessTLVMode -> IO ()
setMode avB17221AECPAddressAccessTLV value =
  sendMessage avB17221AECPAddressAccessTLV setModeSelector value

-- | address
--
-- The address field of the Address Access TLV.
--
-- ObjC selector: @- address@
address :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> IO CULong
address avB17221AECPAddressAccessTLV =
  sendMessage avB17221AECPAddressAccessTLV addressSelector

-- | address
--
-- The address field of the Address Access TLV.
--
-- ObjC selector: @- setAddress:@
setAddress :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> CULong -> IO ()
setAddress avB17221AECPAddressAccessTLV value =
  sendMessage avB17221AECPAddressAccessTLV setAddressSelector value

-- | memoryData
--
-- The memory_data field of the Address Access TLV.
--
-- ObjC selector: @- memoryData@
memoryData :: IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV => avB17221AECPAddressAccessTLV -> IO (Id NSData)
memoryData avB17221AECPAddressAccessTLV =
  sendMessage avB17221AECPAddressAccessTLV memoryDataSelector

-- | memoryData
--
-- The memory_data field of the Address Access TLV.
--
-- ObjC selector: @- setMemoryData:@
setMemoryData :: (IsAVB17221AECPAddressAccessTLV avB17221AECPAddressAccessTLV, IsNSData value) => avB17221AECPAddressAccessTLV -> value -> IO ()
setMemoryData avB17221AECPAddressAccessTLV value =
  sendMessage avB17221AECPAddressAccessTLV setMemoryDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mode@
modeSelector :: Selector '[] AVB17221AECPAddressAccessTLVMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[AVB17221AECPAddressAccessTLVMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @address@
addressSelector :: Selector '[] CULong
addressSelector = mkSelector "address"

-- | @Selector@ for @setAddress:@
setAddressSelector :: Selector '[CULong] ()
setAddressSelector = mkSelector "setAddress:"

-- | @Selector@ for @memoryData@
memoryDataSelector :: Selector '[] (Id NSData)
memoryDataSelector = mkSelector "memoryData"

-- | @Selector@ for @setMemoryData:@
setMemoryDataSelector :: Selector '[Id NSData] ()
setMemoryDataSelector = mkSelector "setMemoryData:"

