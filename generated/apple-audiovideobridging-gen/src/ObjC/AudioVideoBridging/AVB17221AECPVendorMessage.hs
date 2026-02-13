{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPVendorMessage
--
-- AVB17221AECPVendorMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Vendor Unique message.
--
-- AVB17221AECPVendorMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Vendor Unique message.				This class is a concrete subclass of AVB17221AECPMessage which provides support for the AEM messages.
--
-- Generated bindings for @AVB17221AECPVendorMessage@.
module ObjC.AudioVideoBridging.AVB17221AECPVendorMessage
  ( AVB17221AECPVendorMessage
  , IsAVB17221AECPVendorMessage(..)
  , protocolID
  , setProtocolID
  , protocolSpecificData
  , setProtocolSpecificData
  , protocolIDSelector
  , protocolSpecificDataSelector
  , setProtocolIDSelector
  , setProtocolSpecificDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | protocolID
--
-- The protocol_id field of the AECP Vendor Unique message.
--
-- ObjC selector: @- protocolID@
protocolID :: IsAVB17221AECPVendorMessage avB17221AECPVendorMessage => avB17221AECPVendorMessage -> IO CULong
protocolID avB17221AECPVendorMessage =
  sendMessage avB17221AECPVendorMessage protocolIDSelector

-- | protocolID
--
-- The protocol_id field of the AECP Vendor Unique message.
--
-- ObjC selector: @- setProtocolID:@
setProtocolID :: IsAVB17221AECPVendorMessage avB17221AECPVendorMessage => avB17221AECPVendorMessage -> CULong -> IO ()
setProtocolID avB17221AECPVendorMessage value =
  sendMessage avB17221AECPVendorMessage setProtocolIDSelector value

-- | protocolSpecificData
--
-- The protocol_specific_data field of the AECP Vendor Unique message.
--
-- ObjC selector: @- protocolSpecificData@
protocolSpecificData :: IsAVB17221AECPVendorMessage avB17221AECPVendorMessage => avB17221AECPVendorMessage -> IO (Id NSData)
protocolSpecificData avB17221AECPVendorMessage =
  sendMessage avB17221AECPVendorMessage protocolSpecificDataSelector

-- | protocolSpecificData
--
-- The protocol_specific_data field of the AECP Vendor Unique message.
--
-- ObjC selector: @- setProtocolSpecificData:@
setProtocolSpecificData :: (IsAVB17221AECPVendorMessage avB17221AECPVendorMessage, IsNSData value) => avB17221AECPVendorMessage -> value -> IO ()
setProtocolSpecificData avB17221AECPVendorMessage value =
  sendMessage avB17221AECPVendorMessage setProtocolSpecificDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @protocolID@
protocolIDSelector :: Selector '[] CULong
protocolIDSelector = mkSelector "protocolID"

-- | @Selector@ for @setProtocolID:@
setProtocolIDSelector :: Selector '[CULong] ()
setProtocolIDSelector = mkSelector "setProtocolID:"

-- | @Selector@ for @protocolSpecificData@
protocolSpecificDataSelector :: Selector '[] (Id NSData)
protocolSpecificDataSelector = mkSelector "protocolSpecificData"

-- | @Selector@ for @setProtocolSpecificData:@
setProtocolSpecificDataSelector :: Selector '[Id NSData] ()
setProtocolSpecificDataSelector = mkSelector "setProtocolSpecificData:"

