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
  , setProtocolIDSelector
  , protocolSpecificDataSelector
  , setProtocolSpecificDataSelector


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
import ObjC.Foundation.Internal.Classes

-- | protocolID
--
-- The protocol_id field of the AECP Vendor Unique message.
--
-- ObjC selector: @- protocolID@
protocolID :: IsAVB17221AECPVendorMessage avB17221AECPVendorMessage => avB17221AECPVendorMessage -> IO CULong
protocolID avB17221AECPVendorMessage  =
  sendMsg avB17221AECPVendorMessage (mkSelector "protocolID") retCULong []

-- | protocolID
--
-- The protocol_id field of the AECP Vendor Unique message.
--
-- ObjC selector: @- setProtocolID:@
setProtocolID :: IsAVB17221AECPVendorMessage avB17221AECPVendorMessage => avB17221AECPVendorMessage -> CULong -> IO ()
setProtocolID avB17221AECPVendorMessage  value =
  sendMsg avB17221AECPVendorMessage (mkSelector "setProtocolID:") retVoid [argCULong (fromIntegral value)]

-- | protocolSpecificData
--
-- The protocol_specific_data field of the AECP Vendor Unique message.
--
-- ObjC selector: @- protocolSpecificData@
protocolSpecificData :: IsAVB17221AECPVendorMessage avB17221AECPVendorMessage => avB17221AECPVendorMessage -> IO (Id NSData)
protocolSpecificData avB17221AECPVendorMessage  =
  sendMsg avB17221AECPVendorMessage (mkSelector "protocolSpecificData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | protocolSpecificData
--
-- The protocol_specific_data field of the AECP Vendor Unique message.
--
-- ObjC selector: @- setProtocolSpecificData:@
setProtocolSpecificData :: (IsAVB17221AECPVendorMessage avB17221AECPVendorMessage, IsNSData value) => avB17221AECPVendorMessage -> value -> IO ()
setProtocolSpecificData avB17221AECPVendorMessage  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221AECPVendorMessage (mkSelector "setProtocolSpecificData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @protocolID@
protocolIDSelector :: Selector
protocolIDSelector = mkSelector "protocolID"

-- | @Selector@ for @setProtocolID:@
setProtocolIDSelector :: Selector
setProtocolIDSelector = mkSelector "setProtocolID:"

-- | @Selector@ for @protocolSpecificData@
protocolSpecificDataSelector :: Selector
protocolSpecificDataSelector = mkSelector "protocolSpecificData"

-- | @Selector@ for @setProtocolSpecificData:@
setProtocolSpecificDataSelector :: Selector
setProtocolSpecificDataSelector = mkSelector "setProtocolSpecificData:"

