{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPAddressAccessMessage
--
-- AVB17221AECPAddressAccessMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Address Access message.
--
-- AVB17221AECPAddressAccessMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Address Access message.				This class is a concrete subclass of AVB17221AECPMessage which provides support for the Address Access messages.
--
-- Generated bindings for @AVB17221AECPAddressAccessMessage@.
module ObjC.AudioVideoBridging.AVB17221AECPAddressAccessMessage
  ( AVB17221AECPAddressAccessMessage
  , IsAVB17221AECPAddressAccessMessage(..)
  , commandMessage
  , responseMessage
  , tlvs
  , setTlvs
  , commandMessageSelector
  , responseMessageSelector
  , tlvsSelector
  , setTlvsSelector


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

-- | commandMessage
--
-- This method returns an AVB17221AECPAddressAccessMessage instance setup as an Address Access command.
--
-- Returns: An AVB17221AECPAddressAccessMessage instance pre-setup as an Address Access command.
--
-- ObjC selector: @+ commandMessage@
commandMessage :: IO (Id AVB17221AECPAddressAccessMessage)
commandMessage  =
  do
    cls' <- getRequiredClass "AVB17221AECPAddressAccessMessage"
    sendClassMsg cls' (mkSelector "commandMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | responseMessage
--
-- This method returns an AVB17221AECPAEMMessage instance setup as an Address Access response.
--
-- Returns: An AVB17221AECPAddressAccessMessage instance pre-setup as an Address Access response.
--
-- ObjC selector: @+ responseMessage@
responseMessage :: IO (Id AVB17221AECPAddressAccessMessage)
responseMessage  =
  do
    cls' <- getRequiredClass "AVB17221AECPAddressAccessMessage"
    sendClassMsg cls' (mkSelector "responseMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tlvs
--
-- An array of AVB17221AECPAddressAccessTLV objects representing the tlv_data field of the AECP Address Access message.
--
-- ObjC selector: @- tlvs@
tlvs :: IsAVB17221AECPAddressAccessMessage avB17221AECPAddressAccessMessage => avB17221AECPAddressAccessMessage -> IO (Id NSArray)
tlvs avB17221AECPAddressAccessMessage  =
  sendMsg avB17221AECPAddressAccessMessage (mkSelector "tlvs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tlvs
--
-- An array of AVB17221AECPAddressAccessTLV objects representing the tlv_data field of the AECP Address Access message.
--
-- ObjC selector: @- setTlvs:@
setTlvs :: (IsAVB17221AECPAddressAccessMessage avB17221AECPAddressAccessMessage, IsNSArray value) => avB17221AECPAddressAccessMessage -> value -> IO ()
setTlvs avB17221AECPAddressAccessMessage  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221AECPAddressAccessMessage (mkSelector "setTlvs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandMessage@
commandMessageSelector :: Selector
commandMessageSelector = mkSelector "commandMessage"

-- | @Selector@ for @responseMessage@
responseMessageSelector :: Selector
responseMessageSelector = mkSelector "responseMessage"

-- | @Selector@ for @tlvs@
tlvsSelector :: Selector
tlvsSelector = mkSelector "tlvs"

-- | @Selector@ for @setTlvs:@
setTlvsSelector :: Selector
setTlvsSelector = mkSelector "setTlvs:"

