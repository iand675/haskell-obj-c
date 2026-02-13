{-# LANGUAGE DataKinds #-}
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
  , setTlvsSelector
  , tlvsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' commandMessageSelector

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
    sendClassMessage cls' responseMessageSelector

-- | tlvs
--
-- An array of AVB17221AECPAddressAccessTLV objects representing the tlv_data field of the AECP Address Access message.
--
-- ObjC selector: @- tlvs@
tlvs :: IsAVB17221AECPAddressAccessMessage avB17221AECPAddressAccessMessage => avB17221AECPAddressAccessMessage -> IO (Id NSArray)
tlvs avB17221AECPAddressAccessMessage =
  sendMessage avB17221AECPAddressAccessMessage tlvsSelector

-- | tlvs
--
-- An array of AVB17221AECPAddressAccessTLV objects representing the tlv_data field of the AECP Address Access message.
--
-- ObjC selector: @- setTlvs:@
setTlvs :: (IsAVB17221AECPAddressAccessMessage avB17221AECPAddressAccessMessage, IsNSArray value) => avB17221AECPAddressAccessMessage -> value -> IO ()
setTlvs avB17221AECPAddressAccessMessage value =
  sendMessage avB17221AECPAddressAccessMessage setTlvsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandMessage@
commandMessageSelector :: Selector '[] (Id AVB17221AECPAddressAccessMessage)
commandMessageSelector = mkSelector "commandMessage"

-- | @Selector@ for @responseMessage@
responseMessageSelector :: Selector '[] (Id AVB17221AECPAddressAccessMessage)
responseMessageSelector = mkSelector "responseMessage"

-- | @Selector@ for @tlvs@
tlvsSelector :: Selector '[] (Id NSArray)
tlvsSelector = mkSelector "tlvs"

-- | @Selector@ for @setTlvs:@
setTlvsSelector :: Selector '[Id NSArray] ()
setTlvsSelector = mkSelector "setTlvs:"

