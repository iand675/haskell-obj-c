{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPAVCMessage
--
-- AVB17221AECPAVCMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Legacy AV/C message.
--
-- AVB17221AECPAVCMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Legacy AV/C message.				This class is a concrete subclass of AVB17221AECPMessage which provides support for the Legacy AV/C messages.
--
-- Generated bindings for @AVB17221AECPAVCMessage@.
module ObjC.AudioVideoBridging.AVB17221AECPAVCMessage
  ( AVB17221AECPAVCMessage
  , IsAVB17221AECPAVCMessage(..)
  , commandResponse
  , setCommandResponse
  , commandResponseSelector
  , setCommandResponseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | commandResponse
--
-- The avc_command_response field of the AECP AEM message.
--
-- ObjC selector: @- commandResponse@
commandResponse :: IsAVB17221AECPAVCMessage avB17221AECPAVCMessage => avB17221AECPAVCMessage -> IO (Id NSData)
commandResponse avB17221AECPAVCMessage =
  sendMessage avB17221AECPAVCMessage commandResponseSelector

-- | commandResponse
--
-- The avc_command_response field of the AECP AEM message.
--
-- ObjC selector: @- setCommandResponse:@
setCommandResponse :: (IsAVB17221AECPAVCMessage avB17221AECPAVCMessage, IsNSData value) => avB17221AECPAVCMessage -> value -> IO ()
setCommandResponse avB17221AECPAVCMessage value =
  sendMessage avB17221AECPAVCMessage setCommandResponseSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandResponse@
commandResponseSelector :: Selector '[] (Id NSData)
commandResponseSelector = mkSelector "commandResponse"

-- | @Selector@ for @setCommandResponse:@
setCommandResponseSelector :: Selector '[Id NSData] ()
setCommandResponseSelector = mkSelector "setCommandResponse:"

