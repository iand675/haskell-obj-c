{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterPacketProvider
--
-- The NEFilterPacketProvider class declares the programmatic interface for an object that evaluates network packets decisions about whether to block, allow, or delay the packets.
--
-- Generated bindings for @NEFilterPacketProvider@.
module ObjC.NetworkExtension.NEFilterPacketProvider
  ( NEFilterPacketProvider
  , IsNEFilterPacketProvider(..)
  , delayCurrentPacket
  , allowPacket
  , packetHandler
  , setPacketHandler
  , delayCurrentPacketSelector
  , allowPacketSelector
  , packetHandlerSelector
  , setPacketHandlerSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | delayCurrentPacket
--
-- This function is used to delay a packet currently presented by packetHandler.             This function is only valid within the packetHandler block and a verdict of             NEFilterPacketProviderVerdictDelay must be returned after a packet is delayed.  A delayed             packet will be prevented from continuing its journey through the networking stack until             it is either allowed by calling allow() or is dropped by being released.
--
-- @context@ — The context of the current packet filter which is passed to the packetHandler block.		   The packetHandler block must pass this context when calling delayCurrentPacket().
--
-- ObjC selector: @- delayCurrentPacket:@
delayCurrentPacket :: (IsNEFilterPacketProvider neFilterPacketProvider, IsNEFilterPacketContext context) => neFilterPacketProvider -> context -> IO (Id NEPacket)
delayCurrentPacket neFilterPacketProvider  context =
withObjCPtr context $ \raw_context ->
    sendMsg neFilterPacketProvider (mkSelector "delayCurrentPacket:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | allowPacket:
--
-- This function is used to allow a previously-delayed packet to continue its journey into or out of the networking stack.
--
-- @packet@ — A NEPacket object that contains the data of the packet that was previously delayed by the NEFilterPacketProvider.
--
-- ObjC selector: @- allowPacket:@
allowPacket :: (IsNEFilterPacketProvider neFilterPacketProvider, IsNEPacket packet) => neFilterPacketProvider -> packet -> IO ()
allowPacket neFilterPacketProvider  packet =
withObjCPtr packet $ \raw_packet ->
    sendMsg neFilterPacketProvider (mkSelector "allowPacket:") retVoid [argPtr (castPtr raw_packet :: Ptr ())]

-- | packetHandler
--
-- A block to be set to handle each packet received or to be sent.  A verdict             to allow, drop or delay must be returned to indicate the treatment of             the packet.  Since there may be multiple filtering sources presenting             frames to the provider, this packet handler may be executed by multiple			   simultaneous threads.  This packet handler must be able to handle execution			   in a multi-threaded environment.
--
-- ObjC selector: @- packetHandler@
packetHandler :: IsNEFilterPacketProvider neFilterPacketProvider => neFilterPacketProvider -> IO (Ptr ())
packetHandler neFilterPacketProvider  =
  fmap castPtr $ sendMsg neFilterPacketProvider (mkSelector "packetHandler") (retPtr retVoid) []

-- | packetHandler
--
-- A block to be set to handle each packet received or to be sent.  A verdict             to allow, drop or delay must be returned to indicate the treatment of             the packet.  Since there may be multiple filtering sources presenting             frames to the provider, this packet handler may be executed by multiple			   simultaneous threads.  This packet handler must be able to handle execution			   in a multi-threaded environment.
--
-- ObjC selector: @- setPacketHandler:@
setPacketHandler :: IsNEFilterPacketProvider neFilterPacketProvider => neFilterPacketProvider -> Ptr () -> IO ()
setPacketHandler neFilterPacketProvider  value =
  sendMsg neFilterPacketProvider (mkSelector "setPacketHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delayCurrentPacket:@
delayCurrentPacketSelector :: Selector
delayCurrentPacketSelector = mkSelector "delayCurrentPacket:"

-- | @Selector@ for @allowPacket:@
allowPacketSelector :: Selector
allowPacketSelector = mkSelector "allowPacket:"

-- | @Selector@ for @packetHandler@
packetHandlerSelector :: Selector
packetHandlerSelector = mkSelector "packetHandler"

-- | @Selector@ for @setPacketHandler:@
setPacketHandlerSelector :: Selector
setPacketHandlerSelector = mkSelector "setPacketHandler:"

