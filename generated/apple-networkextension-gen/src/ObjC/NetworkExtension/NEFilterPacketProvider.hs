{-# LANGUAGE DataKinds #-}
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
  , allowPacketSelector
  , delayCurrentPacketSelector
  , packetHandlerSelector
  , setPacketHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
delayCurrentPacket neFilterPacketProvider context =
  sendMessage neFilterPacketProvider delayCurrentPacketSelector (toNEFilterPacketContext context)

-- | allowPacket:
--
-- This function is used to allow a previously-delayed packet to continue its journey into or out of the networking stack.
--
-- @packet@ — A NEPacket object that contains the data of the packet that was previously delayed by the NEFilterPacketProvider.
--
-- ObjC selector: @- allowPacket:@
allowPacket :: (IsNEFilterPacketProvider neFilterPacketProvider, IsNEPacket packet) => neFilterPacketProvider -> packet -> IO ()
allowPacket neFilterPacketProvider packet =
  sendMessage neFilterPacketProvider allowPacketSelector (toNEPacket packet)

-- | packetHandler
--
-- A block to be set to handle each packet received or to be sent.  A verdict             to allow, drop or delay must be returned to indicate the treatment of             the packet.  Since there may be multiple filtering sources presenting             frames to the provider, this packet handler may be executed by multiple			   simultaneous threads.  This packet handler must be able to handle execution			   in a multi-threaded environment.
--
-- ObjC selector: @- packetHandler@
packetHandler :: IsNEFilterPacketProvider neFilterPacketProvider => neFilterPacketProvider -> IO (Ptr ())
packetHandler neFilterPacketProvider =
  sendMessage neFilterPacketProvider packetHandlerSelector

-- | packetHandler
--
-- A block to be set to handle each packet received or to be sent.  A verdict             to allow, drop or delay must be returned to indicate the treatment of             the packet.  Since there may be multiple filtering sources presenting             frames to the provider, this packet handler may be executed by multiple			   simultaneous threads.  This packet handler must be able to handle execution			   in a multi-threaded environment.
--
-- ObjC selector: @- setPacketHandler:@
setPacketHandler :: IsNEFilterPacketProvider neFilterPacketProvider => neFilterPacketProvider -> Ptr () -> IO ()
setPacketHandler neFilterPacketProvider value =
  sendMessage neFilterPacketProvider setPacketHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delayCurrentPacket:@
delayCurrentPacketSelector :: Selector '[Id NEFilterPacketContext] (Id NEPacket)
delayCurrentPacketSelector = mkSelector "delayCurrentPacket:"

-- | @Selector@ for @allowPacket:@
allowPacketSelector :: Selector '[Id NEPacket] ()
allowPacketSelector = mkSelector "allowPacket:"

-- | @Selector@ for @packetHandler@
packetHandlerSelector :: Selector '[] (Ptr ())
packetHandlerSelector = mkSelector "packetHandler"

-- | @Selector@ for @setPacketHandler:@
setPacketHandlerSelector :: Selector '[Ptr ()] ()
setPacketHandlerSelector = mkSelector "setPacketHandler:"

