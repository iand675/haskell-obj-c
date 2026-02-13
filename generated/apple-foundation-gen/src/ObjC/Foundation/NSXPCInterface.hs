{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSXPCInterface@.
module ObjC.Foundation.NSXPCInterface
  ( NSXPCInterface
  , IsNSXPCInterface(..)
  , interfaceWithProtocol
  , setClasses_forSelector_argumentIndex_ofReply
  , classesForSelector_argumentIndex_ofReply
  , setInterface_forSelector_argumentIndex_ofReply
  , interfaceForSelector_argumentIndex_ofReply
  , setXPCType_forSelector_argumentIndex_ofReply
  , xpcTypeForSelector_argumentIndex_ofReply
  , protocol
  , setProtocol
  , classesForSelector_argumentIndex_ofReplySelector
  , interfaceForSelector_argumentIndex_ofReplySelector
  , interfaceWithProtocolSelector
  , protocolSelector
  , setClasses_forSelector_argumentIndex_ofReplySelector
  , setInterface_forSelector_argumentIndex_ofReplySelector
  , setProtocolSelector
  , setXPCType_forSelector_argumentIndex_ofReplySelector
  , xpcTypeForSelector_argumentIndex_ofReplySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ interfaceWithProtocol:@
interfaceWithProtocol :: RawId -> IO (Id NSXPCInterface)
interfaceWithProtocol protocol =
  do
    cls' <- getRequiredClass "NSXPCInterface"
    sendClassMessage cls' interfaceWithProtocolSelector protocol

-- | @- setClasses:forSelector:argumentIndex:ofReply:@
setClasses_forSelector_argumentIndex_ofReply :: (IsNSXPCInterface nsxpcInterface, IsNSSet classes) => nsxpcInterface -> classes -> Sel -> CULong -> Bool -> IO ()
setClasses_forSelector_argumentIndex_ofReply nsxpcInterface classes sel arg ofReply =
  sendMessage nsxpcInterface setClasses_forSelector_argumentIndex_ofReplySelector (toNSSet classes) sel arg ofReply

-- | @- classesForSelector:argumentIndex:ofReply:@
classesForSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> Sel -> CULong -> Bool -> IO (Id NSSet)
classesForSelector_argumentIndex_ofReply nsxpcInterface sel arg ofReply =
  sendMessage nsxpcInterface classesForSelector_argumentIndex_ofReplySelector sel arg ofReply

-- | @- setInterface:forSelector:argumentIndex:ofReply:@
setInterface_forSelector_argumentIndex_ofReply :: (IsNSXPCInterface nsxpcInterface, IsNSXPCInterface ifc) => nsxpcInterface -> ifc -> Sel -> CULong -> Bool -> IO ()
setInterface_forSelector_argumentIndex_ofReply nsxpcInterface ifc sel arg ofReply =
  sendMessage nsxpcInterface setInterface_forSelector_argumentIndex_ofReplySelector (toNSXPCInterface ifc) sel arg ofReply

-- | @- interfaceForSelector:argumentIndex:ofReply:@
interfaceForSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> Sel -> CULong -> Bool -> IO (Id NSXPCInterface)
interfaceForSelector_argumentIndex_ofReply nsxpcInterface sel arg ofReply =
  sendMessage nsxpcInterface interfaceForSelector_argumentIndex_ofReplySelector sel arg ofReply

-- | @- setXPCType:forSelector:argumentIndex:ofReply:@
setXPCType_forSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> RawId -> Sel -> CULong -> Bool -> IO ()
setXPCType_forSelector_argumentIndex_ofReply nsxpcInterface type_ sel arg ofReply =
  sendMessage nsxpcInterface setXPCType_forSelector_argumentIndex_ofReplySelector type_ sel arg ofReply

-- | @- XPCTypeForSelector:argumentIndex:ofReply:@
xpcTypeForSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> Sel -> CULong -> Bool -> IO RawId
xpcTypeForSelector_argumentIndex_ofReply nsxpcInterface sel arg ofReply =
  sendMessage nsxpcInterface xpcTypeForSelector_argumentIndex_ofReplySelector sel arg ofReply

-- | @- protocol@
protocol :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> IO RawId
protocol nsxpcInterface =
  sendMessage nsxpcInterface protocolSelector

-- | @- setProtocol:@
setProtocol :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> RawId -> IO ()
setProtocol nsxpcInterface value =
  sendMessage nsxpcInterface setProtocolSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interfaceWithProtocol:@
interfaceWithProtocolSelector :: Selector '[RawId] (Id NSXPCInterface)
interfaceWithProtocolSelector = mkSelector "interfaceWithProtocol:"

-- | @Selector@ for @setClasses:forSelector:argumentIndex:ofReply:@
setClasses_forSelector_argumentIndex_ofReplySelector :: Selector '[Id NSSet, Sel, CULong, Bool] ()
setClasses_forSelector_argumentIndex_ofReplySelector = mkSelector "setClasses:forSelector:argumentIndex:ofReply:"

-- | @Selector@ for @classesForSelector:argumentIndex:ofReply:@
classesForSelector_argumentIndex_ofReplySelector :: Selector '[Sel, CULong, Bool] (Id NSSet)
classesForSelector_argumentIndex_ofReplySelector = mkSelector "classesForSelector:argumentIndex:ofReply:"

-- | @Selector@ for @setInterface:forSelector:argumentIndex:ofReply:@
setInterface_forSelector_argumentIndex_ofReplySelector :: Selector '[Id NSXPCInterface, Sel, CULong, Bool] ()
setInterface_forSelector_argumentIndex_ofReplySelector = mkSelector "setInterface:forSelector:argumentIndex:ofReply:"

-- | @Selector@ for @interfaceForSelector:argumentIndex:ofReply:@
interfaceForSelector_argumentIndex_ofReplySelector :: Selector '[Sel, CULong, Bool] (Id NSXPCInterface)
interfaceForSelector_argumentIndex_ofReplySelector = mkSelector "interfaceForSelector:argumentIndex:ofReply:"

-- | @Selector@ for @setXPCType:forSelector:argumentIndex:ofReply:@
setXPCType_forSelector_argumentIndex_ofReplySelector :: Selector '[RawId, Sel, CULong, Bool] ()
setXPCType_forSelector_argumentIndex_ofReplySelector = mkSelector "setXPCType:forSelector:argumentIndex:ofReply:"

-- | @Selector@ for @XPCTypeForSelector:argumentIndex:ofReply:@
xpcTypeForSelector_argumentIndex_ofReplySelector :: Selector '[Sel, CULong, Bool] RawId
xpcTypeForSelector_argumentIndex_ofReplySelector = mkSelector "XPCTypeForSelector:argumentIndex:ofReply:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] RawId
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @setProtocol:@
setProtocolSelector :: Selector '[RawId] ()
setProtocolSelector = mkSelector "setProtocol:"

