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
  , interfaceWithProtocolSelector
  , setClasses_forSelector_argumentIndex_ofReplySelector
  , classesForSelector_argumentIndex_ofReplySelector
  , setInterface_forSelector_argumentIndex_ofReplySelector
  , interfaceForSelector_argumentIndex_ofReplySelector
  , setXPCType_forSelector_argumentIndex_ofReplySelector
  , xpcTypeForSelector_argumentIndex_ofReplySelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ interfaceWithProtocol:@
interfaceWithProtocol :: RawId -> IO (Id NSXPCInterface)
interfaceWithProtocol protocol =
  do
    cls' <- getRequiredClass "NSXPCInterface"
    sendClassMsg cls' (mkSelector "interfaceWithProtocol:") (retPtr retVoid) [argPtr (castPtr (unRawId protocol) :: Ptr ())] >>= retainedObject . castPtr

-- | @- setClasses:forSelector:argumentIndex:ofReply:@
setClasses_forSelector_argumentIndex_ofReply :: (IsNSXPCInterface nsxpcInterface, IsNSSet classes) => nsxpcInterface -> classes -> Selector -> CULong -> Bool -> IO ()
setClasses_forSelector_argumentIndex_ofReply nsxpcInterface  classes sel arg ofReply =
withObjCPtr classes $ \raw_classes ->
    sendMsg nsxpcInterface (mkSelector "setClasses:forSelector:argumentIndex:ofReply:") retVoid [argPtr (castPtr raw_classes :: Ptr ()), argPtr (unSelector sel), argCULong (fromIntegral arg), argCULong (if ofReply then 1 else 0)]

-- | @- classesForSelector:argumentIndex:ofReply:@
classesForSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> Selector -> CULong -> Bool -> IO (Id NSSet)
classesForSelector_argumentIndex_ofReply nsxpcInterface  sel arg ofReply =
  sendMsg nsxpcInterface (mkSelector "classesForSelector:argumentIndex:ofReply:") (retPtr retVoid) [argPtr (unSelector sel), argCULong (fromIntegral arg), argCULong (if ofReply then 1 else 0)] >>= retainedObject . castPtr

-- | @- setInterface:forSelector:argumentIndex:ofReply:@
setInterface_forSelector_argumentIndex_ofReply :: (IsNSXPCInterface nsxpcInterface, IsNSXPCInterface ifc) => nsxpcInterface -> ifc -> Selector -> CULong -> Bool -> IO ()
setInterface_forSelector_argumentIndex_ofReply nsxpcInterface  ifc sel arg ofReply =
withObjCPtr ifc $ \raw_ifc ->
    sendMsg nsxpcInterface (mkSelector "setInterface:forSelector:argumentIndex:ofReply:") retVoid [argPtr (castPtr raw_ifc :: Ptr ()), argPtr (unSelector sel), argCULong (fromIntegral arg), argCULong (if ofReply then 1 else 0)]

-- | @- interfaceForSelector:argumentIndex:ofReply:@
interfaceForSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> Selector -> CULong -> Bool -> IO (Id NSXPCInterface)
interfaceForSelector_argumentIndex_ofReply nsxpcInterface  sel arg ofReply =
  sendMsg nsxpcInterface (mkSelector "interfaceForSelector:argumentIndex:ofReply:") (retPtr retVoid) [argPtr (unSelector sel), argCULong (fromIntegral arg), argCULong (if ofReply then 1 else 0)] >>= retainedObject . castPtr

-- | @- setXPCType:forSelector:argumentIndex:ofReply:@
setXPCType_forSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> RawId -> Selector -> CULong -> Bool -> IO ()
setXPCType_forSelector_argumentIndex_ofReply nsxpcInterface  type_ sel arg ofReply =
  sendMsg nsxpcInterface (mkSelector "setXPCType:forSelector:argumentIndex:ofReply:") retVoid [argPtr (castPtr (unRawId type_) :: Ptr ()), argPtr (unSelector sel), argCULong (fromIntegral arg), argCULong (if ofReply then 1 else 0)]

-- | @- XPCTypeForSelector:argumentIndex:ofReply:@
xpcTypeForSelector_argumentIndex_ofReply :: IsNSXPCInterface nsxpcInterface => nsxpcInterface -> Selector -> CULong -> Bool -> IO RawId
xpcTypeForSelector_argumentIndex_ofReply nsxpcInterface  sel arg ofReply =
  fmap (RawId . castPtr) $ sendMsg nsxpcInterface (mkSelector "XPCTypeForSelector:argumentIndex:ofReply:") (retPtr retVoid) [argPtr (unSelector sel), argCULong (fromIntegral arg), argCULong (if ofReply then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interfaceWithProtocol:@
interfaceWithProtocolSelector :: Selector
interfaceWithProtocolSelector = mkSelector "interfaceWithProtocol:"

-- | @Selector@ for @setClasses:forSelector:argumentIndex:ofReply:@
setClasses_forSelector_argumentIndex_ofReplySelector :: Selector
setClasses_forSelector_argumentIndex_ofReplySelector = mkSelector "setClasses:forSelector:argumentIndex:ofReply:"

-- | @Selector@ for @classesForSelector:argumentIndex:ofReply:@
classesForSelector_argumentIndex_ofReplySelector :: Selector
classesForSelector_argumentIndex_ofReplySelector = mkSelector "classesForSelector:argumentIndex:ofReply:"

-- | @Selector@ for @setInterface:forSelector:argumentIndex:ofReply:@
setInterface_forSelector_argumentIndex_ofReplySelector :: Selector
setInterface_forSelector_argumentIndex_ofReplySelector = mkSelector "setInterface:forSelector:argumentIndex:ofReply:"

-- | @Selector@ for @interfaceForSelector:argumentIndex:ofReply:@
interfaceForSelector_argumentIndex_ofReplySelector :: Selector
interfaceForSelector_argumentIndex_ofReplySelector = mkSelector "interfaceForSelector:argumentIndex:ofReply:"

-- | @Selector@ for @setXPCType:forSelector:argumentIndex:ofReply:@
setXPCType_forSelector_argumentIndex_ofReplySelector :: Selector
setXPCType_forSelector_argumentIndex_ofReplySelector = mkSelector "setXPCType:forSelector:argumentIndex:ofReply:"

-- | @Selector@ for @XPCTypeForSelector:argumentIndex:ofReply:@
xpcTypeForSelector_argumentIndex_ofReplySelector :: Selector
xpcTypeForSelector_argumentIndex_ofReplySelector = mkSelector "XPCTypeForSelector:argumentIndex:ofReply:"

