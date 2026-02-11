{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPortCoder@.
module ObjC.Foundation.NSPortCoder
  ( NSPortCoder
  , IsNSPortCoder(..)
  , isBycopy
  , isByref
  , encodePortObject
  , decodePortObject
  , connection
  , portCoderWithReceivePort_sendPort_components
  , initWithReceivePort_sendPort_components
  , dispatch
  , isBycopySelector
  , isByrefSelector
  , encodePortObjectSelector
  , decodePortObjectSelector
  , connectionSelector
  , portCoderWithReceivePort_sendPort_componentsSelector
  , initWithReceivePort_sendPort_componentsSelector
  , dispatchSelector


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

-- | @- isBycopy@
isBycopy :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO Bool
isBycopy nsPortCoder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPortCoder (mkSelector "isBycopy") retCULong []

-- | @- isByref@
isByref :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO Bool
isByref nsPortCoder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPortCoder (mkSelector "isByref") retCULong []

-- | @- encodePortObject:@
encodePortObject :: (IsNSPortCoder nsPortCoder, IsNSPort aport) => nsPortCoder -> aport -> IO ()
encodePortObject nsPortCoder  aport =
withObjCPtr aport $ \raw_aport ->
    sendMsg nsPortCoder (mkSelector "encodePortObject:") retVoid [argPtr (castPtr raw_aport :: Ptr ())]

-- | @- decodePortObject@
decodePortObject :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO (Id NSPort)
decodePortObject nsPortCoder  =
  sendMsg nsPortCoder (mkSelector "decodePortObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- connection@
connection :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO (Id NSConnection)
connection nsPortCoder  =
  sendMsg nsPortCoder (mkSelector "connection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ portCoderWithReceivePort:sendPort:components:@
portCoderWithReceivePort_sendPort_components :: (IsNSPort rcvPort, IsNSPort sndPort, IsNSArray comps) => rcvPort -> sndPort -> comps -> IO RawId
portCoderWithReceivePort_sendPort_components rcvPort sndPort comps =
  do
    cls' <- getRequiredClass "NSPortCoder"
    withObjCPtr rcvPort $ \raw_rcvPort ->
      withObjCPtr sndPort $ \raw_sndPort ->
        withObjCPtr comps $ \raw_comps ->
          fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "portCoderWithReceivePort:sendPort:components:") (retPtr retVoid) [argPtr (castPtr raw_rcvPort :: Ptr ()), argPtr (castPtr raw_sndPort :: Ptr ()), argPtr (castPtr raw_comps :: Ptr ())]

-- | @- initWithReceivePort:sendPort:components:@
initWithReceivePort_sendPort_components :: (IsNSPortCoder nsPortCoder, IsNSPort rcvPort, IsNSPort sndPort, IsNSArray comps) => nsPortCoder -> rcvPort -> sndPort -> comps -> IO RawId
initWithReceivePort_sendPort_components nsPortCoder  rcvPort sndPort comps =
withObjCPtr rcvPort $ \raw_rcvPort ->
  withObjCPtr sndPort $ \raw_sndPort ->
    withObjCPtr comps $ \raw_comps ->
        fmap (RawId . castPtr) $ sendMsg nsPortCoder (mkSelector "initWithReceivePort:sendPort:components:") (retPtr retVoid) [argPtr (castPtr raw_rcvPort :: Ptr ()), argPtr (castPtr raw_sndPort :: Ptr ()), argPtr (castPtr raw_comps :: Ptr ())]

-- | @- dispatch@
dispatch :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO ()
dispatch nsPortCoder  =
  sendMsg nsPortCoder (mkSelector "dispatch") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isBycopy@
isBycopySelector :: Selector
isBycopySelector = mkSelector "isBycopy"

-- | @Selector@ for @isByref@
isByrefSelector :: Selector
isByrefSelector = mkSelector "isByref"

-- | @Selector@ for @encodePortObject:@
encodePortObjectSelector :: Selector
encodePortObjectSelector = mkSelector "encodePortObject:"

-- | @Selector@ for @decodePortObject@
decodePortObjectSelector :: Selector
decodePortObjectSelector = mkSelector "decodePortObject"

-- | @Selector@ for @connection@
connectionSelector :: Selector
connectionSelector = mkSelector "connection"

-- | @Selector@ for @portCoderWithReceivePort:sendPort:components:@
portCoderWithReceivePort_sendPort_componentsSelector :: Selector
portCoderWithReceivePort_sendPort_componentsSelector = mkSelector "portCoderWithReceivePort:sendPort:components:"

-- | @Selector@ for @initWithReceivePort:sendPort:components:@
initWithReceivePort_sendPort_componentsSelector :: Selector
initWithReceivePort_sendPort_componentsSelector = mkSelector "initWithReceivePort:sendPort:components:"

-- | @Selector@ for @dispatch@
dispatchSelector :: Selector
dispatchSelector = mkSelector "dispatch"

