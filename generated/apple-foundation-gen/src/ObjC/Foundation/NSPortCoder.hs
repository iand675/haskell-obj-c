{-# LANGUAGE DataKinds #-}
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
  , connectionSelector
  , decodePortObjectSelector
  , dispatchSelector
  , encodePortObjectSelector
  , initWithReceivePort_sendPort_componentsSelector
  , isBycopySelector
  , isByrefSelector
  , portCoderWithReceivePort_sendPort_componentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- isBycopy@
isBycopy :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO Bool
isBycopy nsPortCoder =
  sendMessage nsPortCoder isBycopySelector

-- | @- isByref@
isByref :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO Bool
isByref nsPortCoder =
  sendMessage nsPortCoder isByrefSelector

-- | @- encodePortObject:@
encodePortObject :: (IsNSPortCoder nsPortCoder, IsNSPort aport) => nsPortCoder -> aport -> IO ()
encodePortObject nsPortCoder aport =
  sendMessage nsPortCoder encodePortObjectSelector (toNSPort aport)

-- | @- decodePortObject@
decodePortObject :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO (Id NSPort)
decodePortObject nsPortCoder =
  sendMessage nsPortCoder decodePortObjectSelector

-- | @- connection@
connection :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO (Id NSConnection)
connection nsPortCoder =
  sendMessage nsPortCoder connectionSelector

-- | @+ portCoderWithReceivePort:sendPort:components:@
portCoderWithReceivePort_sendPort_components :: (IsNSPort rcvPort, IsNSPort sndPort, IsNSArray comps) => rcvPort -> sndPort -> comps -> IO RawId
portCoderWithReceivePort_sendPort_components rcvPort sndPort comps =
  do
    cls' <- getRequiredClass "NSPortCoder"
    sendClassMessage cls' portCoderWithReceivePort_sendPort_componentsSelector (toNSPort rcvPort) (toNSPort sndPort) (toNSArray comps)

-- | @- initWithReceivePort:sendPort:components:@
initWithReceivePort_sendPort_components :: (IsNSPortCoder nsPortCoder, IsNSPort rcvPort, IsNSPort sndPort, IsNSArray comps) => nsPortCoder -> rcvPort -> sndPort -> comps -> IO RawId
initWithReceivePort_sendPort_components nsPortCoder rcvPort sndPort comps =
  sendOwnedMessage nsPortCoder initWithReceivePort_sendPort_componentsSelector (toNSPort rcvPort) (toNSPort sndPort) (toNSArray comps)

-- | @- dispatch@
dispatch :: IsNSPortCoder nsPortCoder => nsPortCoder -> IO ()
dispatch nsPortCoder =
  sendMessage nsPortCoder dispatchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isBycopy@
isBycopySelector :: Selector '[] Bool
isBycopySelector = mkSelector "isBycopy"

-- | @Selector@ for @isByref@
isByrefSelector :: Selector '[] Bool
isByrefSelector = mkSelector "isByref"

-- | @Selector@ for @encodePortObject:@
encodePortObjectSelector :: Selector '[Id NSPort] ()
encodePortObjectSelector = mkSelector "encodePortObject:"

-- | @Selector@ for @decodePortObject@
decodePortObjectSelector :: Selector '[] (Id NSPort)
decodePortObjectSelector = mkSelector "decodePortObject"

-- | @Selector@ for @connection@
connectionSelector :: Selector '[] (Id NSConnection)
connectionSelector = mkSelector "connection"

-- | @Selector@ for @portCoderWithReceivePort:sendPort:components:@
portCoderWithReceivePort_sendPort_componentsSelector :: Selector '[Id NSPort, Id NSPort, Id NSArray] RawId
portCoderWithReceivePort_sendPort_componentsSelector = mkSelector "portCoderWithReceivePort:sendPort:components:"

-- | @Selector@ for @initWithReceivePort:sendPort:components:@
initWithReceivePort_sendPort_componentsSelector :: Selector '[Id NSPort, Id NSPort, Id NSArray] RawId
initWithReceivePort_sendPort_componentsSelector = mkSelector "initWithReceivePort:sendPort:components:"

-- | @Selector@ for @dispatch@
dispatchSelector :: Selector '[] ()
dispatchSelector = mkSelector "dispatch"

