{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProtocolChecker@.
module ObjC.Foundation.NSProtocolChecker
  ( NSProtocolChecker
  , IsNSProtocolChecker(..)
  , protocolCheckerWithTarget_protocol
  , initWithTarget_protocol
  , protocol
  , target
  , initWithTarget_protocolSelector
  , protocolCheckerWithTarget_protocolSelector
  , protocolSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ protocolCheckerWithTarget:protocol:@
protocolCheckerWithTarget_protocol :: IsNSObject anObject => anObject -> RawId -> IO (Id NSProtocolChecker)
protocolCheckerWithTarget_protocol anObject aProtocol =
  do
    cls' <- getRequiredClass "NSProtocolChecker"
    sendClassMessage cls' protocolCheckerWithTarget_protocolSelector (toNSObject anObject) aProtocol

-- | @- initWithTarget:protocol:@
initWithTarget_protocol :: (IsNSProtocolChecker nsProtocolChecker, IsNSObject anObject) => nsProtocolChecker -> anObject -> RawId -> IO (Id NSProtocolChecker)
initWithTarget_protocol nsProtocolChecker anObject aProtocol =
  sendOwnedMessage nsProtocolChecker initWithTarget_protocolSelector (toNSObject anObject) aProtocol

-- | @- protocol@
protocol :: IsNSProtocolChecker nsProtocolChecker => nsProtocolChecker -> IO RawId
protocol nsProtocolChecker =
  sendMessage nsProtocolChecker protocolSelector

-- | @- target@
target :: IsNSProtocolChecker nsProtocolChecker => nsProtocolChecker -> IO (Id NSObject)
target nsProtocolChecker =
  sendMessage nsProtocolChecker targetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @protocolCheckerWithTarget:protocol:@
protocolCheckerWithTarget_protocolSelector :: Selector '[Id NSObject, RawId] (Id NSProtocolChecker)
protocolCheckerWithTarget_protocolSelector = mkSelector "protocolCheckerWithTarget:protocol:"

-- | @Selector@ for @initWithTarget:protocol:@
initWithTarget_protocolSelector :: Selector '[Id NSObject, RawId] (Id NSProtocolChecker)
initWithTarget_protocolSelector = mkSelector "initWithTarget:protocol:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] RawId
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id NSObject)
targetSelector = mkSelector "target"

