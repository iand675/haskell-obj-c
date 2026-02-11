{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProtocolChecker@.
module ObjC.Foundation.NSProtocolChecker
  ( NSProtocolChecker
  , IsNSProtocolChecker(..)
  , protocolCheckerWithTarget_protocol
  , initWithTarget_protocol
  , target
  , protocolCheckerWithTarget_protocolSelector
  , initWithTarget_protocolSelector
  , targetSelector


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

-- | @+ protocolCheckerWithTarget:protocol:@
protocolCheckerWithTarget_protocol :: IsNSObject anObject => anObject -> RawId -> IO (Id NSProtocolChecker)
protocolCheckerWithTarget_protocol anObject aProtocol =
  do
    cls' <- getRequiredClass "NSProtocolChecker"
    withObjCPtr anObject $ \raw_anObject ->
      sendClassMsg cls' (mkSelector "protocolCheckerWithTarget:protocol:") (retPtr retVoid) [argPtr (castPtr raw_anObject :: Ptr ()), argPtr (castPtr (unRawId aProtocol) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithTarget:protocol:@
initWithTarget_protocol :: (IsNSProtocolChecker nsProtocolChecker, IsNSObject anObject) => nsProtocolChecker -> anObject -> RawId -> IO (Id NSProtocolChecker)
initWithTarget_protocol nsProtocolChecker  anObject aProtocol =
withObjCPtr anObject $ \raw_anObject ->
    sendMsg nsProtocolChecker (mkSelector "initWithTarget:protocol:") (retPtr retVoid) [argPtr (castPtr raw_anObject :: Ptr ()), argPtr (castPtr (unRawId aProtocol) :: Ptr ())] >>= ownedObject . castPtr

-- | @- target@
target :: IsNSProtocolChecker nsProtocolChecker => nsProtocolChecker -> IO (Id NSObject)
target nsProtocolChecker  =
  sendMsg nsProtocolChecker (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @protocolCheckerWithTarget:protocol:@
protocolCheckerWithTarget_protocolSelector :: Selector
protocolCheckerWithTarget_protocolSelector = mkSelector "protocolCheckerWithTarget:protocol:"

-- | @Selector@ for @initWithTarget:protocol:@
initWithTarget_protocolSelector :: Selector
initWithTarget_protocolSelector = mkSelector "initWithTarget:protocol:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

