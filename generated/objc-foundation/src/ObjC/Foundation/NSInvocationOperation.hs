{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInvocationOperation@.
module ObjC.Foundation.NSInvocationOperation
  ( NSInvocationOperation
  , IsNSInvocationOperation(..)
  , initWithTarget_selector_object
  , initWithInvocation
  , invocation
  , result
  , initWithTarget_selector_objectSelector
  , initWithInvocationSelector
  , invocationSelector
  , resultSelector


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

-- | @- initWithTarget:selector:object:@
initWithTarget_selector_object :: IsNSInvocationOperation nsInvocationOperation => nsInvocationOperation -> RawId -> Selector -> RawId -> IO (Id NSInvocationOperation)
initWithTarget_selector_object nsInvocationOperation  target sel arg =
  sendMsg nsInvocationOperation (mkSelector "initWithTarget:selector:object:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector sel), argPtr (castPtr (unRawId arg) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithInvocation:@
initWithInvocation :: (IsNSInvocationOperation nsInvocationOperation, IsNSInvocation inv) => nsInvocationOperation -> inv -> IO (Id NSInvocationOperation)
initWithInvocation nsInvocationOperation  inv =
withObjCPtr inv $ \raw_inv ->
    sendMsg nsInvocationOperation (mkSelector "initWithInvocation:") (retPtr retVoid) [argPtr (castPtr raw_inv :: Ptr ())] >>= ownedObject . castPtr

-- | @- invocation@
invocation :: IsNSInvocationOperation nsInvocationOperation => nsInvocationOperation -> IO (Id NSInvocation)
invocation nsInvocationOperation  =
  sendMsg nsInvocationOperation (mkSelector "invocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- result@
result :: IsNSInvocationOperation nsInvocationOperation => nsInvocationOperation -> IO RawId
result nsInvocationOperation  =
  fmap (RawId . castPtr) $ sendMsg nsInvocationOperation (mkSelector "result") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTarget:selector:object:@
initWithTarget_selector_objectSelector :: Selector
initWithTarget_selector_objectSelector = mkSelector "initWithTarget:selector:object:"

-- | @Selector@ for @initWithInvocation:@
initWithInvocationSelector :: Selector
initWithInvocationSelector = mkSelector "initWithInvocation:"

-- | @Selector@ for @invocation@
invocationSelector :: Selector
invocationSelector = mkSelector "invocation"

-- | @Selector@ for @result@
resultSelector :: Selector
resultSelector = mkSelector "result"

