{-# LANGUAGE DataKinds #-}
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
  , initWithInvocationSelector
  , initWithTarget_selector_objectSelector
  , invocationSelector
  , resultSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithTarget:selector:object:@
initWithTarget_selector_object :: IsNSInvocationOperation nsInvocationOperation => nsInvocationOperation -> RawId -> Sel -> RawId -> IO (Id NSInvocationOperation)
initWithTarget_selector_object nsInvocationOperation target sel arg =
  sendOwnedMessage nsInvocationOperation initWithTarget_selector_objectSelector target sel arg

-- | @- initWithInvocation:@
initWithInvocation :: (IsNSInvocationOperation nsInvocationOperation, IsNSInvocation inv) => nsInvocationOperation -> inv -> IO (Id NSInvocationOperation)
initWithInvocation nsInvocationOperation inv =
  sendOwnedMessage nsInvocationOperation initWithInvocationSelector (toNSInvocation inv)

-- | @- invocation@
invocation :: IsNSInvocationOperation nsInvocationOperation => nsInvocationOperation -> IO (Id NSInvocation)
invocation nsInvocationOperation =
  sendMessage nsInvocationOperation invocationSelector

-- | @- result@
result :: IsNSInvocationOperation nsInvocationOperation => nsInvocationOperation -> IO RawId
result nsInvocationOperation =
  sendMessage nsInvocationOperation resultSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTarget:selector:object:@
initWithTarget_selector_objectSelector :: Selector '[RawId, Sel, RawId] (Id NSInvocationOperation)
initWithTarget_selector_objectSelector = mkSelector "initWithTarget:selector:object:"

-- | @Selector@ for @initWithInvocation:@
initWithInvocationSelector :: Selector '[Id NSInvocation] (Id NSInvocationOperation)
initWithInvocationSelector = mkSelector "initWithInvocation:"

-- | @Selector@ for @invocation@
invocationSelector :: Selector '[] (Id NSInvocation)
invocationSelector = mkSelector "invocation"

-- | @Selector@ for @result@
resultSelector :: Selector '[] RawId
resultSelector = mkSelector "result"

