{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXHandle@.
module ObjC.CallKit.CXHandle
  ( CXHandle
  , IsCXHandle(..)
  , initWithType_value
  , init_
  , isEqualToHandle
  , type_
  , value
  , initSelector
  , initWithType_valueSelector
  , isEqualToHandleSelector
  , typeSelector
  , valueSelector

  -- * Enum types
  , CXHandleType(CXHandleType)
  , pattern CXHandleTypeGeneric
  , pattern CXHandleTypePhoneNumber
  , pattern CXHandleTypeEmailAddress

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.CallKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithType:value:@
initWithType_value :: (IsCXHandle cxHandle, IsNSString value) => cxHandle -> CXHandleType -> value -> IO (Id CXHandle)
initWithType_value cxHandle type_ value =
  sendOwnedMessage cxHandle initWithType_valueSelector type_ (toNSString value)

-- | @- init@
init_ :: IsCXHandle cxHandle => cxHandle -> IO (Id CXHandle)
init_ cxHandle =
  sendOwnedMessage cxHandle initSelector

-- | @- isEqualToHandle:@
isEqualToHandle :: (IsCXHandle cxHandle, IsCXHandle handle) => cxHandle -> handle -> IO Bool
isEqualToHandle cxHandle handle =
  sendMessage cxHandle isEqualToHandleSelector (toCXHandle handle)

-- | @- type@
type_ :: IsCXHandle cxHandle => cxHandle -> IO CXHandleType
type_ cxHandle =
  sendMessage cxHandle typeSelector

-- | @- value@
value :: IsCXHandle cxHandle => cxHandle -> IO (Id NSString)
value cxHandle =
  sendMessage cxHandle valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:value:@
initWithType_valueSelector :: Selector '[CXHandleType, Id NSString] (Id CXHandle)
initWithType_valueSelector = mkSelector "initWithType:value:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXHandle)
initSelector = mkSelector "init"

-- | @Selector@ for @isEqualToHandle:@
isEqualToHandleSelector :: Selector '[Id CXHandle] Bool
isEqualToHandleSelector = mkSelector "isEqualToHandle:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CXHandleType
typeSelector = mkSelector "type"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

