{-# LANGUAGE PatternSynonyms #-}
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
  , initWithType_valueSelector
  , initSelector
  , isEqualToHandleSelector
  , typeSelector
  , valueSelector

  -- * Enum types
  , CXHandleType(CXHandleType)
  , pattern CXHandleTypeGeneric
  , pattern CXHandleTypePhoneNumber
  , pattern CXHandleTypeEmailAddress

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

import ObjC.CallKit.Internal.Classes
import ObjC.CallKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithType:value:@
initWithType_value :: (IsCXHandle cxHandle, IsNSString value) => cxHandle -> CXHandleType -> value -> IO (Id CXHandle)
initWithType_value cxHandle  type_ value =
withObjCPtr value $ \raw_value ->
    sendMsg cxHandle (mkSelector "initWithType:value:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCXHandle cxHandle => cxHandle -> IO (Id CXHandle)
init_ cxHandle  =
  sendMsg cxHandle (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- isEqualToHandle:@
isEqualToHandle :: (IsCXHandle cxHandle, IsCXHandle handle) => cxHandle -> handle -> IO Bool
isEqualToHandle cxHandle  handle =
withObjCPtr handle $ \raw_handle ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxHandle (mkSelector "isEqualToHandle:") retCULong [argPtr (castPtr raw_handle :: Ptr ())]

-- | @- type@
type_ :: IsCXHandle cxHandle => cxHandle -> IO CXHandleType
type_ cxHandle  =
  fmap (coerce :: CLong -> CXHandleType) $ sendMsg cxHandle (mkSelector "type") retCLong []

-- | @- value@
value :: IsCXHandle cxHandle => cxHandle -> IO (Id NSString)
value cxHandle  =
  sendMsg cxHandle (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:value:@
initWithType_valueSelector :: Selector
initWithType_valueSelector = mkSelector "initWithType:value:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @isEqualToHandle:@
isEqualToHandleSelector :: Selector
isEqualToHandleSelector = mkSelector "isEqualToHandle:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

