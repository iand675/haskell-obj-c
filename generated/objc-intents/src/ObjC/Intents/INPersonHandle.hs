{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPersonHandle@.
module ObjC.Intents.INPersonHandle
  ( INPersonHandle
  , IsINPersonHandle(..)
  , init_
  , initWithValue_type_label
  , initWithValue_type
  , value
  , type_
  , label
  , initSelector
  , initWithValue_type_labelSelector
  , initWithValue_typeSelector
  , valueSelector
  , typeSelector
  , labelSelector

  -- * Enum types
  , INPersonHandleType(INPersonHandleType)
  , pattern INPersonHandleTypeUnknown
  , pattern INPersonHandleTypeEmailAddress
  , pattern INPersonHandleTypePhoneNumber

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO (Id INPersonHandle)
init_ inPersonHandle  =
  sendMsg inPersonHandle (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithValue:type:label:@
initWithValue_type_label :: (IsINPersonHandle inPersonHandle, IsNSString value, IsNSString label) => inPersonHandle -> value -> INPersonHandleType -> label -> IO (Id INPersonHandle)
initWithValue_type_label inPersonHandle  value type_ label =
withObjCPtr value $ \raw_value ->
  withObjCPtr label $ \raw_label ->
      sendMsg inPersonHandle (mkSelector "initWithValue:type:label:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_label :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValue:type:@
initWithValue_type :: (IsINPersonHandle inPersonHandle, IsNSString value) => inPersonHandle -> value -> INPersonHandleType -> IO (Id INPersonHandle)
initWithValue_type inPersonHandle  value type_ =
withObjCPtr value $ \raw_value ->
    sendMsg inPersonHandle (mkSelector "initWithValue:type:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ()), argCLong (coerce type_)] >>= ownedObject . castPtr

-- | @- value@
value :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO (Id NSString)
value inPersonHandle  =
  sendMsg inPersonHandle (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO INPersonHandleType
type_ inPersonHandle  =
  fmap (coerce :: CLong -> INPersonHandleType) $ sendMsg inPersonHandle (mkSelector "type") retCLong []

-- | @- label@
label :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO (Id NSString)
label inPersonHandle  =
  sendMsg inPersonHandle (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithValue:type:label:@
initWithValue_type_labelSelector :: Selector
initWithValue_type_labelSelector = mkSelector "initWithValue:type:label:"

-- | @Selector@ for @initWithValue:type:@
initWithValue_typeSelector :: Selector
initWithValue_typeSelector = mkSelector "initWithValue:type:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

