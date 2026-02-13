{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initWithValue_typeSelector
  , initWithValue_type_labelSelector
  , labelSelector
  , typeSelector
  , valueSelector

  -- * Enum types
  , INPersonHandleType(INPersonHandleType)
  , pattern INPersonHandleTypeUnknown
  , pattern INPersonHandleTypeEmailAddress
  , pattern INPersonHandleTypePhoneNumber

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO (Id INPersonHandle)
init_ inPersonHandle =
  sendOwnedMessage inPersonHandle initSelector

-- | @- initWithValue:type:label:@
initWithValue_type_label :: (IsINPersonHandle inPersonHandle, IsNSString value, IsNSString label) => inPersonHandle -> value -> INPersonHandleType -> label -> IO (Id INPersonHandle)
initWithValue_type_label inPersonHandle value type_ label =
  sendOwnedMessage inPersonHandle initWithValue_type_labelSelector (toNSString value) type_ (toNSString label)

-- | @- initWithValue:type:@
initWithValue_type :: (IsINPersonHandle inPersonHandle, IsNSString value) => inPersonHandle -> value -> INPersonHandleType -> IO (Id INPersonHandle)
initWithValue_type inPersonHandle value type_ =
  sendOwnedMessage inPersonHandle initWithValue_typeSelector (toNSString value) type_

-- | @- value@
value :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO (Id NSString)
value inPersonHandle =
  sendMessage inPersonHandle valueSelector

-- | @- type@
type_ :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO INPersonHandleType
type_ inPersonHandle =
  sendMessage inPersonHandle typeSelector

-- | @- label@
label :: IsINPersonHandle inPersonHandle => inPersonHandle -> IO (Id NSString)
label inPersonHandle =
  sendMessage inPersonHandle labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPersonHandle)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithValue:type:label:@
initWithValue_type_labelSelector :: Selector '[Id NSString, INPersonHandleType, Id NSString] (Id INPersonHandle)
initWithValue_type_labelSelector = mkSelector "initWithValue:type:label:"

-- | @Selector@ for @initWithValue:type:@
initWithValue_typeSelector :: Selector '[Id NSString, INPersonHandleType] (Id INPersonHandle)
initWithValue_typeSelector = mkSelector "initWithValue:type:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @type@
typeSelector :: Selector '[] INPersonHandleType
typeSelector = mkSelector "type"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

