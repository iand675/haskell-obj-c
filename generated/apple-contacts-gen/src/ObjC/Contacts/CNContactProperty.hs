{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains related information for a specific contact property.
--
-- CNContactProperty is used by the CNContactPicker to return the user's selected property.
--
-- Generated bindings for @CNContactProperty@.
module ObjC.Contacts.CNContactProperty
  ( CNContactProperty
  , IsCNContactProperty(..)
  , contact
  , key
  , value
  , identifier
  , label
  , contactSelector
  , identifierSelector
  , keySelector
  , labelSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contact@
contact :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id CNContact)
contact cnContactProperty =
  sendMessage cnContactProperty contactSelector

-- | The key of the contact property, as defined in CNContact.h.
--
-- ObjC selector: @- key@
key :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id NSString)
key cnContactProperty =
  sendMessage cnContactProperty keySelector

-- | The value of the property.
--
-- ObjC selector: @- value@
value :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO RawId
value cnContactProperty =
  sendMessage cnContactProperty valueSelector

-- | The identifier of the labeled value if the property is an array of labeled values, otherwise is nil.
--
-- ObjC selector: @- identifier@
identifier :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id NSString)
identifier cnContactProperty =
  sendMessage cnContactProperty identifierSelector

-- | The label of the labeled value if the property is an array of labeled values, otherwise is nil.
--
-- ObjC selector: @- label@
label :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id NSString)
label cnContactProperty =
  sendMessage cnContactProperty labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contact@
contactSelector :: Selector '[] (Id CNContact)
contactSelector = mkSelector "contact"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSString)
keySelector = mkSelector "key"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

