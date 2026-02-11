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
  , keySelector
  , valueSelector
  , identifierSelector
  , labelSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contact@
contact :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id CNContact)
contact cnContactProperty  =
  sendMsg cnContactProperty (mkSelector "contact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key of the contact property, as defined in CNContact.h.
--
-- ObjC selector: @- key@
key :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id NSString)
key cnContactProperty  =
  sendMsg cnContactProperty (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value of the property.
--
-- ObjC selector: @- value@
value :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO RawId
value cnContactProperty  =
  fmap (RawId . castPtr) $ sendMsg cnContactProperty (mkSelector "value") (retPtr retVoid) []

-- | The identifier of the labeled value if the property is an array of labeled values, otherwise is nil.
--
-- ObjC selector: @- identifier@
identifier :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id NSString)
identifier cnContactProperty  =
  sendMsg cnContactProperty (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The label of the labeled value if the property is an array of labeled values, otherwise is nil.
--
-- ObjC selector: @- label@
label :: IsCNContactProperty cnContactProperty => cnContactProperty -> IO (Id NSString)
label cnContactProperty  =
  sendMsg cnContactProperty (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contact@
contactSelector :: Selector
contactSelector = mkSelector "contact"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

