{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWCollaborationOptionsPickerGroup
--
-- Represents a group of SWCollaborationOptions that should be grouped together in a picker list, with mutually exclusive options.
--
-- SWCollaborationOptionsPickerGroup is displayed as a picker view. Only one option in the group can be selected by the user.
--
-- Generated bindings for @SWCollaborationOptionsPickerGroup@.
module ObjC.SharedWithYouCore.SWCollaborationOptionsPickerGroup
  ( SWCollaborationOptionsPickerGroup
  , IsSWCollaborationOptionsPickerGroup(..)
  , selectedOptionIdentifier
  , setSelectedOptionIdentifier
  , selectedOptionIdentifierSelector
  , setSelectedOptionIdentifierSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The identifier of the selected option in the group. Defaults to the first SWCollaborationOption's identifier.
--
-- ObjC selector: @- selectedOptionIdentifier@
selectedOptionIdentifier :: IsSWCollaborationOptionsPickerGroup swCollaborationOptionsPickerGroup => swCollaborationOptionsPickerGroup -> IO (Id NSString)
selectedOptionIdentifier swCollaborationOptionsPickerGroup  =
  sendMsg swCollaborationOptionsPickerGroup (mkSelector "selectedOptionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier of the selected option in the group. Defaults to the first SWCollaborationOption's identifier.
--
-- ObjC selector: @- setSelectedOptionIdentifier:@
setSelectedOptionIdentifier :: (IsSWCollaborationOptionsPickerGroup swCollaborationOptionsPickerGroup, IsNSString value) => swCollaborationOptionsPickerGroup -> value -> IO ()
setSelectedOptionIdentifier swCollaborationOptionsPickerGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOptionsPickerGroup (mkSelector "setSelectedOptionIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectedOptionIdentifier@
selectedOptionIdentifierSelector :: Selector
selectedOptionIdentifierSelector = mkSelector "selectedOptionIdentifier"

-- | @Selector@ for @setSelectedOptionIdentifier:@
setSelectedOptionIdentifierSelector :: Selector
setSelectedOptionIdentifierSelector = mkSelector "setSelectedOptionIdentifier:"

