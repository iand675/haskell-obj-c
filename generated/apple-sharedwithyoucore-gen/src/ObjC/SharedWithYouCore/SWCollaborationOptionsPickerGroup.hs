{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The identifier of the selected option in the group. Defaults to the first SWCollaborationOption's identifier.
--
-- ObjC selector: @- selectedOptionIdentifier@
selectedOptionIdentifier :: IsSWCollaborationOptionsPickerGroup swCollaborationOptionsPickerGroup => swCollaborationOptionsPickerGroup -> IO (Id NSString)
selectedOptionIdentifier swCollaborationOptionsPickerGroup =
  sendMessage swCollaborationOptionsPickerGroup selectedOptionIdentifierSelector

-- | The identifier of the selected option in the group. Defaults to the first SWCollaborationOption's identifier.
--
-- ObjC selector: @- setSelectedOptionIdentifier:@
setSelectedOptionIdentifier :: (IsSWCollaborationOptionsPickerGroup swCollaborationOptionsPickerGroup, IsNSString value) => swCollaborationOptionsPickerGroup -> value -> IO ()
setSelectedOptionIdentifier swCollaborationOptionsPickerGroup value =
  sendMessage swCollaborationOptionsPickerGroup setSelectedOptionIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectedOptionIdentifier@
selectedOptionIdentifierSelector :: Selector '[] (Id NSString)
selectedOptionIdentifierSelector = mkSelector "selectedOptionIdentifier"

-- | @Selector@ for @setSelectedOptionIdentifier:@
setSelectedOptionIdentifierSelector :: Selector '[Id NSString] ()
setSelectedOptionIdentifierSelector = mkSelector "setSelectedOptionIdentifier:"

