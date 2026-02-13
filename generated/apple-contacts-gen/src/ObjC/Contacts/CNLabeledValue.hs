{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact property that has a value and label.
--
-- Generated bindings for @CNLabeledValue@.
module ObjC.Contacts.CNLabeledValue
  ( CNLabeledValue
  , IsCNLabeledValue(..)
  , labeledValueBySettingLabel
  , localizedStringForLabel
  , identifier
  , label
  , identifierSelector
  , labelSelector
  , labeledValueBySettingLabelSelector
  , localizedStringForLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a new CNLabeledValue with the existing value and identifier.
--
-- ObjC selector: @- labeledValueBySettingLabel:@
labeledValueBySettingLabel :: (IsCNLabeledValue cnLabeledValue, IsNSString label) => cnLabeledValue -> label -> IO (Id CNLabeledValue)
labeledValueBySettingLabel cnLabeledValue label =
  sendMessage cnLabeledValue labeledValueBySettingLabelSelector (toNSString label)

-- | Get a localized label.
--
-- Some labels are special keys representing generic labels. Use this to obtain a localized string for a label to display to a user.
--
-- @label@ — to localize.
--
-- Returns: The localized string if a Contacts framework defined label, otherwise just returns the label.
--
-- ObjC selector: @+ localizedStringForLabel:@
localizedStringForLabel :: IsNSString label => label -> IO (Id NSString)
localizedStringForLabel label =
  do
    cls' <- getRequiredClass "CNLabeledValue"
    sendClassMessage cls' localizedStringForLabelSelector (toNSString label)

-- | The identifier is unique among contacts on the device. It can be saved and used for finding labeled values next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNLabeledValue cnLabeledValue => cnLabeledValue -> IO (Id NSString)
identifier cnLabeledValue =
  sendMessage cnLabeledValue identifierSelector

-- | @- label@
label :: IsCNLabeledValue cnLabeledValue => cnLabeledValue -> IO (Id NSString)
label cnLabeledValue =
  sendMessage cnLabeledValue labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @labeledValueBySettingLabel:@
labeledValueBySettingLabelSelector :: Selector '[Id NSString] (Id CNLabeledValue)
labeledValueBySettingLabelSelector = mkSelector "labeledValueBySettingLabel:"

-- | @Selector@ for @localizedStringForLabel:@
localizedStringForLabelSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForLabelSelector = mkSelector "localizedStringForLabel:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

