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
  , labeledValueBySettingLabelSelector
  , localizedStringForLabelSelector
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

-- | Returns a new CNLabeledValue with the existing value and identifier.
--
-- ObjC selector: @- labeledValueBySettingLabel:@
labeledValueBySettingLabel :: (IsCNLabeledValue cnLabeledValue, IsNSString label) => cnLabeledValue -> label -> IO (Id CNLabeledValue)
labeledValueBySettingLabel cnLabeledValue  label =
withObjCPtr label $ \raw_label ->
    sendMsg cnLabeledValue (mkSelector "labeledValueBySettingLabel:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr label $ \raw_label ->
      sendClassMsg cls' (mkSelector "localizedStringForLabel:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ())] >>= retainedObject . castPtr

-- | The identifier is unique among contacts on the device. It can be saved and used for finding labeled values next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNLabeledValue cnLabeledValue => cnLabeledValue -> IO (Id NSString)
identifier cnLabeledValue  =
  sendMsg cnLabeledValue (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- label@
label :: IsCNLabeledValue cnLabeledValue => cnLabeledValue -> IO (Id NSString)
label cnLabeledValue  =
  sendMsg cnLabeledValue (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @labeledValueBySettingLabel:@
labeledValueBySettingLabelSelector :: Selector
labeledValueBySettingLabelSelector = mkSelector "labeledValueBySettingLabel:"

-- | @Selector@ for @localizedStringForLabel:@
localizedStringForLabelSelector :: Selector
localizedStringForLabelSelector = mkSelector "localizedStringForLabel:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

