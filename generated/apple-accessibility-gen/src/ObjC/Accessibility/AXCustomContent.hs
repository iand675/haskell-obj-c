{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXCustomContent@.
module ObjC.Accessibility.AXCustomContent
  ( AXCustomContent
  , IsAXCustomContent(..)
  , customContentWithLabel_value
  , customContentWithAttributedLabel_attributedValue
  , init_
  , new
  , label
  , attributedLabel
  , value
  , attributedValue
  , importance
  , setImportance
  , attributedLabelSelector
  , attributedValueSelector
  , customContentWithAttributedLabel_attributedValueSelector
  , customContentWithLabel_valueSelector
  , importanceSelector
  , initSelector
  , labelSelector
  , newSelector
  , setImportanceSelector
  , valueSelector

  -- * Enum types
  , AXCustomContentImportance(AXCustomContentImportance)
  , pattern AXCustomContentImportanceDefault
  , pattern AXCustomContentImportanceHigh

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Accessibility.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ customContentWithLabel:value:@
customContentWithLabel_value :: (IsNSString label, IsNSString value) => label -> value -> IO (Id AXCustomContent)
customContentWithLabel_value label value =
  do
    cls' <- getRequiredClass "AXCustomContent"
    sendClassMessage cls' customContentWithLabel_valueSelector (toNSString label) (toNSString value)

-- | @+ customContentWithAttributedLabel:attributedValue:@
customContentWithAttributedLabel_attributedValue :: (IsNSAttributedString label, IsNSAttributedString value) => label -> value -> IO (Id AXCustomContent)
customContentWithAttributedLabel_attributedValue label value =
  do
    cls' <- getRequiredClass "AXCustomContent"
    sendClassMessage cls' customContentWithAttributedLabel_attributedValueSelector (toNSAttributedString label) (toNSAttributedString value)

-- | @- init@
init_ :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id AXCustomContent)
init_ axCustomContent =
  sendOwnedMessage axCustomContent initSelector

-- | @+ new@
new :: IO (Id AXCustomContent)
new  =
  do
    cls' <- getRequiredClass "AXCustomContent"
    sendOwnedClassMessage cls' newSelector

-- | @- label@
label :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSString)
label axCustomContent =
  sendMessage axCustomContent labelSelector

-- | @- attributedLabel@
attributedLabel :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSAttributedString)
attributedLabel axCustomContent =
  sendMessage axCustomContent attributedLabelSelector

-- | @- value@
value :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSString)
value axCustomContent =
  sendMessage axCustomContent valueSelector

-- | @- attributedValue@
attributedValue :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSAttributedString)
attributedValue axCustomContent =
  sendMessage axCustomContent attributedValueSelector

-- | @- importance@
importance :: IsAXCustomContent axCustomContent => axCustomContent -> IO AXCustomContentImportance
importance axCustomContent =
  sendMessage axCustomContent importanceSelector

-- | @- setImportance:@
setImportance :: IsAXCustomContent axCustomContent => axCustomContent -> AXCustomContentImportance -> IO ()
setImportance axCustomContent value =
  sendMessage axCustomContent setImportanceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @customContentWithLabel:value:@
customContentWithLabel_valueSelector :: Selector '[Id NSString, Id NSString] (Id AXCustomContent)
customContentWithLabel_valueSelector = mkSelector "customContentWithLabel:value:"

-- | @Selector@ for @customContentWithAttributedLabel:attributedValue:@
customContentWithAttributedLabel_attributedValueSelector :: Selector '[Id NSAttributedString, Id NSAttributedString] (Id AXCustomContent)
customContentWithAttributedLabel_attributedValueSelector = mkSelector "customContentWithAttributedLabel:attributedValue:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXCustomContent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXCustomContent)
newSelector = mkSelector "new"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @attributedLabel@
attributedLabelSelector :: Selector '[] (Id NSAttributedString)
attributedLabelSelector = mkSelector "attributedLabel"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @attributedValue@
attributedValueSelector :: Selector '[] (Id NSAttributedString)
attributedValueSelector = mkSelector "attributedValue"

-- | @Selector@ for @importance@
importanceSelector :: Selector '[] AXCustomContentImportance
importanceSelector = mkSelector "importance"

-- | @Selector@ for @setImportance:@
setImportanceSelector :: Selector '[AXCustomContentImportance] ()
setImportanceSelector = mkSelector "setImportance:"

