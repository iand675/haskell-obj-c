{-# LANGUAGE PatternSynonyms #-}
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
  , customContentWithLabel_valueSelector
  , customContentWithAttributedLabel_attributedValueSelector
  , initSelector
  , newSelector
  , labelSelector
  , attributedLabelSelector
  , valueSelector
  , attributedValueSelector
  , importanceSelector
  , setImportanceSelector

  -- * Enum types
  , AXCustomContentImportance(AXCustomContentImportance)
  , pattern AXCustomContentImportanceDefault
  , pattern AXCustomContentImportanceHigh

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

import ObjC.Accessibility.Internal.Classes
import ObjC.Accessibility.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ customContentWithLabel:value:@
customContentWithLabel_value :: (IsNSString label, IsNSString value) => label -> value -> IO (Id AXCustomContent)
customContentWithLabel_value label value =
  do
    cls' <- getRequiredClass "AXCustomContent"
    withObjCPtr label $ \raw_label ->
      withObjCPtr value $ \raw_value ->
        sendClassMsg cls' (mkSelector "customContentWithLabel:value:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= retainedObject . castPtr

-- | @+ customContentWithAttributedLabel:attributedValue:@
customContentWithAttributedLabel_attributedValue :: (IsNSAttributedString label, IsNSAttributedString value) => label -> value -> IO (Id AXCustomContent)
customContentWithAttributedLabel_attributedValue label value =
  do
    cls' <- getRequiredClass "AXCustomContent"
    withObjCPtr label $ \raw_label ->
      withObjCPtr value $ \raw_value ->
        sendClassMsg cls' (mkSelector "customContentWithAttributedLabel:attributedValue:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id AXCustomContent)
init_ axCustomContent  =
  sendMsg axCustomContent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXCustomContent)
new  =
  do
    cls' <- getRequiredClass "AXCustomContent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- label@
label :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSString)
label axCustomContent  =
  sendMsg axCustomContent (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributedLabel@
attributedLabel :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSAttributedString)
attributedLabel axCustomContent  =
  sendMsg axCustomContent (mkSelector "attributedLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSString)
value axCustomContent  =
  sendMsg axCustomContent (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributedValue@
attributedValue :: IsAXCustomContent axCustomContent => axCustomContent -> IO (Id NSAttributedString)
attributedValue axCustomContent  =
  sendMsg axCustomContent (mkSelector "attributedValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- importance@
importance :: IsAXCustomContent axCustomContent => axCustomContent -> IO AXCustomContentImportance
importance axCustomContent  =
  fmap (coerce :: CULong -> AXCustomContentImportance) $ sendMsg axCustomContent (mkSelector "importance") retCULong []

-- | @- setImportance:@
setImportance :: IsAXCustomContent axCustomContent => axCustomContent -> AXCustomContentImportance -> IO ()
setImportance axCustomContent  value =
  sendMsg axCustomContent (mkSelector "setImportance:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @customContentWithLabel:value:@
customContentWithLabel_valueSelector :: Selector
customContentWithLabel_valueSelector = mkSelector "customContentWithLabel:value:"

-- | @Selector@ for @customContentWithAttributedLabel:attributedValue:@
customContentWithAttributedLabel_attributedValueSelector :: Selector
customContentWithAttributedLabel_attributedValueSelector = mkSelector "customContentWithAttributedLabel:attributedValue:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @attributedLabel@
attributedLabelSelector :: Selector
attributedLabelSelector = mkSelector "attributedLabel"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @attributedValue@
attributedValueSelector :: Selector
attributedValueSelector = mkSelector "attributedValue"

-- | @Selector@ for @importance@
importanceSelector :: Selector
importanceSelector = mkSelector "importance"

-- | @Selector@ for @setImportance:@
setImportanceSelector :: Selector
setImportanceSelector = mkSelector "setImportance:"

