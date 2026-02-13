{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKLabeledValue@.
module ObjC.PassKit.PKLabeledValue
  ( PKLabeledValue
  , IsPKLabeledValue(..)
  , initWithLabel_value
  , label
  , value
  , initWithLabel_valueSelector
  , labelSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLabel:value:@
initWithLabel_value :: (IsPKLabeledValue pkLabeledValue, IsNSString label, IsNSString value) => pkLabeledValue -> label -> value -> IO (Id PKLabeledValue)
initWithLabel_value pkLabeledValue label value =
  sendOwnedMessage pkLabeledValue initWithLabel_valueSelector (toNSString label) (toNSString value)

-- | @- label@
label :: IsPKLabeledValue pkLabeledValue => pkLabeledValue -> IO (Id NSString)
label pkLabeledValue =
  sendMessage pkLabeledValue labelSelector

-- | @- value@
value :: IsPKLabeledValue pkLabeledValue => pkLabeledValue -> IO (Id NSString)
value pkLabeledValue =
  sendMessage pkLabeledValue valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLabel:value:@
initWithLabel_valueSelector :: Selector '[Id NSString, Id NSString] (Id PKLabeledValue)
initWithLabel_valueSelector = mkSelector "initWithLabel:value:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

