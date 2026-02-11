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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLabel:value:@
initWithLabel_value :: (IsPKLabeledValue pkLabeledValue, IsNSString label, IsNSString value) => pkLabeledValue -> label -> value -> IO (Id PKLabeledValue)
initWithLabel_value pkLabeledValue  label value =
withObjCPtr label $ \raw_label ->
  withObjCPtr value $ \raw_value ->
      sendMsg pkLabeledValue (mkSelector "initWithLabel:value:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | @- label@
label :: IsPKLabeledValue pkLabeledValue => pkLabeledValue -> IO (Id NSString)
label pkLabeledValue  =
  sendMsg pkLabeledValue (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsPKLabeledValue pkLabeledValue => pkLabeledValue -> IO (Id NSString)
value pkLabeledValue  =
  sendMsg pkLabeledValue (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLabel:value:@
initWithLabel_valueSelector :: Selector
initWithLabel_valueSelector = mkSelector "initWithLabel:value:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

