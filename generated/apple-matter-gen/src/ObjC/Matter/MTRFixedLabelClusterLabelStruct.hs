{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRFixedLabelClusterLabelStruct@.
module ObjC.Matter.MTRFixedLabelClusterLabelStruct
  ( MTRFixedLabelClusterLabelStruct
  , IsMTRFixedLabelClusterLabelStruct(..)
  , label
  , setLabel
  , value
  , setValue
  , labelSelector
  , setLabelSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- label@
label :: IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct => mtrFixedLabelClusterLabelStruct -> IO (Id NSString)
label mtrFixedLabelClusterLabelStruct =
  sendMessage mtrFixedLabelClusterLabelStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct, IsNSString value) => mtrFixedLabelClusterLabelStruct -> value -> IO ()
setLabel mtrFixedLabelClusterLabelStruct value =
  sendMessage mtrFixedLabelClusterLabelStruct setLabelSelector (toNSString value)

-- | @- value@
value :: IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct => mtrFixedLabelClusterLabelStruct -> IO (Id NSString)
value mtrFixedLabelClusterLabelStruct =
  sendMessage mtrFixedLabelClusterLabelStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct, IsNSString value) => mtrFixedLabelClusterLabelStruct -> value -> IO ()
setValue mtrFixedLabelClusterLabelStruct value =
  sendMessage mtrFixedLabelClusterLabelStruct setValueSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

