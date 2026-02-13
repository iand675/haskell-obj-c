{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUserLabelClusterLabelStruct@.
module ObjC.Matter.MTRUserLabelClusterLabelStruct
  ( MTRUserLabelClusterLabelStruct
  , IsMTRUserLabelClusterLabelStruct(..)
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
label :: IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct => mtrUserLabelClusterLabelStruct -> IO (Id NSString)
label mtrUserLabelClusterLabelStruct =
  sendMessage mtrUserLabelClusterLabelStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct, IsNSString value) => mtrUserLabelClusterLabelStruct -> value -> IO ()
setLabel mtrUserLabelClusterLabelStruct value =
  sendMessage mtrUserLabelClusterLabelStruct setLabelSelector (toNSString value)

-- | @- value@
value :: IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct => mtrUserLabelClusterLabelStruct -> IO (Id NSString)
value mtrUserLabelClusterLabelStruct =
  sendMessage mtrUserLabelClusterLabelStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct, IsNSString value) => mtrUserLabelClusterLabelStruct -> value -> IO ()
setValue mtrUserLabelClusterLabelStruct value =
  sendMessage mtrUserLabelClusterLabelStruct setValueSelector (toNSString value)

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

