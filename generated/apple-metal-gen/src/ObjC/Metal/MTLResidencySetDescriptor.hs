{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLResidencySetDescriptor
--
-- Specifies the parameters for MTLResidencySet creation.
--
-- Generated bindings for @MTLResidencySetDescriptor@.
module ObjC.Metal.MTLResidencySetDescriptor
  ( MTLResidencySetDescriptor
  , IsMTLResidencySetDescriptor(..)
  , label
  , setLabel
  , initialCapacity
  , setInitialCapacity
  , initialCapacitySelector
  , labelSelector
  , setInitialCapacitySelector
  , setLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | label
--
-- An optional label for the MTLResidencySet.
--
-- ObjC selector: @- label@
label :: IsMTLResidencySetDescriptor mtlResidencySetDescriptor => mtlResidencySetDescriptor -> IO (Id NSString)
label mtlResidencySetDescriptor =
  sendMessage mtlResidencySetDescriptor labelSelector

-- | label
--
-- An optional label for the MTLResidencySet.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLResidencySetDescriptor mtlResidencySetDescriptor, IsNSString value) => mtlResidencySetDescriptor -> value -> IO ()
setLabel mtlResidencySetDescriptor value =
  sendMessage mtlResidencySetDescriptor setLabelSelector (toNSString value)

-- | initialCapacity
--
-- If non-zero, defines the number of allocations for which to initialize the internal arrays. Defaults to zero.
--
-- ObjC selector: @- initialCapacity@
initialCapacity :: IsMTLResidencySetDescriptor mtlResidencySetDescriptor => mtlResidencySetDescriptor -> IO CULong
initialCapacity mtlResidencySetDescriptor =
  sendOwnedMessage mtlResidencySetDescriptor initialCapacitySelector

-- | initialCapacity
--
-- If non-zero, defines the number of allocations for which to initialize the internal arrays. Defaults to zero.
--
-- ObjC selector: @- setInitialCapacity:@
setInitialCapacity :: IsMTLResidencySetDescriptor mtlResidencySetDescriptor => mtlResidencySetDescriptor -> CULong -> IO ()
setInitialCapacity mtlResidencySetDescriptor value =
  sendMessage mtlResidencySetDescriptor setInitialCapacitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @initialCapacity@
initialCapacitySelector :: Selector '[] CULong
initialCapacitySelector = mkSelector "initialCapacity"

-- | @Selector@ for @setInitialCapacity:@
setInitialCapacitySelector :: Selector '[CULong] ()
setInitialCapacitySelector = mkSelector "setInitialCapacity:"

