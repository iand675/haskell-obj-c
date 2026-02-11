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
  , labelSelector
  , setLabelSelector
  , initialCapacitySelector
  , setInitialCapacitySelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | label
--
-- An optional label for the MTLResidencySet.
--
-- ObjC selector: @- label@
label :: IsMTLResidencySetDescriptor mtlResidencySetDescriptor => mtlResidencySetDescriptor -> IO (Id NSString)
label mtlResidencySetDescriptor  =
  sendMsg mtlResidencySetDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- An optional label for the MTLResidencySet.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLResidencySetDescriptor mtlResidencySetDescriptor, IsNSString value) => mtlResidencySetDescriptor -> value -> IO ()
setLabel mtlResidencySetDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlResidencySetDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | initialCapacity
--
-- If non-zero, defines the number of allocations for which to initialize the internal arrays. Defaults to zero.
--
-- ObjC selector: @- initialCapacity@
initialCapacity :: IsMTLResidencySetDescriptor mtlResidencySetDescriptor => mtlResidencySetDescriptor -> IO CULong
initialCapacity mtlResidencySetDescriptor  =
  sendMsg mtlResidencySetDescriptor (mkSelector "initialCapacity") retCULong []

-- | initialCapacity
--
-- If non-zero, defines the number of allocations for which to initialize the internal arrays. Defaults to zero.
--
-- ObjC selector: @- setInitialCapacity:@
setInitialCapacity :: IsMTLResidencySetDescriptor mtlResidencySetDescriptor => mtlResidencySetDescriptor -> CULong -> IO ()
setInitialCapacity mtlResidencySetDescriptor  value =
  sendMsg mtlResidencySetDescriptor (mkSelector "setInitialCapacity:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @initialCapacity@
initialCapacitySelector :: Selector
initialCapacitySelector = mkSelector "initialCapacity"

-- | @Selector@ for @setInitialCapacity:@
setInitialCapacitySelector :: Selector
setInitialCapacitySelector = mkSelector "setInitialCapacity:"

