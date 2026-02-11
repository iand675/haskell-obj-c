{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together parameters for creating a command allocator.
--
-- Generated bindings for @MTL4CommandAllocatorDescriptor@.
module ObjC.Metal.MTL4CommandAllocatorDescriptor
  ( MTL4CommandAllocatorDescriptor
  , IsMTL4CommandAllocatorDescriptor(..)
  , label
  , setLabel
  , labelSelector
  , setLabelSelector


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

-- | An optional label you can assign to the command allocator to aid debugging.
--
-- ObjC selector: @- label@
label :: IsMTL4CommandAllocatorDescriptor mtL4CommandAllocatorDescriptor => mtL4CommandAllocatorDescriptor -> IO (Id NSString)
label mtL4CommandAllocatorDescriptor  =
  sendMsg mtL4CommandAllocatorDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional label you can assign to the command allocator to aid debugging.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4CommandAllocatorDescriptor mtL4CommandAllocatorDescriptor, IsNSString value) => mtL4CommandAllocatorDescriptor -> value -> IO ()
setLabel mtL4CommandAllocatorDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4CommandAllocatorDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

