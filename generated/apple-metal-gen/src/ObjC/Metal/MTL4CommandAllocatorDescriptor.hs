{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An optional label you can assign to the command allocator to aid debugging.
--
-- ObjC selector: @- label@
label :: IsMTL4CommandAllocatorDescriptor mtL4CommandAllocatorDescriptor => mtL4CommandAllocatorDescriptor -> IO (Id NSString)
label mtL4CommandAllocatorDescriptor =
  sendMessage mtL4CommandAllocatorDescriptor labelSelector

-- | An optional label you can assign to the command allocator to aid debugging.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4CommandAllocatorDescriptor mtL4CommandAllocatorDescriptor, IsNSString value) => mtL4CommandAllocatorDescriptor -> value -> IO ()
setLabel mtL4CommandAllocatorDescriptor value =
  sendMessage mtL4CommandAllocatorDescriptor setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

