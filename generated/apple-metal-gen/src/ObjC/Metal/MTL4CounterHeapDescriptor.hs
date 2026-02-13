{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together parameters for configuring a counter heap object at creation time.
--
-- Generated bindings for @MTL4CounterHeapDescriptor@.
module ObjC.Metal.MTL4CounterHeapDescriptor
  ( MTL4CounterHeapDescriptor
  , IsMTL4CounterHeapDescriptor(..)
  , type_
  , setType
  , count
  , setCount
  , countSelector
  , setCountSelector
  , setTypeSelector
  , typeSelector

  -- * Enum types
  , MTL4CounterHeapType(MTL4CounterHeapType)
  , pattern MTL4CounterHeapTypeInvalid
  , pattern MTL4CounterHeapTypeTimestamp

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Assigns the type of data that the heap contains.
--
-- ObjC selector: @- type@
type_ :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> IO MTL4CounterHeapType
type_ mtL4CounterHeapDescriptor =
  sendMessage mtL4CounterHeapDescriptor typeSelector

-- | Assigns the type of data that the heap contains.
--
-- ObjC selector: @- setType:@
setType :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> MTL4CounterHeapType -> IO ()
setType mtL4CounterHeapDescriptor value =
  sendMessage mtL4CounterHeapDescriptor setTypeSelector value

-- | Assigns the number of entries in the heap.
--
-- Each entry represents one item in the heap. The size of the individual entries depends on the heap type.
--
-- ObjC selector: @- count@
count :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> IO CULong
count mtL4CounterHeapDescriptor =
  sendMessage mtL4CounterHeapDescriptor countSelector

-- | Assigns the number of entries in the heap.
--
-- Each entry represents one item in the heap. The size of the individual entries depends on the heap type.
--
-- ObjC selector: @- setCount:@
setCount :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> CULong -> IO ()
setCount mtL4CounterHeapDescriptor value =
  sendMessage mtL4CounterHeapDescriptor setCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] MTL4CounterHeapType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[MTL4CounterHeapType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector '[CULong] ()
setCountSelector = mkSelector "setCount:"

