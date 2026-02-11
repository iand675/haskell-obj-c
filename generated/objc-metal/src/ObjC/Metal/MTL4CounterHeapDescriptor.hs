{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , setTypeSelector
  , countSelector
  , setCountSelector

  -- * Enum types
  , MTL4CounterHeapType(MTL4CounterHeapType)
  , pattern MTL4CounterHeapTypeInvalid
  , pattern MTL4CounterHeapTypeTimestamp

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
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Assigns the type of data that the heap contains.
--
-- ObjC selector: @- type@
type_ :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> IO MTL4CounterHeapType
type_ mtL4CounterHeapDescriptor  =
  fmap (coerce :: CLong -> MTL4CounterHeapType) $ sendMsg mtL4CounterHeapDescriptor (mkSelector "type") retCLong []

-- | Assigns the type of data that the heap contains.
--
-- ObjC selector: @- setType:@
setType :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> MTL4CounterHeapType -> IO ()
setType mtL4CounterHeapDescriptor  value =
  sendMsg mtL4CounterHeapDescriptor (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | Assigns the number of entries in the heap.
--
-- Each entry represents one item in the heap. The size of the individual entries depends on the heap type.
--
-- ObjC selector: @- count@
count :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> IO CULong
count mtL4CounterHeapDescriptor  =
  sendMsg mtL4CounterHeapDescriptor (mkSelector "count") retCULong []

-- | Assigns the number of entries in the heap.
--
-- Each entry represents one item in the heap. The size of the individual entries depends on the heap type.
--
-- ObjC selector: @- setCount:@
setCount :: IsMTL4CounterHeapDescriptor mtL4CounterHeapDescriptor => mtL4CounterHeapDescriptor -> CULong -> IO ()
setCount mtL4CounterHeapDescriptor  value =
  sendMsg mtL4CounterHeapDescriptor (mkSelector "setCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector
setCountSelector = mkSelector "setCount:"

