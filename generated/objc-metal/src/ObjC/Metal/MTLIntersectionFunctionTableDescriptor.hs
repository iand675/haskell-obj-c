{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLIntersectionFunctionTableDescriptor@.
module ObjC.Metal.MTLIntersectionFunctionTableDescriptor
  ( MTLIntersectionFunctionTableDescriptor
  , IsMTLIntersectionFunctionTableDescriptor(..)
  , intersectionFunctionTableDescriptor
  , functionCount
  , setFunctionCount
  , intersectionFunctionTableDescriptorSelector
  , functionCountSelector
  , setFunctionCountSelector


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

-- | intersectionFunctionTableDescriptor
--
-- Create an autoreleased intersection function table descriptor
--
-- ObjC selector: @+ intersectionFunctionTableDescriptor@
intersectionFunctionTableDescriptor :: IO (Id MTLIntersectionFunctionTableDescriptor)
intersectionFunctionTableDescriptor  =
  do
    cls' <- getRequiredClass "MTLIntersectionFunctionTableDescriptor"
    sendClassMsg cls' (mkSelector "intersectionFunctionTableDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- functionCount@
functionCount :: IsMTLIntersectionFunctionTableDescriptor mtlIntersectionFunctionTableDescriptor => mtlIntersectionFunctionTableDescriptor -> IO CULong
functionCount mtlIntersectionFunctionTableDescriptor  =
  sendMsg mtlIntersectionFunctionTableDescriptor (mkSelector "functionCount") retCULong []

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- setFunctionCount:@
setFunctionCount :: IsMTLIntersectionFunctionTableDescriptor mtlIntersectionFunctionTableDescriptor => mtlIntersectionFunctionTableDescriptor -> CULong -> IO ()
setFunctionCount mtlIntersectionFunctionTableDescriptor  value =
  sendMsg mtlIntersectionFunctionTableDescriptor (mkSelector "setFunctionCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intersectionFunctionTableDescriptor@
intersectionFunctionTableDescriptorSelector :: Selector
intersectionFunctionTableDescriptorSelector = mkSelector "intersectionFunctionTableDescriptor"

-- | @Selector@ for @functionCount@
functionCountSelector :: Selector
functionCountSelector = mkSelector "functionCount"

-- | @Selector@ for @setFunctionCount:@
setFunctionCountSelector :: Selector
setFunctionCountSelector = mkSelector "setFunctionCount:"

