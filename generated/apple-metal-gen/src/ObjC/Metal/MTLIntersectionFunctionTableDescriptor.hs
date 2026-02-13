{-# LANGUAGE DataKinds #-}
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
  , functionCountSelector
  , intersectionFunctionTableDescriptorSelector
  , setFunctionCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' intersectionFunctionTableDescriptorSelector

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- functionCount@
functionCount :: IsMTLIntersectionFunctionTableDescriptor mtlIntersectionFunctionTableDescriptor => mtlIntersectionFunctionTableDescriptor -> IO CULong
functionCount mtlIntersectionFunctionTableDescriptor =
  sendMessage mtlIntersectionFunctionTableDescriptor functionCountSelector

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- setFunctionCount:@
setFunctionCount :: IsMTLIntersectionFunctionTableDescriptor mtlIntersectionFunctionTableDescriptor => mtlIntersectionFunctionTableDescriptor -> CULong -> IO ()
setFunctionCount mtlIntersectionFunctionTableDescriptor value =
  sendMessage mtlIntersectionFunctionTableDescriptor setFunctionCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intersectionFunctionTableDescriptor@
intersectionFunctionTableDescriptorSelector :: Selector '[] (Id MTLIntersectionFunctionTableDescriptor)
intersectionFunctionTableDescriptorSelector = mkSelector "intersectionFunctionTableDescriptor"

-- | @Selector@ for @functionCount@
functionCountSelector :: Selector '[] CULong
functionCountSelector = mkSelector "functionCount"

-- | @Selector@ for @setFunctionCount:@
setFunctionCountSelector :: Selector '[CULong] ()
setFunctionCountSelector = mkSelector "setFunctionCount:"

