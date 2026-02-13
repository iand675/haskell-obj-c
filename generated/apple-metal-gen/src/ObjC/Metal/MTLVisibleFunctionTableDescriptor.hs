{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLVisibleFunctionTableDescriptor@.
module ObjC.Metal.MTLVisibleFunctionTableDescriptor
  ( MTLVisibleFunctionTableDescriptor
  , IsMTLVisibleFunctionTableDescriptor(..)
  , visibleFunctionTableDescriptor
  , functionCount
  , setFunctionCount
  , functionCountSelector
  , setFunctionCountSelector
  , visibleFunctionTableDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | visibleFunctionTableDescriptor
--
-- Create an autoreleased visible function table descriptor
--
-- ObjC selector: @+ visibleFunctionTableDescriptor@
visibleFunctionTableDescriptor :: IO (Id MTLVisibleFunctionTableDescriptor)
visibleFunctionTableDescriptor  =
  do
    cls' <- getRequiredClass "MTLVisibleFunctionTableDescriptor"
    sendClassMessage cls' visibleFunctionTableDescriptorSelector

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- functionCount@
functionCount :: IsMTLVisibleFunctionTableDescriptor mtlVisibleFunctionTableDescriptor => mtlVisibleFunctionTableDescriptor -> IO CULong
functionCount mtlVisibleFunctionTableDescriptor =
  sendMessage mtlVisibleFunctionTableDescriptor functionCountSelector

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- setFunctionCount:@
setFunctionCount :: IsMTLVisibleFunctionTableDescriptor mtlVisibleFunctionTableDescriptor => mtlVisibleFunctionTableDescriptor -> CULong -> IO ()
setFunctionCount mtlVisibleFunctionTableDescriptor value =
  sendMessage mtlVisibleFunctionTableDescriptor setFunctionCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @visibleFunctionTableDescriptor@
visibleFunctionTableDescriptorSelector :: Selector '[] (Id MTLVisibleFunctionTableDescriptor)
visibleFunctionTableDescriptorSelector = mkSelector "visibleFunctionTableDescriptor"

-- | @Selector@ for @functionCount@
functionCountSelector :: Selector '[] CULong
functionCountSelector = mkSelector "functionCount"

-- | @Selector@ for @setFunctionCount:@
setFunctionCountSelector :: Selector '[CULong] ()
setFunctionCountSelector = mkSelector "setFunctionCount:"

