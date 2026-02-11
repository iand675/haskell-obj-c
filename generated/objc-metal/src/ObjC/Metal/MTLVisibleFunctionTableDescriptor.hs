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
  , visibleFunctionTableDescriptorSelector
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

-- | visibleFunctionTableDescriptor
--
-- Create an autoreleased visible function table descriptor
--
-- ObjC selector: @+ visibleFunctionTableDescriptor@
visibleFunctionTableDescriptor :: IO (Id MTLVisibleFunctionTableDescriptor)
visibleFunctionTableDescriptor  =
  do
    cls' <- getRequiredClass "MTLVisibleFunctionTableDescriptor"
    sendClassMsg cls' (mkSelector "visibleFunctionTableDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- functionCount@
functionCount :: IsMTLVisibleFunctionTableDescriptor mtlVisibleFunctionTableDescriptor => mtlVisibleFunctionTableDescriptor -> IO CULong
functionCount mtlVisibleFunctionTableDescriptor  =
  sendMsg mtlVisibleFunctionTableDescriptor (mkSelector "functionCount") retCULong []

-- | functionCount
--
-- The number of functions in the table.
--
-- ObjC selector: @- setFunctionCount:@
setFunctionCount :: IsMTLVisibleFunctionTableDescriptor mtlVisibleFunctionTableDescriptor => mtlVisibleFunctionTableDescriptor -> CULong -> IO ()
setFunctionCount mtlVisibleFunctionTableDescriptor  value =
  sendMsg mtlVisibleFunctionTableDescriptor (mkSelector "setFunctionCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @visibleFunctionTableDescriptor@
visibleFunctionTableDescriptorSelector :: Selector
visibleFunctionTableDescriptorSelector = mkSelector "visibleFunctionTableDescriptor"

-- | @Selector@ for @functionCount@
functionCountSelector :: Selector
functionCountSelector = mkSelector "functionCount"

-- | @Selector@ for @setFunctionCount:@
setFunctionCountSelector :: Selector
setFunctionCountSelector = mkSelector "setFunctionCount:"

