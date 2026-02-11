{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Allows you to specify additional binary functions to link to each stage of a render pipeline.
--
-- Generated bindings for @MTL4RenderPipelineBinaryFunctionsDescriptor@.
module ObjC.Metal.MTL4RenderPipelineBinaryFunctionsDescriptor
  ( MTL4RenderPipelineBinaryFunctionsDescriptor
  , IsMTL4RenderPipelineBinaryFunctionsDescriptor(..)
  , reset
  , resetSelector


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

-- | Resets this descriptor to its default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4RenderPipelineBinaryFunctionsDescriptor mtL4RenderPipelineBinaryFunctionsDescriptor => mtL4RenderPipelineBinaryFunctionsDescriptor -> IO ()
reset mtL4RenderPipelineBinaryFunctionsDescriptor  =
  sendMsg mtL4RenderPipelineBinaryFunctionsDescriptor (mkSelector "reset") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

