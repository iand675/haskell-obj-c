{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLFunctionStitchingInputNode
--
-- An indexed input node of the produced stitched function.
--
-- Generated bindings for @MTLFunctionStitchingInputNode@.
module ObjC.Metal.MTLFunctionStitchingInputNode
  ( MTLFunctionStitchingInputNode
  , IsMTLFunctionStitchingInputNode(..)
  , initWithArgumentIndex
  , argumentIndex
  , setArgumentIndex
  , argumentIndexSelector
  , initWithArgumentIndexSelector
  , setArgumentIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithArgumentIndex:@
initWithArgumentIndex :: IsMTLFunctionStitchingInputNode mtlFunctionStitchingInputNode => mtlFunctionStitchingInputNode -> CULong -> IO (Id MTLFunctionStitchingInputNode)
initWithArgumentIndex mtlFunctionStitchingInputNode argument =
  sendOwnedMessage mtlFunctionStitchingInputNode initWithArgumentIndexSelector argument

-- | @- argumentIndex@
argumentIndex :: IsMTLFunctionStitchingInputNode mtlFunctionStitchingInputNode => mtlFunctionStitchingInputNode -> IO CULong
argumentIndex mtlFunctionStitchingInputNode =
  sendMessage mtlFunctionStitchingInputNode argumentIndexSelector

-- | @- setArgumentIndex:@
setArgumentIndex :: IsMTLFunctionStitchingInputNode mtlFunctionStitchingInputNode => mtlFunctionStitchingInputNode -> CULong -> IO ()
setArgumentIndex mtlFunctionStitchingInputNode value =
  sendMessage mtlFunctionStitchingInputNode setArgumentIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithArgumentIndex:@
initWithArgumentIndexSelector :: Selector '[CULong] (Id MTLFunctionStitchingInputNode)
initWithArgumentIndexSelector = mkSelector "initWithArgumentIndex:"

-- | @Selector@ for @argumentIndex@
argumentIndexSelector :: Selector '[] CULong
argumentIndexSelector = mkSelector "argumentIndex"

-- | @Selector@ for @setArgumentIndex:@
setArgumentIndexSelector :: Selector '[CULong] ()
setArgumentIndexSelector = mkSelector "setArgumentIndex:"

