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
  , initWithArgumentIndexSelector
  , argumentIndexSelector
  , setArgumentIndexSelector


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

-- | @- initWithArgumentIndex:@
initWithArgumentIndex :: IsMTLFunctionStitchingInputNode mtlFunctionStitchingInputNode => mtlFunctionStitchingInputNode -> CULong -> IO (Id MTLFunctionStitchingInputNode)
initWithArgumentIndex mtlFunctionStitchingInputNode  argument =
  sendMsg mtlFunctionStitchingInputNode (mkSelector "initWithArgumentIndex:") (retPtr retVoid) [argCULong (fromIntegral argument)] >>= ownedObject . castPtr

-- | @- argumentIndex@
argumentIndex :: IsMTLFunctionStitchingInputNode mtlFunctionStitchingInputNode => mtlFunctionStitchingInputNode -> IO CULong
argumentIndex mtlFunctionStitchingInputNode  =
  sendMsg mtlFunctionStitchingInputNode (mkSelector "argumentIndex") retCULong []

-- | @- setArgumentIndex:@
setArgumentIndex :: IsMTLFunctionStitchingInputNode mtlFunctionStitchingInputNode => mtlFunctionStitchingInputNode -> CULong -> IO ()
setArgumentIndex mtlFunctionStitchingInputNode  value =
  sendMsg mtlFunctionStitchingInputNode (mkSelector "setArgumentIndex:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithArgumentIndex:@
initWithArgumentIndexSelector :: Selector
initWithArgumentIndexSelector = mkSelector "initWithArgumentIndex:"

-- | @Selector@ for @argumentIndex@
argumentIndexSelector :: Selector
argumentIndexSelector = mkSelector "argumentIndex"

-- | @Selector@ for @setArgumentIndex:@
setArgumentIndexSelector :: Selector
setArgumentIndexSelector = mkSelector "setArgumentIndex:"

