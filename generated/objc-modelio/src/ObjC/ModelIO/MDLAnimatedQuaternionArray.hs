{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAnimatedQuaternionArray@.
module ObjC.ModelIO.MDLAnimatedQuaternionArray
  ( MDLAnimatedQuaternionArray
  , IsMDLAnimatedQuaternionArray(..)
  , initWithElementCount
  , elementCount
  , initWithElementCountSelector
  , elementCountSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithElementCount:@
initWithElementCount :: IsMDLAnimatedQuaternionArray mdlAnimatedQuaternionArray => mdlAnimatedQuaternionArray -> CULong -> IO RawId
initWithElementCount mdlAnimatedQuaternionArray  arrayElementCount =
  fmap (RawId . castPtr) $ sendMsg mdlAnimatedQuaternionArray (mkSelector "initWithElementCount:") (retPtr retVoid) [argCULong (fromIntegral arrayElementCount)]

-- | @- elementCount@
elementCount :: IsMDLAnimatedQuaternionArray mdlAnimatedQuaternionArray => mdlAnimatedQuaternionArray -> IO CULong
elementCount mdlAnimatedQuaternionArray  =
  sendMsg mdlAnimatedQuaternionArray (mkSelector "elementCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

