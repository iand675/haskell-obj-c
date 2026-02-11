{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAnimatedVector3Array@.
module ObjC.ModelIO.MDLAnimatedVector3Array
  ( MDLAnimatedVector3Array
  , IsMDLAnimatedVector3Array(..)
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
initWithElementCount :: IsMDLAnimatedVector3Array mdlAnimatedVector3Array => mdlAnimatedVector3Array -> CULong -> IO RawId
initWithElementCount mdlAnimatedVector3Array  arrayElementCount =
  fmap (RawId . castPtr) $ sendMsg mdlAnimatedVector3Array (mkSelector "initWithElementCount:") (retPtr retVoid) [argCULong (fromIntegral arrayElementCount)]

-- | @- elementCount@
elementCount :: IsMDLAnimatedVector3Array mdlAnimatedVector3Array => mdlAnimatedVector3Array -> IO CULong
elementCount mdlAnimatedVector3Array  =
  sendMsg mdlAnimatedVector3Array (mkSelector "elementCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

