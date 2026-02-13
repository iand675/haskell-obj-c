{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAnimatedQuaternionArray@.
module ObjC.ModelIO.MDLAnimatedQuaternionArray
  ( MDLAnimatedQuaternionArray
  , IsMDLAnimatedQuaternionArray(..)
  , initWithElementCount
  , elementCount
  , elementCountSelector
  , initWithElementCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithElementCount:@
initWithElementCount :: IsMDLAnimatedQuaternionArray mdlAnimatedQuaternionArray => mdlAnimatedQuaternionArray -> CULong -> IO RawId
initWithElementCount mdlAnimatedQuaternionArray arrayElementCount =
  sendOwnedMessage mdlAnimatedQuaternionArray initWithElementCountSelector arrayElementCount

-- | @- elementCount@
elementCount :: IsMDLAnimatedQuaternionArray mdlAnimatedQuaternionArray => mdlAnimatedQuaternionArray -> IO CULong
elementCount mdlAnimatedQuaternionArray =
  sendMessage mdlAnimatedQuaternionArray elementCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector '[CULong] RawId
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CULong
elementCountSelector = mkSelector "elementCount"

