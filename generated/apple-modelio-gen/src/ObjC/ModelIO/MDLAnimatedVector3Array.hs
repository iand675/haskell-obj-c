{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAnimatedVector3Array@.
module ObjC.ModelIO.MDLAnimatedVector3Array
  ( MDLAnimatedVector3Array
  , IsMDLAnimatedVector3Array(..)
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
initWithElementCount :: IsMDLAnimatedVector3Array mdlAnimatedVector3Array => mdlAnimatedVector3Array -> CULong -> IO RawId
initWithElementCount mdlAnimatedVector3Array arrayElementCount =
  sendOwnedMessage mdlAnimatedVector3Array initWithElementCountSelector arrayElementCount

-- | @- elementCount@
elementCount :: IsMDLAnimatedVector3Array mdlAnimatedVector3Array => mdlAnimatedVector3Array -> IO CULong
elementCount mdlAnimatedVector3Array =
  sendMessage mdlAnimatedVector3Array elementCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector '[CULong] RawId
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CULong
elementCountSelector = mkSelector "elementCount"

