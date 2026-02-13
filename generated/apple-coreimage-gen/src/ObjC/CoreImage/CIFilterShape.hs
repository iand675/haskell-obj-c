{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIFilterShape@.
module ObjC.CoreImage.CIFilterShape
  ( CIFilterShape
  , IsCIFilterShape(..)
  , insetByX_Y
  , unionWith
  , intersectWith
  , insetByX_YSelector
  , intersectWithSelector
  , unionWithSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- insetByX:Y:@
insetByX_Y :: IsCIFilterShape ciFilterShape => ciFilterShape -> CInt -> CInt -> IO (Id CIFilterShape)
insetByX_Y ciFilterShape dx dy =
  sendMessage ciFilterShape insetByX_YSelector dx dy

-- | @- unionWith:@
unionWith :: (IsCIFilterShape ciFilterShape, IsCIFilterShape s2) => ciFilterShape -> s2 -> IO (Id CIFilterShape)
unionWith ciFilterShape s2 =
  sendMessage ciFilterShape unionWithSelector (toCIFilterShape s2)

-- | @- intersectWith:@
intersectWith :: (IsCIFilterShape ciFilterShape, IsCIFilterShape s2) => ciFilterShape -> s2 -> IO (Id CIFilterShape)
intersectWith ciFilterShape s2 =
  sendMessage ciFilterShape intersectWithSelector (toCIFilterShape s2)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insetByX:Y:@
insetByX_YSelector :: Selector '[CInt, CInt] (Id CIFilterShape)
insetByX_YSelector = mkSelector "insetByX:Y:"

-- | @Selector@ for @unionWith:@
unionWithSelector :: Selector '[Id CIFilterShape] (Id CIFilterShape)
unionWithSelector = mkSelector "unionWith:"

-- | @Selector@ for @intersectWith:@
intersectWithSelector :: Selector '[Id CIFilterShape] (Id CIFilterShape)
intersectWithSelector = mkSelector "intersectWith:"

