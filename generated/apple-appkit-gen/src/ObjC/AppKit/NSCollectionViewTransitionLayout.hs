{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewTransitionLayout@.
module ObjC.AppKit.NSCollectionViewTransitionLayout
  ( NSCollectionViewTransitionLayout
  , IsNSCollectionViewTransitionLayout(..)
  , initWithCurrentLayout_nextLayout
  , updateValue_forAnimatedKey
  , valueForAnimatedKey
  , transitionProgress
  , setTransitionProgress
  , currentLayout
  , nextLayout
  , currentLayoutSelector
  , initWithCurrentLayout_nextLayoutSelector
  , nextLayoutSelector
  , setTransitionProgressSelector
  , transitionProgressSelector
  , updateValue_forAnimatedKeySelector
  , valueForAnimatedKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCurrentLayout:nextLayout:@
initWithCurrentLayout_nextLayout :: (IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout, IsNSCollectionViewLayout currentLayout, IsNSCollectionViewLayout newLayout) => nsCollectionViewTransitionLayout -> currentLayout -> newLayout -> IO (Id NSCollectionViewTransitionLayout)
initWithCurrentLayout_nextLayout nsCollectionViewTransitionLayout currentLayout newLayout =
  sendOwnedMessage nsCollectionViewTransitionLayout initWithCurrentLayout_nextLayoutSelector (toNSCollectionViewLayout currentLayout) (toNSCollectionViewLayout newLayout)

-- | @- updateValue:forAnimatedKey:@
updateValue_forAnimatedKey :: (IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout, IsNSString key) => nsCollectionViewTransitionLayout -> CDouble -> key -> IO ()
updateValue_forAnimatedKey nsCollectionViewTransitionLayout value key =
  sendMessage nsCollectionViewTransitionLayout updateValue_forAnimatedKeySelector value (toNSString key)

-- | @- valueForAnimatedKey:@
valueForAnimatedKey :: (IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout, IsNSString key) => nsCollectionViewTransitionLayout -> key -> IO CDouble
valueForAnimatedKey nsCollectionViewTransitionLayout key =
  sendMessage nsCollectionViewTransitionLayout valueForAnimatedKeySelector (toNSString key)

-- | @- transitionProgress@
transitionProgress :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> IO CDouble
transitionProgress nsCollectionViewTransitionLayout =
  sendMessage nsCollectionViewTransitionLayout transitionProgressSelector

-- | @- setTransitionProgress:@
setTransitionProgress :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> CDouble -> IO ()
setTransitionProgress nsCollectionViewTransitionLayout value =
  sendMessage nsCollectionViewTransitionLayout setTransitionProgressSelector value

-- | @- currentLayout@
currentLayout :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> IO (Id NSCollectionViewLayout)
currentLayout nsCollectionViewTransitionLayout =
  sendMessage nsCollectionViewTransitionLayout currentLayoutSelector

-- | @- nextLayout@
nextLayout :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> IO (Id NSCollectionViewLayout)
nextLayout nsCollectionViewTransitionLayout =
  sendMessage nsCollectionViewTransitionLayout nextLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCurrentLayout:nextLayout:@
initWithCurrentLayout_nextLayoutSelector :: Selector '[Id NSCollectionViewLayout, Id NSCollectionViewLayout] (Id NSCollectionViewTransitionLayout)
initWithCurrentLayout_nextLayoutSelector = mkSelector "initWithCurrentLayout:nextLayout:"

-- | @Selector@ for @updateValue:forAnimatedKey:@
updateValue_forAnimatedKeySelector :: Selector '[CDouble, Id NSString] ()
updateValue_forAnimatedKeySelector = mkSelector "updateValue:forAnimatedKey:"

-- | @Selector@ for @valueForAnimatedKey:@
valueForAnimatedKeySelector :: Selector '[Id NSString] CDouble
valueForAnimatedKeySelector = mkSelector "valueForAnimatedKey:"

-- | @Selector@ for @transitionProgress@
transitionProgressSelector :: Selector '[] CDouble
transitionProgressSelector = mkSelector "transitionProgress"

-- | @Selector@ for @setTransitionProgress:@
setTransitionProgressSelector :: Selector '[CDouble] ()
setTransitionProgressSelector = mkSelector "setTransitionProgress:"

-- | @Selector@ for @currentLayout@
currentLayoutSelector :: Selector '[] (Id NSCollectionViewLayout)
currentLayoutSelector = mkSelector "currentLayout"

-- | @Selector@ for @nextLayout@
nextLayoutSelector :: Selector '[] (Id NSCollectionViewLayout)
nextLayoutSelector = mkSelector "nextLayout"

