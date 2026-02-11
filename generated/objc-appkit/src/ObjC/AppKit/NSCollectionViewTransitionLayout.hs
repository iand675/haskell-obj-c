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
  , initWithCurrentLayout_nextLayoutSelector
  , updateValue_forAnimatedKeySelector
  , valueForAnimatedKeySelector
  , transitionProgressSelector
  , setTransitionProgressSelector
  , currentLayoutSelector
  , nextLayoutSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCurrentLayout:nextLayout:@
initWithCurrentLayout_nextLayout :: (IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout, IsNSCollectionViewLayout currentLayout, IsNSCollectionViewLayout newLayout) => nsCollectionViewTransitionLayout -> currentLayout -> newLayout -> IO (Id NSCollectionViewTransitionLayout)
initWithCurrentLayout_nextLayout nsCollectionViewTransitionLayout  currentLayout newLayout =
withObjCPtr currentLayout $ \raw_currentLayout ->
  withObjCPtr newLayout $ \raw_newLayout ->
      sendMsg nsCollectionViewTransitionLayout (mkSelector "initWithCurrentLayout:nextLayout:") (retPtr retVoid) [argPtr (castPtr raw_currentLayout :: Ptr ()), argPtr (castPtr raw_newLayout :: Ptr ())] >>= ownedObject . castPtr

-- | @- updateValue:forAnimatedKey:@
updateValue_forAnimatedKey :: (IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout, IsNSString key) => nsCollectionViewTransitionLayout -> CDouble -> key -> IO ()
updateValue_forAnimatedKey nsCollectionViewTransitionLayout  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCollectionViewTransitionLayout (mkSelector "updateValue:forAnimatedKey:") retVoid [argCDouble (fromIntegral value), argPtr (castPtr raw_key :: Ptr ())]

-- | @- valueForAnimatedKey:@
valueForAnimatedKey :: (IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout, IsNSString key) => nsCollectionViewTransitionLayout -> key -> IO CDouble
valueForAnimatedKey nsCollectionViewTransitionLayout  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsCollectionViewTransitionLayout (mkSelector "valueForAnimatedKey:") retCDouble [argPtr (castPtr raw_key :: Ptr ())]

-- | @- transitionProgress@
transitionProgress :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> IO CDouble
transitionProgress nsCollectionViewTransitionLayout  =
  sendMsg nsCollectionViewTransitionLayout (mkSelector "transitionProgress") retCDouble []

-- | @- setTransitionProgress:@
setTransitionProgress :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> CDouble -> IO ()
setTransitionProgress nsCollectionViewTransitionLayout  value =
  sendMsg nsCollectionViewTransitionLayout (mkSelector "setTransitionProgress:") retVoid [argCDouble (fromIntegral value)]

-- | @- currentLayout@
currentLayout :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> IO (Id NSCollectionViewLayout)
currentLayout nsCollectionViewTransitionLayout  =
  sendMsg nsCollectionViewTransitionLayout (mkSelector "currentLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextLayout@
nextLayout :: IsNSCollectionViewTransitionLayout nsCollectionViewTransitionLayout => nsCollectionViewTransitionLayout -> IO (Id NSCollectionViewLayout)
nextLayout nsCollectionViewTransitionLayout  =
  sendMsg nsCollectionViewTransitionLayout (mkSelector "nextLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCurrentLayout:nextLayout:@
initWithCurrentLayout_nextLayoutSelector :: Selector
initWithCurrentLayout_nextLayoutSelector = mkSelector "initWithCurrentLayout:nextLayout:"

-- | @Selector@ for @updateValue:forAnimatedKey:@
updateValue_forAnimatedKeySelector :: Selector
updateValue_forAnimatedKeySelector = mkSelector "updateValue:forAnimatedKey:"

-- | @Selector@ for @valueForAnimatedKey:@
valueForAnimatedKeySelector :: Selector
valueForAnimatedKeySelector = mkSelector "valueForAnimatedKey:"

-- | @Selector@ for @transitionProgress@
transitionProgressSelector :: Selector
transitionProgressSelector = mkSelector "transitionProgress"

-- | @Selector@ for @setTransitionProgress:@
setTransitionProgressSelector :: Selector
setTransitionProgressSelector = mkSelector "setTransitionProgress:"

-- | @Selector@ for @currentLayout@
currentLayoutSelector :: Selector
currentLayoutSelector = mkSelector "currentLayout"

-- | @Selector@ for @nextLayout@
nextLayoutSelector :: Selector
nextLayoutSelector = mkSelector "nextLayout"

