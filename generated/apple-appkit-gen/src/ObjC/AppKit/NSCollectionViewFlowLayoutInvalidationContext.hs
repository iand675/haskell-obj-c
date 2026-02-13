{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewFlowLayoutInvalidationContext@.
module ObjC.AppKit.NSCollectionViewFlowLayoutInvalidationContext
  ( NSCollectionViewFlowLayoutInvalidationContext
  , IsNSCollectionViewFlowLayoutInvalidationContext(..)
  , invalidateFlowLayoutDelegateMetrics
  , setInvalidateFlowLayoutDelegateMetrics
  , invalidateFlowLayoutAttributes
  , setInvalidateFlowLayoutAttributes
  , invalidateFlowLayoutAttributesSelector
  , invalidateFlowLayoutDelegateMetricsSelector
  , setInvalidateFlowLayoutAttributesSelector
  , setInvalidateFlowLayoutDelegateMetricsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- invalidateFlowLayoutDelegateMetrics@
invalidateFlowLayoutDelegateMetrics :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> IO Bool
invalidateFlowLayoutDelegateMetrics nsCollectionViewFlowLayoutInvalidationContext =
  sendMessage nsCollectionViewFlowLayoutInvalidationContext invalidateFlowLayoutDelegateMetricsSelector

-- | @- setInvalidateFlowLayoutDelegateMetrics:@
setInvalidateFlowLayoutDelegateMetrics :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> Bool -> IO ()
setInvalidateFlowLayoutDelegateMetrics nsCollectionViewFlowLayoutInvalidationContext value =
  sendMessage nsCollectionViewFlowLayoutInvalidationContext setInvalidateFlowLayoutDelegateMetricsSelector value

-- | @- invalidateFlowLayoutAttributes@
invalidateFlowLayoutAttributes :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> IO Bool
invalidateFlowLayoutAttributes nsCollectionViewFlowLayoutInvalidationContext =
  sendMessage nsCollectionViewFlowLayoutInvalidationContext invalidateFlowLayoutAttributesSelector

-- | @- setInvalidateFlowLayoutAttributes:@
setInvalidateFlowLayoutAttributes :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> Bool -> IO ()
setInvalidateFlowLayoutAttributes nsCollectionViewFlowLayoutInvalidationContext value =
  sendMessage nsCollectionViewFlowLayoutInvalidationContext setInvalidateFlowLayoutAttributesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateFlowLayoutDelegateMetrics@
invalidateFlowLayoutDelegateMetricsSelector :: Selector '[] Bool
invalidateFlowLayoutDelegateMetricsSelector = mkSelector "invalidateFlowLayoutDelegateMetrics"

-- | @Selector@ for @setInvalidateFlowLayoutDelegateMetrics:@
setInvalidateFlowLayoutDelegateMetricsSelector :: Selector '[Bool] ()
setInvalidateFlowLayoutDelegateMetricsSelector = mkSelector "setInvalidateFlowLayoutDelegateMetrics:"

-- | @Selector@ for @invalidateFlowLayoutAttributes@
invalidateFlowLayoutAttributesSelector :: Selector '[] Bool
invalidateFlowLayoutAttributesSelector = mkSelector "invalidateFlowLayoutAttributes"

-- | @Selector@ for @setInvalidateFlowLayoutAttributes:@
setInvalidateFlowLayoutAttributesSelector :: Selector '[Bool] ()
setInvalidateFlowLayoutAttributesSelector = mkSelector "setInvalidateFlowLayoutAttributes:"

