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
  , invalidateFlowLayoutDelegateMetricsSelector
  , setInvalidateFlowLayoutDelegateMetricsSelector
  , invalidateFlowLayoutAttributesSelector
  , setInvalidateFlowLayoutAttributesSelector


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

-- | @- invalidateFlowLayoutDelegateMetrics@
invalidateFlowLayoutDelegateMetrics :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> IO Bool
invalidateFlowLayoutDelegateMetrics nsCollectionViewFlowLayoutInvalidationContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewFlowLayoutInvalidationContext (mkSelector "invalidateFlowLayoutDelegateMetrics") retCULong []

-- | @- setInvalidateFlowLayoutDelegateMetrics:@
setInvalidateFlowLayoutDelegateMetrics :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> Bool -> IO ()
setInvalidateFlowLayoutDelegateMetrics nsCollectionViewFlowLayoutInvalidationContext  value =
  sendMsg nsCollectionViewFlowLayoutInvalidationContext (mkSelector "setInvalidateFlowLayoutDelegateMetrics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- invalidateFlowLayoutAttributes@
invalidateFlowLayoutAttributes :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> IO Bool
invalidateFlowLayoutAttributes nsCollectionViewFlowLayoutInvalidationContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewFlowLayoutInvalidationContext (mkSelector "invalidateFlowLayoutAttributes") retCULong []

-- | @- setInvalidateFlowLayoutAttributes:@
setInvalidateFlowLayoutAttributes :: IsNSCollectionViewFlowLayoutInvalidationContext nsCollectionViewFlowLayoutInvalidationContext => nsCollectionViewFlowLayoutInvalidationContext -> Bool -> IO ()
setInvalidateFlowLayoutAttributes nsCollectionViewFlowLayoutInvalidationContext  value =
  sendMsg nsCollectionViewFlowLayoutInvalidationContext (mkSelector "setInvalidateFlowLayoutAttributes:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateFlowLayoutDelegateMetrics@
invalidateFlowLayoutDelegateMetricsSelector :: Selector
invalidateFlowLayoutDelegateMetricsSelector = mkSelector "invalidateFlowLayoutDelegateMetrics"

-- | @Selector@ for @setInvalidateFlowLayoutDelegateMetrics:@
setInvalidateFlowLayoutDelegateMetricsSelector :: Selector
setInvalidateFlowLayoutDelegateMetricsSelector = mkSelector "setInvalidateFlowLayoutDelegateMetrics:"

-- | @Selector@ for @invalidateFlowLayoutAttributes@
invalidateFlowLayoutAttributesSelector :: Selector
invalidateFlowLayoutAttributesSelector = mkSelector "invalidateFlowLayoutAttributes"

-- | @Selector@ for @setInvalidateFlowLayoutAttributes:@
setInvalidateFlowLayoutAttributesSelector :: Selector
setInvalidateFlowLayoutAttributesSelector = mkSelector "setInvalidateFlowLayoutAttributes:"

