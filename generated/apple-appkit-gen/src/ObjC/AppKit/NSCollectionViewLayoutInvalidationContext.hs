{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewLayoutInvalidationContext@.
module ObjC.AppKit.NSCollectionViewLayoutInvalidationContext
  ( NSCollectionViewLayoutInvalidationContext
  , IsNSCollectionViewLayoutInvalidationContext(..)
  , invalidateItemsAtIndexPaths
  , invalidateSupplementaryElementsOfKind_atIndexPaths
  , invalidateDecorationElementsOfKind_atIndexPaths
  , invalidateEverything
  , invalidateDataSourceCounts
  , invalidatedItemIndexPaths
  , invalidatedSupplementaryIndexPaths
  , invalidatedDecorationIndexPaths
  , contentOffsetAdjustment
  , setContentOffsetAdjustment
  , contentSizeAdjustment
  , setContentSizeAdjustment
  , contentOffsetAdjustmentSelector
  , contentSizeAdjustmentSelector
  , invalidateDataSourceCountsSelector
  , invalidateDecorationElementsOfKind_atIndexPathsSelector
  , invalidateEverythingSelector
  , invalidateItemsAtIndexPathsSelector
  , invalidateSupplementaryElementsOfKind_atIndexPathsSelector
  , invalidatedDecorationIndexPathsSelector
  , invalidatedItemIndexPathsSelector
  , invalidatedSupplementaryIndexPathsSelector
  , setContentOffsetAdjustmentSelector
  , setContentSizeAdjustmentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- invalidateItemsAtIndexPaths:@
invalidateItemsAtIndexPaths :: (IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext, IsNSSet indexPaths) => nsCollectionViewLayoutInvalidationContext -> indexPaths -> IO ()
invalidateItemsAtIndexPaths nsCollectionViewLayoutInvalidationContext indexPaths =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidateItemsAtIndexPathsSelector (toNSSet indexPaths)

-- | @- invalidateSupplementaryElementsOfKind:atIndexPaths:@
invalidateSupplementaryElementsOfKind_atIndexPaths :: (IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext, IsNSString elementKind, IsNSSet indexPaths) => nsCollectionViewLayoutInvalidationContext -> elementKind -> indexPaths -> IO ()
invalidateSupplementaryElementsOfKind_atIndexPaths nsCollectionViewLayoutInvalidationContext elementKind indexPaths =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidateSupplementaryElementsOfKind_atIndexPathsSelector (toNSString elementKind) (toNSSet indexPaths)

-- | @- invalidateDecorationElementsOfKind:atIndexPaths:@
invalidateDecorationElementsOfKind_atIndexPaths :: (IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext, IsNSString elementKind, IsNSSet indexPaths) => nsCollectionViewLayoutInvalidationContext -> elementKind -> indexPaths -> IO ()
invalidateDecorationElementsOfKind_atIndexPaths nsCollectionViewLayoutInvalidationContext elementKind indexPaths =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidateDecorationElementsOfKind_atIndexPathsSelector (toNSString elementKind) (toNSSet indexPaths)

-- | @- invalidateEverything@
invalidateEverything :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO Bool
invalidateEverything nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidateEverythingSelector

-- | @- invalidateDataSourceCounts@
invalidateDataSourceCounts :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO Bool
invalidateDataSourceCounts nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidateDataSourceCountsSelector

-- | @- invalidatedItemIndexPaths@
invalidatedItemIndexPaths :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO (Id NSSet)
invalidatedItemIndexPaths nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidatedItemIndexPathsSelector

-- | @- invalidatedSupplementaryIndexPaths@
invalidatedSupplementaryIndexPaths :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO (Id NSDictionary)
invalidatedSupplementaryIndexPaths nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidatedSupplementaryIndexPathsSelector

-- | @- invalidatedDecorationIndexPaths@
invalidatedDecorationIndexPaths :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO (Id NSDictionary)
invalidatedDecorationIndexPaths nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext invalidatedDecorationIndexPathsSelector

-- | @- contentOffsetAdjustment@
contentOffsetAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO NSPoint
contentOffsetAdjustment nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext contentOffsetAdjustmentSelector

-- | @- setContentOffsetAdjustment:@
setContentOffsetAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> NSPoint -> IO ()
setContentOffsetAdjustment nsCollectionViewLayoutInvalidationContext value =
  sendMessage nsCollectionViewLayoutInvalidationContext setContentOffsetAdjustmentSelector value

-- | @- contentSizeAdjustment@
contentSizeAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO NSSize
contentSizeAdjustment nsCollectionViewLayoutInvalidationContext =
  sendMessage nsCollectionViewLayoutInvalidationContext contentSizeAdjustmentSelector

-- | @- setContentSizeAdjustment:@
setContentSizeAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> NSSize -> IO ()
setContentSizeAdjustment nsCollectionViewLayoutInvalidationContext value =
  sendMessage nsCollectionViewLayoutInvalidationContext setContentSizeAdjustmentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateItemsAtIndexPaths:@
invalidateItemsAtIndexPathsSelector :: Selector '[Id NSSet] ()
invalidateItemsAtIndexPathsSelector = mkSelector "invalidateItemsAtIndexPaths:"

-- | @Selector@ for @invalidateSupplementaryElementsOfKind:atIndexPaths:@
invalidateSupplementaryElementsOfKind_atIndexPathsSelector :: Selector '[Id NSString, Id NSSet] ()
invalidateSupplementaryElementsOfKind_atIndexPathsSelector = mkSelector "invalidateSupplementaryElementsOfKind:atIndexPaths:"

-- | @Selector@ for @invalidateDecorationElementsOfKind:atIndexPaths:@
invalidateDecorationElementsOfKind_atIndexPathsSelector :: Selector '[Id NSString, Id NSSet] ()
invalidateDecorationElementsOfKind_atIndexPathsSelector = mkSelector "invalidateDecorationElementsOfKind:atIndexPaths:"

-- | @Selector@ for @invalidateEverything@
invalidateEverythingSelector :: Selector '[] Bool
invalidateEverythingSelector = mkSelector "invalidateEverything"

-- | @Selector@ for @invalidateDataSourceCounts@
invalidateDataSourceCountsSelector :: Selector '[] Bool
invalidateDataSourceCountsSelector = mkSelector "invalidateDataSourceCounts"

-- | @Selector@ for @invalidatedItemIndexPaths@
invalidatedItemIndexPathsSelector :: Selector '[] (Id NSSet)
invalidatedItemIndexPathsSelector = mkSelector "invalidatedItemIndexPaths"

-- | @Selector@ for @invalidatedSupplementaryIndexPaths@
invalidatedSupplementaryIndexPathsSelector :: Selector '[] (Id NSDictionary)
invalidatedSupplementaryIndexPathsSelector = mkSelector "invalidatedSupplementaryIndexPaths"

-- | @Selector@ for @invalidatedDecorationIndexPaths@
invalidatedDecorationIndexPathsSelector :: Selector '[] (Id NSDictionary)
invalidatedDecorationIndexPathsSelector = mkSelector "invalidatedDecorationIndexPaths"

-- | @Selector@ for @contentOffsetAdjustment@
contentOffsetAdjustmentSelector :: Selector '[] NSPoint
contentOffsetAdjustmentSelector = mkSelector "contentOffsetAdjustment"

-- | @Selector@ for @setContentOffsetAdjustment:@
setContentOffsetAdjustmentSelector :: Selector '[NSPoint] ()
setContentOffsetAdjustmentSelector = mkSelector "setContentOffsetAdjustment:"

-- | @Selector@ for @contentSizeAdjustment@
contentSizeAdjustmentSelector :: Selector '[] NSSize
contentSizeAdjustmentSelector = mkSelector "contentSizeAdjustment"

-- | @Selector@ for @setContentSizeAdjustment:@
setContentSizeAdjustmentSelector :: Selector '[NSSize] ()
setContentSizeAdjustmentSelector = mkSelector "setContentSizeAdjustment:"

