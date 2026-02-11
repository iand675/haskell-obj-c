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
  , invalidateItemsAtIndexPathsSelector
  , invalidateSupplementaryElementsOfKind_atIndexPathsSelector
  , invalidateDecorationElementsOfKind_atIndexPathsSelector
  , invalidateEverythingSelector
  , invalidateDataSourceCountsSelector
  , invalidatedItemIndexPathsSelector
  , invalidatedSupplementaryIndexPathsSelector
  , invalidatedDecorationIndexPathsSelector
  , contentOffsetAdjustmentSelector
  , setContentOffsetAdjustmentSelector
  , contentSizeAdjustmentSelector
  , setContentSizeAdjustmentSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- invalidateItemsAtIndexPaths:@
invalidateItemsAtIndexPaths :: (IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext, IsNSSet indexPaths) => nsCollectionViewLayoutInvalidationContext -> indexPaths -> IO ()
invalidateItemsAtIndexPaths nsCollectionViewLayoutInvalidationContext  indexPaths =
withObjCPtr indexPaths $ \raw_indexPaths ->
    sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidateItemsAtIndexPaths:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- invalidateSupplementaryElementsOfKind:atIndexPaths:@
invalidateSupplementaryElementsOfKind_atIndexPaths :: (IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext, IsNSString elementKind, IsNSSet indexPaths) => nsCollectionViewLayoutInvalidationContext -> elementKind -> indexPaths -> IO ()
invalidateSupplementaryElementsOfKind_atIndexPaths nsCollectionViewLayoutInvalidationContext  elementKind indexPaths =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidateSupplementaryElementsOfKind:atIndexPaths:") retVoid [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- invalidateDecorationElementsOfKind:atIndexPaths:@
invalidateDecorationElementsOfKind_atIndexPaths :: (IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext, IsNSString elementKind, IsNSSet indexPaths) => nsCollectionViewLayoutInvalidationContext -> elementKind -> indexPaths -> IO ()
invalidateDecorationElementsOfKind_atIndexPaths nsCollectionViewLayoutInvalidationContext  elementKind indexPaths =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidateDecorationElementsOfKind:atIndexPaths:") retVoid [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- invalidateEverything@
invalidateEverything :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO Bool
invalidateEverything nsCollectionViewLayoutInvalidationContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidateEverything") retCULong []

-- | @- invalidateDataSourceCounts@
invalidateDataSourceCounts :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO Bool
invalidateDataSourceCounts nsCollectionViewLayoutInvalidationContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidateDataSourceCounts") retCULong []

-- | @- invalidatedItemIndexPaths@
invalidatedItemIndexPaths :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO (Id NSSet)
invalidatedItemIndexPaths nsCollectionViewLayoutInvalidationContext  =
  sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidatedItemIndexPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- invalidatedSupplementaryIndexPaths@
invalidatedSupplementaryIndexPaths :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO (Id NSDictionary)
invalidatedSupplementaryIndexPaths nsCollectionViewLayoutInvalidationContext  =
  sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidatedSupplementaryIndexPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- invalidatedDecorationIndexPaths@
invalidatedDecorationIndexPaths :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO (Id NSDictionary)
invalidatedDecorationIndexPaths nsCollectionViewLayoutInvalidationContext  =
  sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "invalidatedDecorationIndexPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentOffsetAdjustment@
contentOffsetAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO NSPoint
contentOffsetAdjustment nsCollectionViewLayoutInvalidationContext  =
  sendMsgStret nsCollectionViewLayoutInvalidationContext (mkSelector "contentOffsetAdjustment") retNSPoint []

-- | @- setContentOffsetAdjustment:@
setContentOffsetAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> NSPoint -> IO ()
setContentOffsetAdjustment nsCollectionViewLayoutInvalidationContext  value =
  sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "setContentOffsetAdjustment:") retVoid [argNSPoint value]

-- | @- contentSizeAdjustment@
contentSizeAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> IO NSSize
contentSizeAdjustment nsCollectionViewLayoutInvalidationContext  =
  sendMsgStret nsCollectionViewLayoutInvalidationContext (mkSelector "contentSizeAdjustment") retNSSize []

-- | @- setContentSizeAdjustment:@
setContentSizeAdjustment :: IsNSCollectionViewLayoutInvalidationContext nsCollectionViewLayoutInvalidationContext => nsCollectionViewLayoutInvalidationContext -> NSSize -> IO ()
setContentSizeAdjustment nsCollectionViewLayoutInvalidationContext  value =
  sendMsg nsCollectionViewLayoutInvalidationContext (mkSelector "setContentSizeAdjustment:") retVoid [argNSSize value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateItemsAtIndexPaths:@
invalidateItemsAtIndexPathsSelector :: Selector
invalidateItemsAtIndexPathsSelector = mkSelector "invalidateItemsAtIndexPaths:"

-- | @Selector@ for @invalidateSupplementaryElementsOfKind:atIndexPaths:@
invalidateSupplementaryElementsOfKind_atIndexPathsSelector :: Selector
invalidateSupplementaryElementsOfKind_atIndexPathsSelector = mkSelector "invalidateSupplementaryElementsOfKind:atIndexPaths:"

-- | @Selector@ for @invalidateDecorationElementsOfKind:atIndexPaths:@
invalidateDecorationElementsOfKind_atIndexPathsSelector :: Selector
invalidateDecorationElementsOfKind_atIndexPathsSelector = mkSelector "invalidateDecorationElementsOfKind:atIndexPaths:"

-- | @Selector@ for @invalidateEverything@
invalidateEverythingSelector :: Selector
invalidateEverythingSelector = mkSelector "invalidateEverything"

-- | @Selector@ for @invalidateDataSourceCounts@
invalidateDataSourceCountsSelector :: Selector
invalidateDataSourceCountsSelector = mkSelector "invalidateDataSourceCounts"

-- | @Selector@ for @invalidatedItemIndexPaths@
invalidatedItemIndexPathsSelector :: Selector
invalidatedItemIndexPathsSelector = mkSelector "invalidatedItemIndexPaths"

-- | @Selector@ for @invalidatedSupplementaryIndexPaths@
invalidatedSupplementaryIndexPathsSelector :: Selector
invalidatedSupplementaryIndexPathsSelector = mkSelector "invalidatedSupplementaryIndexPaths"

-- | @Selector@ for @invalidatedDecorationIndexPaths@
invalidatedDecorationIndexPathsSelector :: Selector
invalidatedDecorationIndexPathsSelector = mkSelector "invalidatedDecorationIndexPaths"

-- | @Selector@ for @contentOffsetAdjustment@
contentOffsetAdjustmentSelector :: Selector
contentOffsetAdjustmentSelector = mkSelector "contentOffsetAdjustment"

-- | @Selector@ for @setContentOffsetAdjustment:@
setContentOffsetAdjustmentSelector :: Selector
setContentOffsetAdjustmentSelector = mkSelector "setContentOffsetAdjustment:"

-- | @Selector@ for @contentSizeAdjustment@
contentSizeAdjustmentSelector :: Selector
contentSizeAdjustmentSelector = mkSelector "contentSizeAdjustment"

-- | @Selector@ for @setContentSizeAdjustment:@
setContentSizeAdjustmentSelector :: Selector
setContentSizeAdjustmentSelector = mkSelector "setContentSizeAdjustment:"

