{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewGridLayout@.
module ObjC.AppKit.NSCollectionViewGridLayout
  ( NSCollectionViewGridLayout
  , IsNSCollectionViewGridLayout(..)
  , margins
  , setMargins
  , minimumInteritemSpacing
  , setMinimumInteritemSpacing
  , minimumLineSpacing
  , setMinimumLineSpacing
  , maximumNumberOfRows
  , setMaximumNumberOfRows
  , maximumNumberOfColumns
  , setMaximumNumberOfColumns
  , minimumItemSize
  , setMinimumItemSize
  , maximumItemSize
  , setMaximumItemSize
  , backgroundColors
  , setBackgroundColors
  , backgroundColorsSelector
  , marginsSelector
  , maximumItemSizeSelector
  , maximumNumberOfColumnsSelector
  , maximumNumberOfRowsSelector
  , minimumInteritemSpacingSelector
  , minimumItemSizeSelector
  , minimumLineSpacingSelector
  , setBackgroundColorsSelector
  , setMarginsSelector
  , setMaximumItemSizeSelector
  , setMaximumNumberOfColumnsSelector
  , setMaximumNumberOfRowsSelector
  , setMinimumInteritemSpacingSelector
  , setMinimumItemSizeSelector
  , setMinimumLineSpacingSelector


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

-- | @- margins@
margins :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO NSEdgeInsets
margins nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout marginsSelector

-- | @- setMargins:@
setMargins :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> NSEdgeInsets -> IO ()
setMargins nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMarginsSelector value

-- | @- minimumInteritemSpacing@
minimumInteritemSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CDouble
minimumInteritemSpacing nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout minimumInteritemSpacingSelector

-- | @- setMinimumInteritemSpacing:@
setMinimumInteritemSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CDouble -> IO ()
setMinimumInteritemSpacing nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMinimumInteritemSpacingSelector value

-- | @- minimumLineSpacing@
minimumLineSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CDouble
minimumLineSpacing nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout minimumLineSpacingSelector

-- | @- setMinimumLineSpacing:@
setMinimumLineSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CDouble -> IO ()
setMinimumLineSpacing nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMinimumLineSpacingSelector value

-- | @- maximumNumberOfRows@
maximumNumberOfRows :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CULong
maximumNumberOfRows nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout maximumNumberOfRowsSelector

-- | @- setMaximumNumberOfRows:@
setMaximumNumberOfRows :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CULong -> IO ()
setMaximumNumberOfRows nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMaximumNumberOfRowsSelector value

-- | @- maximumNumberOfColumns@
maximumNumberOfColumns :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CULong
maximumNumberOfColumns nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout maximumNumberOfColumnsSelector

-- | @- setMaximumNumberOfColumns:@
setMaximumNumberOfColumns :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CULong -> IO ()
setMaximumNumberOfColumns nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMaximumNumberOfColumnsSelector value

-- | @- minimumItemSize@
minimumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO NSSize
minimumItemSize nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout minimumItemSizeSelector

-- | @- setMinimumItemSize:@
setMinimumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> NSSize -> IO ()
setMinimumItemSize nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMinimumItemSizeSelector value

-- | @- maximumItemSize@
maximumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO NSSize
maximumItemSize nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout maximumItemSizeSelector

-- | @- setMaximumItemSize:@
setMaximumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> NSSize -> IO ()
setMaximumItemSize nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setMaximumItemSizeSelector value

-- | @- backgroundColors@
backgroundColors :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO (Id NSArray)
backgroundColors nsCollectionViewGridLayout =
  sendMessage nsCollectionViewGridLayout backgroundColorsSelector

-- | @- setBackgroundColors:@
setBackgroundColors :: (IsNSCollectionViewGridLayout nsCollectionViewGridLayout, IsNSArray value) => nsCollectionViewGridLayout -> value -> IO ()
setBackgroundColors nsCollectionViewGridLayout value =
  sendMessage nsCollectionViewGridLayout setBackgroundColorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @margins@
marginsSelector :: Selector '[] NSEdgeInsets
marginsSelector = mkSelector "margins"

-- | @Selector@ for @setMargins:@
setMarginsSelector :: Selector '[NSEdgeInsets] ()
setMarginsSelector = mkSelector "setMargins:"

-- | @Selector@ for @minimumInteritemSpacing@
minimumInteritemSpacingSelector :: Selector '[] CDouble
minimumInteritemSpacingSelector = mkSelector "minimumInteritemSpacing"

-- | @Selector@ for @setMinimumInteritemSpacing:@
setMinimumInteritemSpacingSelector :: Selector '[CDouble] ()
setMinimumInteritemSpacingSelector = mkSelector "setMinimumInteritemSpacing:"

-- | @Selector@ for @minimumLineSpacing@
minimumLineSpacingSelector :: Selector '[] CDouble
minimumLineSpacingSelector = mkSelector "minimumLineSpacing"

-- | @Selector@ for @setMinimumLineSpacing:@
setMinimumLineSpacingSelector :: Selector '[CDouble] ()
setMinimumLineSpacingSelector = mkSelector "setMinimumLineSpacing:"

-- | @Selector@ for @maximumNumberOfRows@
maximumNumberOfRowsSelector :: Selector '[] CULong
maximumNumberOfRowsSelector = mkSelector "maximumNumberOfRows"

-- | @Selector@ for @setMaximumNumberOfRows:@
setMaximumNumberOfRowsSelector :: Selector '[CULong] ()
setMaximumNumberOfRowsSelector = mkSelector "setMaximumNumberOfRows:"

-- | @Selector@ for @maximumNumberOfColumns@
maximumNumberOfColumnsSelector :: Selector '[] CULong
maximumNumberOfColumnsSelector = mkSelector "maximumNumberOfColumns"

-- | @Selector@ for @setMaximumNumberOfColumns:@
setMaximumNumberOfColumnsSelector :: Selector '[CULong] ()
setMaximumNumberOfColumnsSelector = mkSelector "setMaximumNumberOfColumns:"

-- | @Selector@ for @minimumItemSize@
minimumItemSizeSelector :: Selector '[] NSSize
minimumItemSizeSelector = mkSelector "minimumItemSize"

-- | @Selector@ for @setMinimumItemSize:@
setMinimumItemSizeSelector :: Selector '[NSSize] ()
setMinimumItemSizeSelector = mkSelector "setMinimumItemSize:"

-- | @Selector@ for @maximumItemSize@
maximumItemSizeSelector :: Selector '[] NSSize
maximumItemSizeSelector = mkSelector "maximumItemSize"

-- | @Selector@ for @setMaximumItemSize:@
setMaximumItemSizeSelector :: Selector '[NSSize] ()
setMaximumItemSizeSelector = mkSelector "setMaximumItemSize:"

-- | @Selector@ for @backgroundColors@
backgroundColorsSelector :: Selector '[] (Id NSArray)
backgroundColorsSelector = mkSelector "backgroundColors"

-- | @Selector@ for @setBackgroundColors:@
setBackgroundColorsSelector :: Selector '[Id NSArray] ()
setBackgroundColorsSelector = mkSelector "setBackgroundColors:"

