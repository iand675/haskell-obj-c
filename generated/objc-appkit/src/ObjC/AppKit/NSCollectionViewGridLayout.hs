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
  , marginsSelector
  , setMarginsSelector
  , minimumInteritemSpacingSelector
  , setMinimumInteritemSpacingSelector
  , minimumLineSpacingSelector
  , setMinimumLineSpacingSelector
  , maximumNumberOfRowsSelector
  , setMaximumNumberOfRowsSelector
  , maximumNumberOfColumnsSelector
  , setMaximumNumberOfColumnsSelector
  , minimumItemSizeSelector
  , setMinimumItemSizeSelector
  , maximumItemSizeSelector
  , setMaximumItemSizeSelector
  , backgroundColorsSelector
  , setBackgroundColorsSelector


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

-- | @- margins@
margins :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO NSEdgeInsets
margins nsCollectionViewGridLayout  =
  sendMsgStret nsCollectionViewGridLayout (mkSelector "margins") retNSEdgeInsets []

-- | @- setMargins:@
setMargins :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> NSEdgeInsets -> IO ()
setMargins nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMargins:") retVoid [argNSEdgeInsets value]

-- | @- minimumInteritemSpacing@
minimumInteritemSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CDouble
minimumInteritemSpacing nsCollectionViewGridLayout  =
  sendMsg nsCollectionViewGridLayout (mkSelector "minimumInteritemSpacing") retCDouble []

-- | @- setMinimumInteritemSpacing:@
setMinimumInteritemSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CDouble -> IO ()
setMinimumInteritemSpacing nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMinimumInteritemSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- minimumLineSpacing@
minimumLineSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CDouble
minimumLineSpacing nsCollectionViewGridLayout  =
  sendMsg nsCollectionViewGridLayout (mkSelector "minimumLineSpacing") retCDouble []

-- | @- setMinimumLineSpacing:@
setMinimumLineSpacing :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CDouble -> IO ()
setMinimumLineSpacing nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMinimumLineSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumNumberOfRows@
maximumNumberOfRows :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CULong
maximumNumberOfRows nsCollectionViewGridLayout  =
  sendMsg nsCollectionViewGridLayout (mkSelector "maximumNumberOfRows") retCULong []

-- | @- setMaximumNumberOfRows:@
setMaximumNumberOfRows :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CULong -> IO ()
setMaximumNumberOfRows nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMaximumNumberOfRows:") retVoid [argCULong (fromIntegral value)]

-- | @- maximumNumberOfColumns@
maximumNumberOfColumns :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO CULong
maximumNumberOfColumns nsCollectionViewGridLayout  =
  sendMsg nsCollectionViewGridLayout (mkSelector "maximumNumberOfColumns") retCULong []

-- | @- setMaximumNumberOfColumns:@
setMaximumNumberOfColumns :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> CULong -> IO ()
setMaximumNumberOfColumns nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMaximumNumberOfColumns:") retVoid [argCULong (fromIntegral value)]

-- | @- minimumItemSize@
minimumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO NSSize
minimumItemSize nsCollectionViewGridLayout  =
  sendMsgStret nsCollectionViewGridLayout (mkSelector "minimumItemSize") retNSSize []

-- | @- setMinimumItemSize:@
setMinimumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> NSSize -> IO ()
setMinimumItemSize nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMinimumItemSize:") retVoid [argNSSize value]

-- | @- maximumItemSize@
maximumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO NSSize
maximumItemSize nsCollectionViewGridLayout  =
  sendMsgStret nsCollectionViewGridLayout (mkSelector "maximumItemSize") retNSSize []

-- | @- setMaximumItemSize:@
setMaximumItemSize :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> NSSize -> IO ()
setMaximumItemSize nsCollectionViewGridLayout  value =
  sendMsg nsCollectionViewGridLayout (mkSelector "setMaximumItemSize:") retVoid [argNSSize value]

-- | @- backgroundColors@
backgroundColors :: IsNSCollectionViewGridLayout nsCollectionViewGridLayout => nsCollectionViewGridLayout -> IO (Id NSArray)
backgroundColors nsCollectionViewGridLayout  =
  sendMsg nsCollectionViewGridLayout (mkSelector "backgroundColors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColors:@
setBackgroundColors :: (IsNSCollectionViewGridLayout nsCollectionViewGridLayout, IsNSArray value) => nsCollectionViewGridLayout -> value -> IO ()
setBackgroundColors nsCollectionViewGridLayout  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionViewGridLayout (mkSelector "setBackgroundColors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @margins@
marginsSelector :: Selector
marginsSelector = mkSelector "margins"

-- | @Selector@ for @setMargins:@
setMarginsSelector :: Selector
setMarginsSelector = mkSelector "setMargins:"

-- | @Selector@ for @minimumInteritemSpacing@
minimumInteritemSpacingSelector :: Selector
minimumInteritemSpacingSelector = mkSelector "minimumInteritemSpacing"

-- | @Selector@ for @setMinimumInteritemSpacing:@
setMinimumInteritemSpacingSelector :: Selector
setMinimumInteritemSpacingSelector = mkSelector "setMinimumInteritemSpacing:"

-- | @Selector@ for @minimumLineSpacing@
minimumLineSpacingSelector :: Selector
minimumLineSpacingSelector = mkSelector "minimumLineSpacing"

-- | @Selector@ for @setMinimumLineSpacing:@
setMinimumLineSpacingSelector :: Selector
setMinimumLineSpacingSelector = mkSelector "setMinimumLineSpacing:"

-- | @Selector@ for @maximumNumberOfRows@
maximumNumberOfRowsSelector :: Selector
maximumNumberOfRowsSelector = mkSelector "maximumNumberOfRows"

-- | @Selector@ for @setMaximumNumberOfRows:@
setMaximumNumberOfRowsSelector :: Selector
setMaximumNumberOfRowsSelector = mkSelector "setMaximumNumberOfRows:"

-- | @Selector@ for @maximumNumberOfColumns@
maximumNumberOfColumnsSelector :: Selector
maximumNumberOfColumnsSelector = mkSelector "maximumNumberOfColumns"

-- | @Selector@ for @setMaximumNumberOfColumns:@
setMaximumNumberOfColumnsSelector :: Selector
setMaximumNumberOfColumnsSelector = mkSelector "setMaximumNumberOfColumns:"

-- | @Selector@ for @minimumItemSize@
minimumItemSizeSelector :: Selector
minimumItemSizeSelector = mkSelector "minimumItemSize"

-- | @Selector@ for @setMinimumItemSize:@
setMinimumItemSizeSelector :: Selector
setMinimumItemSizeSelector = mkSelector "setMinimumItemSize:"

-- | @Selector@ for @maximumItemSize@
maximumItemSizeSelector :: Selector
maximumItemSizeSelector = mkSelector "maximumItemSize"

-- | @Selector@ for @setMaximumItemSize:@
setMaximumItemSizeSelector :: Selector
setMaximumItemSizeSelector = mkSelector "setMaximumItemSize:"

-- | @Selector@ for @backgroundColors@
backgroundColorsSelector :: Selector
backgroundColorsSelector = mkSelector "backgroundColors"

-- | @Selector@ for @setBackgroundColors:@
setBackgroundColorsSelector :: Selector
setBackgroundColorsSelector = mkSelector "setBackgroundColors:"

