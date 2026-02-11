{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextTable@.
module ObjC.AppKit.NSTextTable
  ( NSTextTable
  , IsNSTextTable(..)
  , rectForBlock_layoutAtPoint_inRect_textContainer_characterRange
  , boundsRectForBlock_contentRect_inRect_textContainer_characterRange
  , drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager
  , numberOfColumns
  , setNumberOfColumns
  , layoutAlgorithm
  , setLayoutAlgorithm
  , collapsesBorders
  , setCollapsesBorders
  , hidesEmptyCells
  , setHidesEmptyCells
  , rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector
  , boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector
  , drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector
  , numberOfColumnsSelector
  , setNumberOfColumnsSelector
  , layoutAlgorithmSelector
  , setLayoutAlgorithmSelector
  , collapsesBordersSelector
  , setCollapsesBordersSelector
  , hidesEmptyCellsSelector
  , setHidesEmptyCellsSelector

  -- * Enum types
  , NSTextTableLayoutAlgorithm(NSTextTableLayoutAlgorithm)
  , pattern NSTextTableAutomaticLayoutAlgorithm
  , pattern NSTextTableFixedLayoutAlgorithm

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:@
rectForBlock_layoutAtPoint_inRect_textContainer_characterRange :: (IsNSTextTable nsTextTable, IsNSTextTableBlock block, IsNSTextContainer textContainer) => nsTextTable -> block -> NSPoint -> NSRect -> textContainer -> NSRange -> IO NSRect
rectForBlock_layoutAtPoint_inRect_textContainer_characterRange nsTextTable  block startingPoint rect textContainer charRange =
withObjCPtr block $ \raw_block ->
  withObjCPtr textContainer $ \raw_textContainer ->
      sendMsgStret nsTextTable (mkSelector "rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:") retNSRect [argPtr (castPtr raw_block :: Ptr ()), argNSPoint startingPoint, argNSRect rect, argPtr (castPtr raw_textContainer :: Ptr ()), argNSRange charRange]

-- | @- boundsRectForBlock:contentRect:inRect:textContainer:characterRange:@
boundsRectForBlock_contentRect_inRect_textContainer_characterRange :: (IsNSTextTable nsTextTable, IsNSTextTableBlock block, IsNSTextContainer textContainer) => nsTextTable -> block -> NSRect -> NSRect -> textContainer -> NSRange -> IO NSRect
boundsRectForBlock_contentRect_inRect_textContainer_characterRange nsTextTable  block contentRect rect textContainer charRange =
withObjCPtr block $ \raw_block ->
  withObjCPtr textContainer $ \raw_textContainer ->
      sendMsgStret nsTextTable (mkSelector "boundsRectForBlock:contentRect:inRect:textContainer:characterRange:") retNSRect [argPtr (castPtr raw_block :: Ptr ()), argNSRect contentRect, argNSRect rect, argPtr (castPtr raw_textContainer :: Ptr ()), argNSRange charRange]

-- | @- drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:@
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager :: (IsNSTextTable nsTextTable, IsNSTextTableBlock block, IsNSView controlView, IsNSLayoutManager layoutManager) => nsTextTable -> block -> NSRect -> controlView -> NSRange -> layoutManager -> IO ()
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager nsTextTable  block frameRect controlView charRange layoutManager =
withObjCPtr block $ \raw_block ->
  withObjCPtr controlView $ \raw_controlView ->
    withObjCPtr layoutManager $ \raw_layoutManager ->
        sendMsg nsTextTable (mkSelector "drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:") retVoid [argPtr (castPtr raw_block :: Ptr ()), argNSRect frameRect, argPtr (castPtr raw_controlView :: Ptr ()), argNSRange charRange, argPtr (castPtr raw_layoutManager :: Ptr ())]

-- | @- numberOfColumns@
numberOfColumns :: IsNSTextTable nsTextTable => nsTextTable -> IO CULong
numberOfColumns nsTextTable  =
  sendMsg nsTextTable (mkSelector "numberOfColumns") retCULong []

-- | @- setNumberOfColumns:@
setNumberOfColumns :: IsNSTextTable nsTextTable => nsTextTable -> CULong -> IO ()
setNumberOfColumns nsTextTable  value =
  sendMsg nsTextTable (mkSelector "setNumberOfColumns:") retVoid [argCULong (fromIntegral value)]

-- | @- layoutAlgorithm@
layoutAlgorithm :: IsNSTextTable nsTextTable => nsTextTable -> IO NSTextTableLayoutAlgorithm
layoutAlgorithm nsTextTable  =
  fmap (coerce :: CULong -> NSTextTableLayoutAlgorithm) $ sendMsg nsTextTable (mkSelector "layoutAlgorithm") retCULong []

-- | @- setLayoutAlgorithm:@
setLayoutAlgorithm :: IsNSTextTable nsTextTable => nsTextTable -> NSTextTableLayoutAlgorithm -> IO ()
setLayoutAlgorithm nsTextTable  value =
  sendMsg nsTextTable (mkSelector "setLayoutAlgorithm:") retVoid [argCULong (coerce value)]

-- | @- collapsesBorders@
collapsesBorders :: IsNSTextTable nsTextTable => nsTextTable -> IO Bool
collapsesBorders nsTextTable  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextTable (mkSelector "collapsesBorders") retCULong []

-- | @- setCollapsesBorders:@
setCollapsesBorders :: IsNSTextTable nsTextTable => nsTextTable -> Bool -> IO ()
setCollapsesBorders nsTextTable  value =
  sendMsg nsTextTable (mkSelector "setCollapsesBorders:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hidesEmptyCells@
hidesEmptyCells :: IsNSTextTable nsTextTable => nsTextTable -> IO Bool
hidesEmptyCells nsTextTable  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextTable (mkSelector "hidesEmptyCells") retCULong []

-- | @- setHidesEmptyCells:@
setHidesEmptyCells :: IsNSTextTable nsTextTable => nsTextTable -> Bool -> IO ()
setHidesEmptyCells nsTextTable  value =
  sendMsg nsTextTable (mkSelector "setHidesEmptyCells:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:@
rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector :: Selector
rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector = mkSelector "rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:"

-- | @Selector@ for @boundsRectForBlock:contentRect:inRect:textContainer:characterRange:@
boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector :: Selector
boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector = mkSelector "boundsRectForBlock:contentRect:inRect:textContainer:characterRange:"

-- | @Selector@ for @drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:@
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector :: Selector
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector = mkSelector "drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setNumberOfColumns:@
setNumberOfColumnsSelector :: Selector
setNumberOfColumnsSelector = mkSelector "setNumberOfColumns:"

-- | @Selector@ for @layoutAlgorithm@
layoutAlgorithmSelector :: Selector
layoutAlgorithmSelector = mkSelector "layoutAlgorithm"

-- | @Selector@ for @setLayoutAlgorithm:@
setLayoutAlgorithmSelector :: Selector
setLayoutAlgorithmSelector = mkSelector "setLayoutAlgorithm:"

-- | @Selector@ for @collapsesBorders@
collapsesBordersSelector :: Selector
collapsesBordersSelector = mkSelector "collapsesBorders"

-- | @Selector@ for @setCollapsesBorders:@
setCollapsesBordersSelector :: Selector
setCollapsesBordersSelector = mkSelector "setCollapsesBorders:"

-- | @Selector@ for @hidesEmptyCells@
hidesEmptyCellsSelector :: Selector
hidesEmptyCellsSelector = mkSelector "hidesEmptyCells"

-- | @Selector@ for @setHidesEmptyCells:@
setHidesEmptyCellsSelector :: Selector
setHidesEmptyCellsSelector = mkSelector "setHidesEmptyCells:"

