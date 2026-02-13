{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector
  , collapsesBordersSelector
  , drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector
  , hidesEmptyCellsSelector
  , layoutAlgorithmSelector
  , numberOfColumnsSelector
  , rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector
  , setCollapsesBordersSelector
  , setHidesEmptyCellsSelector
  , setLayoutAlgorithmSelector
  , setNumberOfColumnsSelector

  -- * Enum types
  , NSTextTableLayoutAlgorithm(NSTextTableLayoutAlgorithm)
  , pattern NSTextTableAutomaticLayoutAlgorithm
  , pattern NSTextTableFixedLayoutAlgorithm

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:@
rectForBlock_layoutAtPoint_inRect_textContainer_characterRange :: (IsNSTextTable nsTextTable, IsNSTextTableBlock block, IsNSTextContainer textContainer) => nsTextTable -> block -> NSPoint -> NSRect -> textContainer -> NSRange -> IO NSRect
rectForBlock_layoutAtPoint_inRect_textContainer_characterRange nsTextTable block startingPoint rect textContainer charRange =
  sendMessage nsTextTable rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector (toNSTextTableBlock block) startingPoint rect (toNSTextContainer textContainer) charRange

-- | @- boundsRectForBlock:contentRect:inRect:textContainer:characterRange:@
boundsRectForBlock_contentRect_inRect_textContainer_characterRange :: (IsNSTextTable nsTextTable, IsNSTextTableBlock block, IsNSTextContainer textContainer) => nsTextTable -> block -> NSRect -> NSRect -> textContainer -> NSRange -> IO NSRect
boundsRectForBlock_contentRect_inRect_textContainer_characterRange nsTextTable block contentRect rect textContainer charRange =
  sendMessage nsTextTable boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector (toNSTextTableBlock block) contentRect rect (toNSTextContainer textContainer) charRange

-- | @- drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:@
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager :: (IsNSTextTable nsTextTable, IsNSTextTableBlock block, IsNSView controlView, IsNSLayoutManager layoutManager) => nsTextTable -> block -> NSRect -> controlView -> NSRange -> layoutManager -> IO ()
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager nsTextTable block frameRect controlView charRange layoutManager =
  sendMessage nsTextTable drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector (toNSTextTableBlock block) frameRect (toNSView controlView) charRange (toNSLayoutManager layoutManager)

-- | @- numberOfColumns@
numberOfColumns :: IsNSTextTable nsTextTable => nsTextTable -> IO CULong
numberOfColumns nsTextTable =
  sendMessage nsTextTable numberOfColumnsSelector

-- | @- setNumberOfColumns:@
setNumberOfColumns :: IsNSTextTable nsTextTable => nsTextTable -> CULong -> IO ()
setNumberOfColumns nsTextTable value =
  sendMessage nsTextTable setNumberOfColumnsSelector value

-- | @- layoutAlgorithm@
layoutAlgorithm :: IsNSTextTable nsTextTable => nsTextTable -> IO NSTextTableLayoutAlgorithm
layoutAlgorithm nsTextTable =
  sendMessage nsTextTable layoutAlgorithmSelector

-- | @- setLayoutAlgorithm:@
setLayoutAlgorithm :: IsNSTextTable nsTextTable => nsTextTable -> NSTextTableLayoutAlgorithm -> IO ()
setLayoutAlgorithm nsTextTable value =
  sendMessage nsTextTable setLayoutAlgorithmSelector value

-- | @- collapsesBorders@
collapsesBorders :: IsNSTextTable nsTextTable => nsTextTable -> IO Bool
collapsesBorders nsTextTable =
  sendMessage nsTextTable collapsesBordersSelector

-- | @- setCollapsesBorders:@
setCollapsesBorders :: IsNSTextTable nsTextTable => nsTextTable -> Bool -> IO ()
setCollapsesBorders nsTextTable value =
  sendMessage nsTextTable setCollapsesBordersSelector value

-- | @- hidesEmptyCells@
hidesEmptyCells :: IsNSTextTable nsTextTable => nsTextTable -> IO Bool
hidesEmptyCells nsTextTable =
  sendMessage nsTextTable hidesEmptyCellsSelector

-- | @- setHidesEmptyCells:@
setHidesEmptyCells :: IsNSTextTable nsTextTable => nsTextTable -> Bool -> IO ()
setHidesEmptyCells nsTextTable value =
  sendMessage nsTextTable setHidesEmptyCellsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:@
rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector :: Selector '[Id NSTextTableBlock, NSPoint, NSRect, Id NSTextContainer, NSRange] NSRect
rectForBlock_layoutAtPoint_inRect_textContainer_characterRangeSelector = mkSelector "rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:"

-- | @Selector@ for @boundsRectForBlock:contentRect:inRect:textContainer:characterRange:@
boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector :: Selector '[Id NSTextTableBlock, NSRect, NSRect, Id NSTextContainer, NSRange] NSRect
boundsRectForBlock_contentRect_inRect_textContainer_characterRangeSelector = mkSelector "boundsRectForBlock:contentRect:inRect:textContainer:characterRange:"

-- | @Selector@ for @drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:@
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector :: Selector '[Id NSTextTableBlock, NSRect, Id NSView, NSRange, Id NSLayoutManager] ()
drawBackgroundForBlock_withFrame_inView_characterRange_layoutManagerSelector = mkSelector "drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CULong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setNumberOfColumns:@
setNumberOfColumnsSelector :: Selector '[CULong] ()
setNumberOfColumnsSelector = mkSelector "setNumberOfColumns:"

-- | @Selector@ for @layoutAlgorithm@
layoutAlgorithmSelector :: Selector '[] NSTextTableLayoutAlgorithm
layoutAlgorithmSelector = mkSelector "layoutAlgorithm"

-- | @Selector@ for @setLayoutAlgorithm:@
setLayoutAlgorithmSelector :: Selector '[NSTextTableLayoutAlgorithm] ()
setLayoutAlgorithmSelector = mkSelector "setLayoutAlgorithm:"

-- | @Selector@ for @collapsesBorders@
collapsesBordersSelector :: Selector '[] Bool
collapsesBordersSelector = mkSelector "collapsesBorders"

-- | @Selector@ for @setCollapsesBorders:@
setCollapsesBordersSelector :: Selector '[Bool] ()
setCollapsesBordersSelector = mkSelector "setCollapsesBorders:"

-- | @Selector@ for @hidesEmptyCells@
hidesEmptyCellsSelector :: Selector '[] Bool
hidesEmptyCellsSelector = mkSelector "hidesEmptyCells"

-- | @Selector@ for @setHidesEmptyCells:@
setHidesEmptyCellsSelector :: Selector '[Bool] ()
setHidesEmptyCellsSelector = mkSelector "setHidesEmptyCells:"

