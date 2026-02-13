{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTableElement@.
module ObjC.WebKit.DOMHTMLTableElement
  ( DOMHTMLTableElement
  , IsDOMHTMLTableElement(..)
  , createTHead
  , deleteTHead
  , createTFoot
  , deleteTFoot
  , createCaption
  , deleteCaption
  , insertRow
  , deleteRow
  , caption
  , setCaption
  , tHead
  , setTHead
  , tFoot
  , setTFoot
  , rows
  , tBodies
  , align
  , setAlign
  , bgColor
  , setBgColor
  , border
  , setBorder
  , cellPadding
  , setCellPadding
  , cellSpacing
  , setCellSpacing
  , frameBorders
  , setFrameBorders
  , rules
  , setRules
  , summary
  , setSummary
  , width
  , setWidth
  , alignSelector
  , bgColorSelector
  , borderSelector
  , captionSelector
  , cellPaddingSelector
  , cellSpacingSelector
  , createCaptionSelector
  , createTFootSelector
  , createTHeadSelector
  , deleteCaptionSelector
  , deleteRowSelector
  , deleteTFootSelector
  , deleteTHeadSelector
  , frameBordersSelector
  , insertRowSelector
  , rowsSelector
  , rulesSelector
  , setAlignSelector
  , setBgColorSelector
  , setBorderSelector
  , setCaptionSelector
  , setCellPaddingSelector
  , setCellSpacingSelector
  , setFrameBordersSelector
  , setRulesSelector
  , setSummarySelector
  , setTFootSelector
  , setTHeadSelector
  , setWidthSelector
  , summarySelector
  , tBodiesSelector
  , tFootSelector
  , tHeadSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createTHead@
createTHead :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLElement)
createTHead domhtmlTableElement =
  sendMessage domhtmlTableElement createTHeadSelector

-- | @- deleteTHead@
deleteTHead :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO ()
deleteTHead domhtmlTableElement =
  sendMessage domhtmlTableElement deleteTHeadSelector

-- | @- createTFoot@
createTFoot :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLElement)
createTFoot domhtmlTableElement =
  sendMessage domhtmlTableElement createTFootSelector

-- | @- deleteTFoot@
deleteTFoot :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO ()
deleteTFoot domhtmlTableElement =
  sendMessage domhtmlTableElement deleteTFootSelector

-- | @- createCaption@
createCaption :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLElement)
createCaption domhtmlTableElement =
  sendMessage domhtmlTableElement createCaptionSelector

-- | @- deleteCaption@
deleteCaption :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO ()
deleteCaption domhtmlTableElement =
  sendMessage domhtmlTableElement deleteCaptionSelector

-- | @- insertRow:@
insertRow :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> CInt -> IO (Id DOMHTMLElement)
insertRow domhtmlTableElement index =
  sendMessage domhtmlTableElement insertRowSelector index

-- | @- deleteRow:@
deleteRow :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> CInt -> IO ()
deleteRow domhtmlTableElement index =
  sendMessage domhtmlTableElement deleteRowSelector index

-- | @- caption@
caption :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLTableCaptionElement)
caption domhtmlTableElement =
  sendMessage domhtmlTableElement captionSelector

-- | @- setCaption:@
setCaption :: (IsDOMHTMLTableElement domhtmlTableElement, IsDOMHTMLTableCaptionElement value) => domhtmlTableElement -> value -> IO ()
setCaption domhtmlTableElement value =
  sendMessage domhtmlTableElement setCaptionSelector (toDOMHTMLTableCaptionElement value)

-- | @- tHead@
tHead :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLTableSectionElement)
tHead domhtmlTableElement =
  sendMessage domhtmlTableElement tHeadSelector

-- | @- setTHead:@
setTHead :: (IsDOMHTMLTableElement domhtmlTableElement, IsDOMHTMLTableSectionElement value) => domhtmlTableElement -> value -> IO ()
setTHead domhtmlTableElement value =
  sendMessage domhtmlTableElement setTHeadSelector (toDOMHTMLTableSectionElement value)

-- | @- tFoot@
tFoot :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLTableSectionElement)
tFoot domhtmlTableElement =
  sendMessage domhtmlTableElement tFootSelector

-- | @- setTFoot:@
setTFoot :: (IsDOMHTMLTableElement domhtmlTableElement, IsDOMHTMLTableSectionElement value) => domhtmlTableElement -> value -> IO ()
setTFoot domhtmlTableElement value =
  sendMessage domhtmlTableElement setTFootSelector (toDOMHTMLTableSectionElement value)

-- | @- rows@
rows :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLCollection)
rows domhtmlTableElement =
  sendMessage domhtmlTableElement rowsSelector

-- | @- tBodies@
tBodies :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLCollection)
tBodies domhtmlTableElement =
  sendMessage domhtmlTableElement tBodiesSelector

-- | @- align@
align :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
align domhtmlTableElement =
  sendMessage domhtmlTableElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setAlign domhtmlTableElement value =
  sendMessage domhtmlTableElement setAlignSelector (toNSString value)

-- | @- bgColor@
bgColor :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
bgColor domhtmlTableElement =
  sendMessage domhtmlTableElement bgColorSelector

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setBgColor domhtmlTableElement value =
  sendMessage domhtmlTableElement setBgColorSelector (toNSString value)

-- | @- border@
border :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
border domhtmlTableElement =
  sendMessage domhtmlTableElement borderSelector

-- | @- setBorder:@
setBorder :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setBorder domhtmlTableElement value =
  sendMessage domhtmlTableElement setBorderSelector (toNSString value)

-- | @- cellPadding@
cellPadding :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
cellPadding domhtmlTableElement =
  sendMessage domhtmlTableElement cellPaddingSelector

-- | @- setCellPadding:@
setCellPadding :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setCellPadding domhtmlTableElement value =
  sendMessage domhtmlTableElement setCellPaddingSelector (toNSString value)

-- | @- cellSpacing@
cellSpacing :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
cellSpacing domhtmlTableElement =
  sendMessage domhtmlTableElement cellSpacingSelector

-- | @- setCellSpacing:@
setCellSpacing :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setCellSpacing domhtmlTableElement value =
  sendMessage domhtmlTableElement setCellSpacingSelector (toNSString value)

-- | @- frameBorders@
frameBorders :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
frameBorders domhtmlTableElement =
  sendMessage domhtmlTableElement frameBordersSelector

-- | @- setFrameBorders:@
setFrameBorders :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setFrameBorders domhtmlTableElement value =
  sendMessage domhtmlTableElement setFrameBordersSelector (toNSString value)

-- | @- rules@
rules :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
rules domhtmlTableElement =
  sendMessage domhtmlTableElement rulesSelector

-- | @- setRules:@
setRules :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setRules domhtmlTableElement value =
  sendMessage domhtmlTableElement setRulesSelector (toNSString value)

-- | @- summary@
summary :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
summary domhtmlTableElement =
  sendMessage domhtmlTableElement summarySelector

-- | @- setSummary:@
setSummary :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setSummary domhtmlTableElement value =
  sendMessage domhtmlTableElement setSummarySelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
width domhtmlTableElement =
  sendMessage domhtmlTableElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setWidth domhtmlTableElement value =
  sendMessage domhtmlTableElement setWidthSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createTHead@
createTHeadSelector :: Selector '[] (Id DOMHTMLElement)
createTHeadSelector = mkSelector "createTHead"

-- | @Selector@ for @deleteTHead@
deleteTHeadSelector :: Selector '[] ()
deleteTHeadSelector = mkSelector "deleteTHead"

-- | @Selector@ for @createTFoot@
createTFootSelector :: Selector '[] (Id DOMHTMLElement)
createTFootSelector = mkSelector "createTFoot"

-- | @Selector@ for @deleteTFoot@
deleteTFootSelector :: Selector '[] ()
deleteTFootSelector = mkSelector "deleteTFoot"

-- | @Selector@ for @createCaption@
createCaptionSelector :: Selector '[] (Id DOMHTMLElement)
createCaptionSelector = mkSelector "createCaption"

-- | @Selector@ for @deleteCaption@
deleteCaptionSelector :: Selector '[] ()
deleteCaptionSelector = mkSelector "deleteCaption"

-- | @Selector@ for @insertRow:@
insertRowSelector :: Selector '[CInt] (Id DOMHTMLElement)
insertRowSelector = mkSelector "insertRow:"

-- | @Selector@ for @deleteRow:@
deleteRowSelector :: Selector '[CInt] ()
deleteRowSelector = mkSelector "deleteRow:"

-- | @Selector@ for @caption@
captionSelector :: Selector '[] (Id DOMHTMLTableCaptionElement)
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector '[Id DOMHTMLTableCaptionElement] ()
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @tHead@
tHeadSelector :: Selector '[] (Id DOMHTMLTableSectionElement)
tHeadSelector = mkSelector "tHead"

-- | @Selector@ for @setTHead:@
setTHeadSelector :: Selector '[Id DOMHTMLTableSectionElement] ()
setTHeadSelector = mkSelector "setTHead:"

-- | @Selector@ for @tFoot@
tFootSelector :: Selector '[] (Id DOMHTMLTableSectionElement)
tFootSelector = mkSelector "tFoot"

-- | @Selector@ for @setTFoot:@
setTFootSelector :: Selector '[Id DOMHTMLTableSectionElement] ()
setTFootSelector = mkSelector "setTFoot:"

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] (Id DOMHTMLCollection)
rowsSelector = mkSelector "rows"

-- | @Selector@ for @tBodies@
tBodiesSelector :: Selector '[] (Id DOMHTMLCollection)
tBodiesSelector = mkSelector "tBodies"

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector '[] (Id NSString)
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector '[Id NSString] ()
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @border@
borderSelector :: Selector '[] (Id NSString)
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector '[Id NSString] ()
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @cellPadding@
cellPaddingSelector :: Selector '[] (Id NSString)
cellPaddingSelector = mkSelector "cellPadding"

-- | @Selector@ for @setCellPadding:@
setCellPaddingSelector :: Selector '[Id NSString] ()
setCellPaddingSelector = mkSelector "setCellPadding:"

-- | @Selector@ for @cellSpacing@
cellSpacingSelector :: Selector '[] (Id NSString)
cellSpacingSelector = mkSelector "cellSpacing"

-- | @Selector@ for @setCellSpacing:@
setCellSpacingSelector :: Selector '[Id NSString] ()
setCellSpacingSelector = mkSelector "setCellSpacing:"

-- | @Selector@ for @frameBorders@
frameBordersSelector :: Selector '[] (Id NSString)
frameBordersSelector = mkSelector "frameBorders"

-- | @Selector@ for @setFrameBorders:@
setFrameBordersSelector :: Selector '[Id NSString] ()
setFrameBordersSelector = mkSelector "setFrameBorders:"

-- | @Selector@ for @rules@
rulesSelector :: Selector '[] (Id NSString)
rulesSelector = mkSelector "rules"

-- | @Selector@ for @setRules:@
setRulesSelector :: Selector '[Id NSString] ()
setRulesSelector = mkSelector "setRules:"

-- | @Selector@ for @summary@
summarySelector :: Selector '[] (Id NSString)
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector '[Id NSString] ()
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSString)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSString] ()
setWidthSelector = mkSelector "setWidth:"

