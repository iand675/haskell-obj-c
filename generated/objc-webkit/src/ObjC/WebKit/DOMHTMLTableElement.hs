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
  , createTHeadSelector
  , deleteTHeadSelector
  , createTFootSelector
  , deleteTFootSelector
  , createCaptionSelector
  , deleteCaptionSelector
  , insertRowSelector
  , deleteRowSelector
  , captionSelector
  , setCaptionSelector
  , tHeadSelector
  , setTHeadSelector
  , tFootSelector
  , setTFootSelector
  , rowsSelector
  , tBodiesSelector
  , alignSelector
  , setAlignSelector
  , bgColorSelector
  , setBgColorSelector
  , borderSelector
  , setBorderSelector
  , cellPaddingSelector
  , setCellPaddingSelector
  , cellSpacingSelector
  , setCellSpacingSelector
  , frameBordersSelector
  , setFrameBordersSelector
  , rulesSelector
  , setRulesSelector
  , summarySelector
  , setSummarySelector
  , widthSelector
  , setWidthSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createTHead@
createTHead :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLElement)
createTHead domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "createTHead") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deleteTHead@
deleteTHead :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO ()
deleteTHead domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "deleteTHead") retVoid []

-- | @- createTFoot@
createTFoot :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLElement)
createTFoot domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "createTFoot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deleteTFoot@
deleteTFoot :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO ()
deleteTFoot domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "deleteTFoot") retVoid []

-- | @- createCaption@
createCaption :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLElement)
createCaption domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "createCaption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deleteCaption@
deleteCaption :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO ()
deleteCaption domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "deleteCaption") retVoid []

-- | @- insertRow:@
insertRow :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> CInt -> IO (Id DOMHTMLElement)
insertRow domhtmlTableElement  index =
  sendMsg domhtmlTableElement (mkSelector "insertRow:") (retPtr retVoid) [argCInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- deleteRow:@
deleteRow :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> CInt -> IO ()
deleteRow domhtmlTableElement  index =
  sendMsg domhtmlTableElement (mkSelector "deleteRow:") retVoid [argCInt (fromIntegral index)]

-- | @- caption@
caption :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLTableCaptionElement)
caption domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "caption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaption:@
setCaption :: (IsDOMHTMLTableElement domhtmlTableElement, IsDOMHTMLTableCaptionElement value) => domhtmlTableElement -> value -> IO ()
setCaption domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setCaption:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tHead@
tHead :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLTableSectionElement)
tHead domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "tHead") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTHead:@
setTHead :: (IsDOMHTMLTableElement domhtmlTableElement, IsDOMHTMLTableSectionElement value) => domhtmlTableElement -> value -> IO ()
setTHead domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setTHead:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tFoot@
tFoot :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLTableSectionElement)
tFoot domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "tFoot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTFoot:@
setTFoot :: (IsDOMHTMLTableElement domhtmlTableElement, IsDOMHTMLTableSectionElement value) => domhtmlTableElement -> value -> IO ()
setTFoot domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setTFoot:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rows@
rows :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLCollection)
rows domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "rows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tBodies@
tBodies :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id DOMHTMLCollection)
tBodies domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "tBodies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- align@
align :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
align domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setAlign domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bgColor@
bgColor :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
bgColor domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "bgColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setBgColor domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setBgColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- border@
border :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
border domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "border") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorder:@
setBorder :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setBorder domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setBorder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cellPadding@
cellPadding :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
cellPadding domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "cellPadding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCellPadding:@
setCellPadding :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setCellPadding domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setCellPadding:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cellSpacing@
cellSpacing :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
cellSpacing domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "cellSpacing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCellSpacing:@
setCellSpacing :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setCellSpacing domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setCellSpacing:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- frameBorders@
frameBorders :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
frameBorders domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "frameBorders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrameBorders:@
setFrameBorders :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setFrameBorders domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setFrameBorders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rules@
rules :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
rules domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "rules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRules:@
setRules :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setRules domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- summary@
summary :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
summary domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "summary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSummary:@
setSummary :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setSummary domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setSummary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLTableElement domhtmlTableElement => domhtmlTableElement -> IO (Id NSString)
width domhtmlTableElement  =
  sendMsg domhtmlTableElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLTableElement domhtmlTableElement, IsNSString value) => domhtmlTableElement -> value -> IO ()
setWidth domhtmlTableElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createTHead@
createTHeadSelector :: Selector
createTHeadSelector = mkSelector "createTHead"

-- | @Selector@ for @deleteTHead@
deleteTHeadSelector :: Selector
deleteTHeadSelector = mkSelector "deleteTHead"

-- | @Selector@ for @createTFoot@
createTFootSelector :: Selector
createTFootSelector = mkSelector "createTFoot"

-- | @Selector@ for @deleteTFoot@
deleteTFootSelector :: Selector
deleteTFootSelector = mkSelector "deleteTFoot"

-- | @Selector@ for @createCaption@
createCaptionSelector :: Selector
createCaptionSelector = mkSelector "createCaption"

-- | @Selector@ for @deleteCaption@
deleteCaptionSelector :: Selector
deleteCaptionSelector = mkSelector "deleteCaption"

-- | @Selector@ for @insertRow:@
insertRowSelector :: Selector
insertRowSelector = mkSelector "insertRow:"

-- | @Selector@ for @deleteRow:@
deleteRowSelector :: Selector
deleteRowSelector = mkSelector "deleteRow:"

-- | @Selector@ for @caption@
captionSelector :: Selector
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @tHead@
tHeadSelector :: Selector
tHeadSelector = mkSelector "tHead"

-- | @Selector@ for @setTHead:@
setTHeadSelector :: Selector
setTHeadSelector = mkSelector "setTHead:"

-- | @Selector@ for @tFoot@
tFootSelector :: Selector
tFootSelector = mkSelector "tFoot"

-- | @Selector@ for @setTFoot:@
setTFootSelector :: Selector
setTFootSelector = mkSelector "setTFoot:"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

-- | @Selector@ for @tBodies@
tBodiesSelector :: Selector
tBodiesSelector = mkSelector "tBodies"

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @border@
borderSelector :: Selector
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @cellPadding@
cellPaddingSelector :: Selector
cellPaddingSelector = mkSelector "cellPadding"

-- | @Selector@ for @setCellPadding:@
setCellPaddingSelector :: Selector
setCellPaddingSelector = mkSelector "setCellPadding:"

-- | @Selector@ for @cellSpacing@
cellSpacingSelector :: Selector
cellSpacingSelector = mkSelector "cellSpacing"

-- | @Selector@ for @setCellSpacing:@
setCellSpacingSelector :: Selector
setCellSpacingSelector = mkSelector "setCellSpacing:"

-- | @Selector@ for @frameBorders@
frameBordersSelector :: Selector
frameBordersSelector = mkSelector "frameBorders"

-- | @Selector@ for @setFrameBorders:@
setFrameBordersSelector :: Selector
setFrameBordersSelector = mkSelector "setFrameBorders:"

-- | @Selector@ for @rules@
rulesSelector :: Selector
rulesSelector = mkSelector "rules"

-- | @Selector@ for @setRules:@
setRulesSelector :: Selector
setRulesSelector = mkSelector "setRules:"

-- | @Selector@ for @summary@
summarySelector :: Selector
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

