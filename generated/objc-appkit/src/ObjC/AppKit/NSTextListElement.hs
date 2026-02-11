{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextListElement@.
module ObjC.AppKit.NSTextListElement
  ( NSTextListElement
  , IsNSTextListElement(..)
  , initWithParentElement_textList_contents_markerAttributes_childElements
  , initWithAttributedString
  , textListElementWithContents_markerAttributes_textList_childElements
  , textListElementWithChildElements_textList_nestingLevel
  , textList
  , contents
  , markerAttributes
  , attributedString
  , childElements
  , parentElement
  , initWithParentElement_textList_contents_markerAttributes_childElementsSelector
  , initWithAttributedStringSelector
  , textListElementWithContents_markerAttributes_textList_childElementsSelector
  , textListElementWithChildElements_textList_nestingLevelSelector
  , textListSelector
  , contentsSelector
  , markerAttributesSelector
  , attributedStringSelector
  , childElementsSelector
  , parentElementSelector


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

-- | @- initWithParentElement:textList:contents:markerAttributes:childElements:@
initWithParentElement_textList_contents_markerAttributes_childElements :: (IsNSTextListElement nsTextListElement, IsNSTextListElement parent, IsNSTextList textList, IsNSAttributedString contents, IsNSDictionary markerAttributes, IsNSArray children) => nsTextListElement -> parent -> textList -> contents -> markerAttributes -> children -> IO (Id NSTextListElement)
initWithParentElement_textList_contents_markerAttributes_childElements nsTextListElement  parent textList contents markerAttributes children =
withObjCPtr parent $ \raw_parent ->
  withObjCPtr textList $ \raw_textList ->
    withObjCPtr contents $ \raw_contents ->
      withObjCPtr markerAttributes $ \raw_markerAttributes ->
        withObjCPtr children $ \raw_children ->
            sendMsg nsTextListElement (mkSelector "initWithParentElement:textList:contents:markerAttributes:childElements:") (retPtr retVoid) [argPtr (castPtr raw_parent :: Ptr ()), argPtr (castPtr raw_textList :: Ptr ()), argPtr (castPtr raw_contents :: Ptr ()), argPtr (castPtr raw_markerAttributes :: Ptr ()), argPtr (castPtr raw_children :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsNSTextListElement nsTextListElement, IsNSAttributedString attributedString) => nsTextListElement -> attributedString -> IO (Id NSTextListElement)
initWithAttributedString nsTextListElement  attributedString =
withObjCPtr attributedString $ \raw_attributedString ->
    sendMsg nsTextListElement (mkSelector "initWithAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_attributedString :: Ptr ())] >>= ownedObject . castPtr

-- | @+ textListElementWithContents:markerAttributes:textList:childElements:@
textListElementWithContents_markerAttributes_textList_childElements :: (IsNSAttributedString contents, IsNSDictionary markerAttributes, IsNSTextList textList, IsNSArray children) => contents -> markerAttributes -> textList -> children -> IO (Id NSTextListElement)
textListElementWithContents_markerAttributes_textList_childElements contents markerAttributes textList children =
  do
    cls' <- getRequiredClass "NSTextListElement"
    withObjCPtr contents $ \raw_contents ->
      withObjCPtr markerAttributes $ \raw_markerAttributes ->
        withObjCPtr textList $ \raw_textList ->
          withObjCPtr children $ \raw_children ->
            sendClassMsg cls' (mkSelector "textListElementWithContents:markerAttributes:textList:childElements:") (retPtr retVoid) [argPtr (castPtr raw_contents :: Ptr ()), argPtr (castPtr raw_markerAttributes :: Ptr ()), argPtr (castPtr raw_textList :: Ptr ()), argPtr (castPtr raw_children :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textListElementWithChildElements:textList:nestingLevel:@
textListElementWithChildElements_textList_nestingLevel :: (IsNSArray children, IsNSTextList textList) => children -> textList -> CLong -> IO (Id NSTextListElement)
textListElementWithChildElements_textList_nestingLevel children textList nestingLevel =
  do
    cls' <- getRequiredClass "NSTextListElement"
    withObjCPtr children $ \raw_children ->
      withObjCPtr textList $ \raw_textList ->
        sendClassMsg cls' (mkSelector "textListElementWithChildElements:textList:nestingLevel:") (retPtr retVoid) [argPtr (castPtr raw_children :: Ptr ()), argPtr (castPtr raw_textList :: Ptr ()), argCLong (fromIntegral nestingLevel)] >>= retainedObject . castPtr

-- | @- textList@
textList :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSTextList)
textList nsTextListElement  =
  sendMsg nsTextListElement (mkSelector "textList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contents@
contents :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSAttributedString)
contents nsTextListElement  =
  sendMsg nsTextListElement (mkSelector "contents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- markerAttributes@
markerAttributes :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSDictionary)
markerAttributes nsTextListElement  =
  sendMsg nsTextListElement (mkSelector "markerAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributedString@
attributedString :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSAttributedString)
attributedString nsTextListElement  =
  sendMsg nsTextListElement (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- childElements@
childElements :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSArray)
childElements nsTextListElement  =
  sendMsg nsTextListElement (mkSelector "childElements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parentElement@
parentElement :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSTextListElement)
parentElement nsTextListElement  =
  sendMsg nsTextListElement (mkSelector "parentElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParentElement:textList:contents:markerAttributes:childElements:@
initWithParentElement_textList_contents_markerAttributes_childElementsSelector :: Selector
initWithParentElement_textList_contents_markerAttributes_childElementsSelector = mkSelector "initWithParentElement:textList:contents:markerAttributes:childElements:"

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @textListElementWithContents:markerAttributes:textList:childElements:@
textListElementWithContents_markerAttributes_textList_childElementsSelector :: Selector
textListElementWithContents_markerAttributes_textList_childElementsSelector = mkSelector "textListElementWithContents:markerAttributes:textList:childElements:"

-- | @Selector@ for @textListElementWithChildElements:textList:nestingLevel:@
textListElementWithChildElements_textList_nestingLevelSelector :: Selector
textListElementWithChildElements_textList_nestingLevelSelector = mkSelector "textListElementWithChildElements:textList:nestingLevel:"

-- | @Selector@ for @textList@
textListSelector :: Selector
textListSelector = mkSelector "textList"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @markerAttributes@
markerAttributesSelector :: Selector
markerAttributesSelector = mkSelector "markerAttributes"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @childElements@
childElementsSelector :: Selector
childElementsSelector = mkSelector "childElements"

-- | @Selector@ for @parentElement@
parentElementSelector :: Selector
parentElementSelector = mkSelector "parentElement"

