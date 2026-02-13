{-# LANGUAGE DataKinds #-}
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
  , attributedStringSelector
  , childElementsSelector
  , contentsSelector
  , initWithAttributedStringSelector
  , initWithParentElement_textList_contents_markerAttributes_childElementsSelector
  , markerAttributesSelector
  , parentElementSelector
  , textListElementWithChildElements_textList_nestingLevelSelector
  , textListElementWithContents_markerAttributes_textList_childElementsSelector
  , textListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithParentElement:textList:contents:markerAttributes:childElements:@
initWithParentElement_textList_contents_markerAttributes_childElements :: (IsNSTextListElement nsTextListElement, IsNSTextListElement parent, IsNSTextList textList, IsNSAttributedString contents, IsNSDictionary markerAttributes, IsNSArray children) => nsTextListElement -> parent -> textList -> contents -> markerAttributes -> children -> IO (Id NSTextListElement)
initWithParentElement_textList_contents_markerAttributes_childElements nsTextListElement parent textList contents markerAttributes children =
  sendOwnedMessage nsTextListElement initWithParentElement_textList_contents_markerAttributes_childElementsSelector (toNSTextListElement parent) (toNSTextList textList) (toNSAttributedString contents) (toNSDictionary markerAttributes) (toNSArray children)

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsNSTextListElement nsTextListElement, IsNSAttributedString attributedString) => nsTextListElement -> attributedString -> IO (Id NSTextListElement)
initWithAttributedString nsTextListElement attributedString =
  sendOwnedMessage nsTextListElement initWithAttributedStringSelector (toNSAttributedString attributedString)

-- | @+ textListElementWithContents:markerAttributes:textList:childElements:@
textListElementWithContents_markerAttributes_textList_childElements :: (IsNSAttributedString contents, IsNSDictionary markerAttributes, IsNSTextList textList, IsNSArray children) => contents -> markerAttributes -> textList -> children -> IO (Id NSTextListElement)
textListElementWithContents_markerAttributes_textList_childElements contents markerAttributes textList children =
  do
    cls' <- getRequiredClass "NSTextListElement"
    sendClassMessage cls' textListElementWithContents_markerAttributes_textList_childElementsSelector (toNSAttributedString contents) (toNSDictionary markerAttributes) (toNSTextList textList) (toNSArray children)

-- | @+ textListElementWithChildElements:textList:nestingLevel:@
textListElementWithChildElements_textList_nestingLevel :: (IsNSArray children, IsNSTextList textList) => children -> textList -> CLong -> IO (Id NSTextListElement)
textListElementWithChildElements_textList_nestingLevel children textList nestingLevel =
  do
    cls' <- getRequiredClass "NSTextListElement"
    sendClassMessage cls' textListElementWithChildElements_textList_nestingLevelSelector (toNSArray children) (toNSTextList textList) nestingLevel

-- | @- textList@
textList :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSTextList)
textList nsTextListElement =
  sendMessage nsTextListElement textListSelector

-- | @- contents@
contents :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSAttributedString)
contents nsTextListElement =
  sendMessage nsTextListElement contentsSelector

-- | @- markerAttributes@
markerAttributes :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSDictionary)
markerAttributes nsTextListElement =
  sendMessage nsTextListElement markerAttributesSelector

-- | @- attributedString@
attributedString :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSAttributedString)
attributedString nsTextListElement =
  sendMessage nsTextListElement attributedStringSelector

-- | @- childElements@
childElements :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSArray)
childElements nsTextListElement =
  sendMessage nsTextListElement childElementsSelector

-- | @- parentElement@
parentElement :: IsNSTextListElement nsTextListElement => nsTextListElement -> IO (Id NSTextListElement)
parentElement nsTextListElement =
  sendMessage nsTextListElement parentElementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParentElement:textList:contents:markerAttributes:childElements:@
initWithParentElement_textList_contents_markerAttributes_childElementsSelector :: Selector '[Id NSTextListElement, Id NSTextList, Id NSAttributedString, Id NSDictionary, Id NSArray] (Id NSTextListElement)
initWithParentElement_textList_contents_markerAttributes_childElementsSelector = mkSelector "initWithParentElement:textList:contents:markerAttributes:childElements:"

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector '[Id NSAttributedString] (Id NSTextListElement)
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @textListElementWithContents:markerAttributes:textList:childElements:@
textListElementWithContents_markerAttributes_textList_childElementsSelector :: Selector '[Id NSAttributedString, Id NSDictionary, Id NSTextList, Id NSArray] (Id NSTextListElement)
textListElementWithContents_markerAttributes_textList_childElementsSelector = mkSelector "textListElementWithContents:markerAttributes:textList:childElements:"

-- | @Selector@ for @textListElementWithChildElements:textList:nestingLevel:@
textListElementWithChildElements_textList_nestingLevelSelector :: Selector '[Id NSArray, Id NSTextList, CLong] (Id NSTextListElement)
textListElementWithChildElements_textList_nestingLevelSelector = mkSelector "textListElementWithChildElements:textList:nestingLevel:"

-- | @Selector@ for @textList@
textListSelector :: Selector '[] (Id NSTextList)
textListSelector = mkSelector "textList"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] (Id NSAttributedString)
contentsSelector = mkSelector "contents"

-- | @Selector@ for @markerAttributes@
markerAttributesSelector :: Selector '[] (Id NSDictionary)
markerAttributesSelector = mkSelector "markerAttributes"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @childElements@
childElementsSelector :: Selector '[] (Id NSArray)
childElementsSelector = mkSelector "childElements"

-- | @Selector@ for @parentElement@
parentElementSelector :: Selector '[] (Id NSTextListElement)
parentElementSelector = mkSelector "parentElement"

