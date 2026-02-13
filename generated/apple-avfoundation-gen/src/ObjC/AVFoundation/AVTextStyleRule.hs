{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVTextStyleRule@.
module ObjC.AVFoundation.AVTextStyleRule
  ( AVTextStyleRule
  , IsAVTextStyleRule(..)
  , init_
  , new
  , propertyListForTextStyleRules
  , textStyleRulesFromPropertyList
  , textStyleRuleWithTextMarkupAttributes
  , textStyleRuleWithTextMarkupAttributes_textSelector
  , initWithTextMarkupAttributes
  , initWithTextMarkupAttributes_textSelector
  , textMarkupAttributes
  , textSelector
  , initSelector
  , initWithTextMarkupAttributesSelector
  , initWithTextMarkupAttributes_textSelectorSelector
  , newSelector
  , propertyListForTextStyleRulesSelector
  , textMarkupAttributesSelector
  , textSelectorSelector
  , textStyleRuleWithTextMarkupAttributesSelector
  , textStyleRuleWithTextMarkupAttributes_textSelectorSelector
  , textStyleRulesFromPropertyListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVTextStyleRule avTextStyleRule => avTextStyleRule -> IO (Id AVTextStyleRule)
init_ avTextStyleRule =
  sendOwnedMessage avTextStyleRule initSelector

-- | @+ new@
new :: IO (Id AVTextStyleRule)
new  =
  do
    cls' <- getRequiredClass "AVTextStyleRule"
    sendOwnedClassMessage cls' newSelector

-- | propertyListForTextStyleRules:
--
-- Converts an NSArray of AVTextStyleRules into a serializable property list that can be used for persistent storage.
--
-- @textStyleRules@ — An array of AVTextStyleRules.
--
-- Returns: A serializable property list.
--
-- For serialization utilities, see NSPropertyList.h.
--
-- ObjC selector: @+ propertyListForTextStyleRules:@
propertyListForTextStyleRules :: IsNSArray textStyleRules => textStyleRules -> IO RawId
propertyListForTextStyleRules textStyleRules =
  do
    cls' <- getRequiredClass "AVTextStyleRule"
    sendClassMessage cls' propertyListForTextStyleRulesSelector (toNSArray textStyleRules)

-- | textStyleRulesFromPropertyList:
--
-- Converts a property list into an NSArray of AVTextStyleRules.
--
-- @plist@ — A property list, normally obtained previously via an invocation of +propertyListForTextStyleRules:.
--
-- Returns: An NSArray of AVTextStyleRules
--
-- ObjC selector: @+ textStyleRulesFromPropertyList:@
textStyleRulesFromPropertyList :: RawId -> IO (Id NSArray)
textStyleRulesFromPropertyList plist =
  do
    cls' <- getRequiredClass "AVTextStyleRule"
    sendClassMessage cls' textStyleRulesFromPropertyListSelector plist

-- | textStyleRuleWithTextMarkupAttributes:
--
-- Creates an instance of AVTextStyleRule with the specified text markup attributes.
--
-- @textMarkupAttributes@ — An NSDictionary with keys representing text style attributes that are specifiable in text markup. Eligible keys are defined in <CoreMedia/CMTextMarkup.h>.
--
-- Returns: An instance of AVTextStyleRule
--
-- Equivalent to invoking +textStyleRuleWithTextMarkupAttributes:textSelector: with a value of nil for textSelector.
--
-- ObjC selector: @+ textStyleRuleWithTextMarkupAttributes:@
textStyleRuleWithTextMarkupAttributes :: IsNSDictionary textMarkupAttributes => textMarkupAttributes -> IO (Id AVTextStyleRule)
textStyleRuleWithTextMarkupAttributes textMarkupAttributes =
  do
    cls' <- getRequiredClass "AVTextStyleRule"
    sendClassMessage cls' textStyleRuleWithTextMarkupAttributesSelector (toNSDictionary textMarkupAttributes)

-- | textStyleRuleWithTextMarkupAttributes:textSelector:
--
-- Creates an instance of AVTextStyleRule with the specified text markup attributes and an identifier for the range or ranges of text to which the attributes should be applied.
--
-- @textMarkupAttributes@ — An NSDictionary with keys representing text style attributes that are specifiable in text markup. Eligible keys are defined in <CoreMedia/CMTextMarkup.h>.
--
-- @textSelector@ — An identifier for the range or ranges of text to which the attributes should be applied. Eligible identifiers are determined by the format and content of the legible media. A value of nil indicates that the textMarkupAttributes should be applied as default styles for all text unless overridden by content markup or other applicable text selectors.
--
-- Returns: An instance of AVTextStyleRule
--
-- ObjC selector: @+ textStyleRuleWithTextMarkupAttributes:textSelector:@
textStyleRuleWithTextMarkupAttributes_textSelector :: (IsNSDictionary textMarkupAttributes, IsNSString textSelector) => textMarkupAttributes -> textSelector -> IO (Id AVTextStyleRule)
textStyleRuleWithTextMarkupAttributes_textSelector textMarkupAttributes textSelector =
  do
    cls' <- getRequiredClass "AVTextStyleRule"
    sendClassMessage cls' textStyleRuleWithTextMarkupAttributes_textSelectorSelector (toNSDictionary textMarkupAttributes) (toNSString textSelector)

-- | initWithTextMarkupAttributes:
--
-- Creates an instance of AVTextStyleRule with the specified text markup attributes.
--
-- @textMarkupAttributes@ — An NSDictionary with keys representing text style attributes that are specifiable in text markup. Eligible keys are defined in <CoreMedia/CMTextMarkup.h>.
--
-- Returns: An instance of AVTextStyleRule
--
-- Equivalent to invoking -initWithTextMarkupAttributes:textSelector: with a value of nil for textSelector.
--
-- ObjC selector: @- initWithTextMarkupAttributes:@
initWithTextMarkupAttributes :: (IsAVTextStyleRule avTextStyleRule, IsNSDictionary textMarkupAttributes) => avTextStyleRule -> textMarkupAttributes -> IO (Id AVTextStyleRule)
initWithTextMarkupAttributes avTextStyleRule textMarkupAttributes =
  sendOwnedMessage avTextStyleRule initWithTextMarkupAttributesSelector (toNSDictionary textMarkupAttributes)

-- | initWithTextMarkupAttributes:textSelector:
--
-- Creates an instance of AVTextStyleRule with the specified text markup attributes and an identifier for the range or ranges of text to which the attributes should be applied.
--
-- @textMarkupAttributes@ — An NSDictionary with keys representing text style attributes that are specifiable in text markup. Eligible keys are defined in <CoreMedia/CMTextMarkup.h>.
--
-- @textSelector@ — An identifier for the range or ranges of text to which the attributes should be applied. Eligible identifiers are determined by the format and content of the legible media. A value of nil indicates that the textMarkupAttributes should be applied as default styles for all text unless overridden by content markup or other applicable text selectors.
--
-- Returns: An instance of AVTextStyleRule
--
-- ObjC selector: @- initWithTextMarkupAttributes:textSelector:@
initWithTextMarkupAttributes_textSelector :: (IsAVTextStyleRule avTextStyleRule, IsNSDictionary textMarkupAttributes, IsNSString textSelector) => avTextStyleRule -> textMarkupAttributes -> textSelector -> IO (Id AVTextStyleRule)
initWithTextMarkupAttributes_textSelector avTextStyleRule textMarkupAttributes textSelector =
  sendOwnedMessage avTextStyleRule initWithTextMarkupAttributes_textSelectorSelector (toNSDictionary textMarkupAttributes) (toNSString textSelector)

-- | textMarkupAttributes
--
-- An NSDictionary with keys representing text style attributes that are specifiable in text markup. Eligible keys and the expected types of their corresponding values are defined in <CoreMedia/CMTextMarkup.h>.
--
-- ObjC selector: @- textMarkupAttributes@
textMarkupAttributes :: IsAVTextStyleRule avTextStyleRule => avTextStyleRule -> IO (Id NSDictionary)
textMarkupAttributes avTextStyleRule =
  sendMessage avTextStyleRule textMarkupAttributesSelector

-- | textSelector
--
-- A string that identifies the range or ranges of text to which the attributes should be applied. A value of nil indicates that the textMarkupAttributes should be applied as default styles for all text unless overridden by content markup or other applicable text selectors.
--
-- The syntax of text selectors is determined by the format of the legible media. Eligible selectors may be determined by the content of the legible media (e.g. CSS selectors that are valid for a specific WebVTT document).
--
-- ObjC selector: @- textSelector@
textSelector :: IsAVTextStyleRule avTextStyleRule => avTextStyleRule -> IO (Id NSString)
textSelector avTextStyleRule =
  sendMessage avTextStyleRule textSelectorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVTextStyleRule)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVTextStyleRule)
newSelector = mkSelector "new"

-- | @Selector@ for @propertyListForTextStyleRules:@
propertyListForTextStyleRulesSelector :: Selector '[Id NSArray] RawId
propertyListForTextStyleRulesSelector = mkSelector "propertyListForTextStyleRules:"

-- | @Selector@ for @textStyleRulesFromPropertyList:@
textStyleRulesFromPropertyListSelector :: Selector '[RawId] (Id NSArray)
textStyleRulesFromPropertyListSelector = mkSelector "textStyleRulesFromPropertyList:"

-- | @Selector@ for @textStyleRuleWithTextMarkupAttributes:@
textStyleRuleWithTextMarkupAttributesSelector :: Selector '[Id NSDictionary] (Id AVTextStyleRule)
textStyleRuleWithTextMarkupAttributesSelector = mkSelector "textStyleRuleWithTextMarkupAttributes:"

-- | @Selector@ for @textStyleRuleWithTextMarkupAttributes:textSelector:@
textStyleRuleWithTextMarkupAttributes_textSelectorSelector :: Selector '[Id NSDictionary, Id NSString] (Id AVTextStyleRule)
textStyleRuleWithTextMarkupAttributes_textSelectorSelector = mkSelector "textStyleRuleWithTextMarkupAttributes:textSelector:"

-- | @Selector@ for @initWithTextMarkupAttributes:@
initWithTextMarkupAttributesSelector :: Selector '[Id NSDictionary] (Id AVTextStyleRule)
initWithTextMarkupAttributesSelector = mkSelector "initWithTextMarkupAttributes:"

-- | @Selector@ for @initWithTextMarkupAttributes:textSelector:@
initWithTextMarkupAttributes_textSelectorSelector :: Selector '[Id NSDictionary, Id NSString] (Id AVTextStyleRule)
initWithTextMarkupAttributes_textSelectorSelector = mkSelector "initWithTextMarkupAttributes:textSelector:"

-- | @Selector@ for @textMarkupAttributes@
textMarkupAttributesSelector :: Selector '[] (Id NSDictionary)
textMarkupAttributesSelector = mkSelector "textMarkupAttributes"

-- | @Selector@ for @textSelector@
textSelectorSelector :: Selector '[] (Id NSString)
textSelectorSelector = mkSelector "textSelector"

