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
  , newSelector
  , propertyListForTextStyleRulesSelector
  , textStyleRulesFromPropertyListSelector
  , textStyleRuleWithTextMarkupAttributesSelector
  , textStyleRuleWithTextMarkupAttributes_textSelectorSelector
  , initWithTextMarkupAttributesSelector
  , initWithTextMarkupAttributes_textSelectorSelector
  , textMarkupAttributesSelector
  , textSelectorSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVTextStyleRule avTextStyleRule => avTextStyleRule -> IO (Id AVTextStyleRule)
init_ avTextStyleRule  =
  sendMsg avTextStyleRule (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVTextStyleRule)
new  =
  do
    cls' <- getRequiredClass "AVTextStyleRule"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr textStyleRules $ \raw_textStyleRules ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "propertyListForTextStyleRules:") (retPtr retVoid) [argPtr (castPtr raw_textStyleRules :: Ptr ())]

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
    sendClassMsg cls' (mkSelector "textStyleRulesFromPropertyList:") (retPtr retVoid) [argPtr (castPtr (unRawId plist) :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr textMarkupAttributes $ \raw_textMarkupAttributes ->
      sendClassMsg cls' (mkSelector "textStyleRuleWithTextMarkupAttributes:") (retPtr retVoid) [argPtr (castPtr raw_textMarkupAttributes :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr textMarkupAttributes $ \raw_textMarkupAttributes ->
      withObjCPtr textSelector $ \raw_textSelector ->
        sendClassMsg cls' (mkSelector "textStyleRuleWithTextMarkupAttributes:textSelector:") (retPtr retVoid) [argPtr (castPtr raw_textMarkupAttributes :: Ptr ()), argPtr (castPtr raw_textSelector :: Ptr ())] >>= retainedObject . castPtr

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
initWithTextMarkupAttributes avTextStyleRule  textMarkupAttributes =
withObjCPtr textMarkupAttributes $ \raw_textMarkupAttributes ->
    sendMsg avTextStyleRule (mkSelector "initWithTextMarkupAttributes:") (retPtr retVoid) [argPtr (castPtr raw_textMarkupAttributes :: Ptr ())] >>= ownedObject . castPtr

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
initWithTextMarkupAttributes_textSelector avTextStyleRule  textMarkupAttributes textSelector =
withObjCPtr textMarkupAttributes $ \raw_textMarkupAttributes ->
  withObjCPtr textSelector $ \raw_textSelector ->
      sendMsg avTextStyleRule (mkSelector "initWithTextMarkupAttributes:textSelector:") (retPtr retVoid) [argPtr (castPtr raw_textMarkupAttributes :: Ptr ()), argPtr (castPtr raw_textSelector :: Ptr ())] >>= ownedObject . castPtr

-- | textMarkupAttributes
--
-- An NSDictionary with keys representing text style attributes that are specifiable in text markup. Eligible keys and the expected types of their corresponding values are defined in <CoreMedia/CMTextMarkup.h>.
--
-- ObjC selector: @- textMarkupAttributes@
textMarkupAttributes :: IsAVTextStyleRule avTextStyleRule => avTextStyleRule -> IO (Id NSDictionary)
textMarkupAttributes avTextStyleRule  =
  sendMsg avTextStyleRule (mkSelector "textMarkupAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | textSelector
--
-- A string that identifies the range or ranges of text to which the attributes should be applied. A value of nil indicates that the textMarkupAttributes should be applied as default styles for all text unless overridden by content markup or other applicable text selectors.
--
-- The syntax of text selectors is determined by the format of the legible media. Eligible selectors may be determined by the content of the legible media (e.g. CSS selectors that are valid for a specific WebVTT document).
--
-- ObjC selector: @- textSelector@
textSelector :: IsAVTextStyleRule avTextStyleRule => avTextStyleRule -> IO (Id NSString)
textSelector avTextStyleRule  =
  sendMsg avTextStyleRule (mkSelector "textSelector") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @propertyListForTextStyleRules:@
propertyListForTextStyleRulesSelector :: Selector
propertyListForTextStyleRulesSelector = mkSelector "propertyListForTextStyleRules:"

-- | @Selector@ for @textStyleRulesFromPropertyList:@
textStyleRulesFromPropertyListSelector :: Selector
textStyleRulesFromPropertyListSelector = mkSelector "textStyleRulesFromPropertyList:"

-- | @Selector@ for @textStyleRuleWithTextMarkupAttributes:@
textStyleRuleWithTextMarkupAttributesSelector :: Selector
textStyleRuleWithTextMarkupAttributesSelector = mkSelector "textStyleRuleWithTextMarkupAttributes:"

-- | @Selector@ for @textStyleRuleWithTextMarkupAttributes:textSelector:@
textStyleRuleWithTextMarkupAttributes_textSelectorSelector :: Selector
textStyleRuleWithTextMarkupAttributes_textSelectorSelector = mkSelector "textStyleRuleWithTextMarkupAttributes:textSelector:"

-- | @Selector@ for @initWithTextMarkupAttributes:@
initWithTextMarkupAttributesSelector :: Selector
initWithTextMarkupAttributesSelector = mkSelector "initWithTextMarkupAttributes:"

-- | @Selector@ for @initWithTextMarkupAttributes:textSelector:@
initWithTextMarkupAttributes_textSelectorSelector :: Selector
initWithTextMarkupAttributes_textSelectorSelector = mkSelector "initWithTextMarkupAttributes:textSelector:"

-- | @Selector@ for @textMarkupAttributes@
textMarkupAttributesSelector :: Selector
textMarkupAttributesSelector = mkSelector "textMarkupAttributes"

-- | @Selector@ for @textSelector@
textSelectorSelector :: Selector
textSelectorSelector = mkSelector "textSelector"

