{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSStyleSheet@.
module ObjC.WebKit.DOMCSSStyleSheet
  ( DOMCSSStyleSheet
  , IsDOMCSSStyleSheet(..)
  , insertRule_index
  , deleteRule
  , addRule_style_index
  , removeRule
  , insertRule
  , ownerRule
  , cssRules
  , rules
  , addRule_style_indexSelector
  , cssRulesSelector
  , deleteRuleSelector
  , insertRuleSelector
  , insertRule_indexSelector
  , ownerRuleSelector
  , removeRuleSelector
  , rulesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- insertRule:index:@
insertRule_index :: (IsDOMCSSStyleSheet domcssStyleSheet, IsNSString rule) => domcssStyleSheet -> rule -> CUInt -> IO CUInt
insertRule_index domcssStyleSheet rule index =
  sendMessage domcssStyleSheet insertRule_indexSelector (toNSString rule) index

-- | @- deleteRule:@
deleteRule :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> CUInt -> IO ()
deleteRule domcssStyleSheet index =
  sendMessage domcssStyleSheet deleteRuleSelector index

-- | @- addRule:style:index:@
addRule_style_index :: (IsDOMCSSStyleSheet domcssStyleSheet, IsNSString selector, IsNSString style) => domcssStyleSheet -> selector -> style -> CUInt -> IO CInt
addRule_style_index domcssStyleSheet selector style index =
  sendMessage domcssStyleSheet addRule_style_indexSelector (toNSString selector) (toNSString style) index

-- | @- removeRule:@
removeRule :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> CUInt -> IO ()
removeRule domcssStyleSheet index =
  sendMessage domcssStyleSheet removeRuleSelector index

-- | @- insertRule::@
insertRule :: (IsDOMCSSStyleSheet domcssStyleSheet, IsNSString rule) => domcssStyleSheet -> rule -> CUInt -> IO CUInt
insertRule domcssStyleSheet rule index =
  sendMessage domcssStyleSheet insertRuleSelector (toNSString rule) index

-- | @- ownerRule@
ownerRule :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> IO (Id DOMCSSRule)
ownerRule domcssStyleSheet =
  sendMessage domcssStyleSheet ownerRuleSelector

-- | @- cssRules@
cssRules :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> IO (Id DOMCSSRuleList)
cssRules domcssStyleSheet =
  sendMessage domcssStyleSheet cssRulesSelector

-- | @- rules@
rules :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> IO (Id DOMCSSRuleList)
rules domcssStyleSheet =
  sendMessage domcssStyleSheet rulesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertRule:index:@
insertRule_indexSelector :: Selector '[Id NSString, CUInt] CUInt
insertRule_indexSelector = mkSelector "insertRule:index:"

-- | @Selector@ for @deleteRule:@
deleteRuleSelector :: Selector '[CUInt] ()
deleteRuleSelector = mkSelector "deleteRule:"

-- | @Selector@ for @addRule:style:index:@
addRule_style_indexSelector :: Selector '[Id NSString, Id NSString, CUInt] CInt
addRule_style_indexSelector = mkSelector "addRule:style:index:"

-- | @Selector@ for @removeRule:@
removeRuleSelector :: Selector '[CUInt] ()
removeRuleSelector = mkSelector "removeRule:"

-- | @Selector@ for @insertRule::@
insertRuleSelector :: Selector '[Id NSString, CUInt] CUInt
insertRuleSelector = mkSelector "insertRule::"

-- | @Selector@ for @ownerRule@
ownerRuleSelector :: Selector '[] (Id DOMCSSRule)
ownerRuleSelector = mkSelector "ownerRule"

-- | @Selector@ for @cssRules@
cssRulesSelector :: Selector '[] (Id DOMCSSRuleList)
cssRulesSelector = mkSelector "cssRules"

-- | @Selector@ for @rules@
rulesSelector :: Selector '[] (Id DOMCSSRuleList)
rulesSelector = mkSelector "rules"

