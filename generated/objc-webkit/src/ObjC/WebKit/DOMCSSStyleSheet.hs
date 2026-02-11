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
  , insertRule_indexSelector
  , deleteRuleSelector
  , addRule_style_indexSelector
  , removeRuleSelector
  , insertRuleSelector
  , ownerRuleSelector
  , cssRulesSelector
  , rulesSelector


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

-- | @- insertRule:index:@
insertRule_index :: (IsDOMCSSStyleSheet domcssStyleSheet, IsNSString rule) => domcssStyleSheet -> rule -> CUInt -> IO CUInt
insertRule_index domcssStyleSheet  rule index =
withObjCPtr rule $ \raw_rule ->
    sendMsg domcssStyleSheet (mkSelector "insertRule:index:") retCUInt [argPtr (castPtr raw_rule :: Ptr ()), argCUInt (fromIntegral index)]

-- | @- deleteRule:@
deleteRule :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> CUInt -> IO ()
deleteRule domcssStyleSheet  index =
  sendMsg domcssStyleSheet (mkSelector "deleteRule:") retVoid [argCUInt (fromIntegral index)]

-- | @- addRule:style:index:@
addRule_style_index :: (IsDOMCSSStyleSheet domcssStyleSheet, IsNSString selector, IsNSString style) => domcssStyleSheet -> selector -> style -> CUInt -> IO CInt
addRule_style_index domcssStyleSheet  selector style index =
withObjCPtr selector $ \raw_selector ->
  withObjCPtr style $ \raw_style ->
      sendMsg domcssStyleSheet (mkSelector "addRule:style:index:") retCInt [argPtr (castPtr raw_selector :: Ptr ()), argPtr (castPtr raw_style :: Ptr ()), argCUInt (fromIntegral index)]

-- | @- removeRule:@
removeRule :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> CUInt -> IO ()
removeRule domcssStyleSheet  index =
  sendMsg domcssStyleSheet (mkSelector "removeRule:") retVoid [argCUInt (fromIntegral index)]

-- | @- insertRule::@
insertRule :: (IsDOMCSSStyleSheet domcssStyleSheet, IsNSString rule) => domcssStyleSheet -> rule -> CUInt -> IO CUInt
insertRule domcssStyleSheet  rule index =
withObjCPtr rule $ \raw_rule ->
    sendMsg domcssStyleSheet (mkSelector "insertRule::") retCUInt [argPtr (castPtr raw_rule :: Ptr ()), argCUInt (fromIntegral index)]

-- | @- ownerRule@
ownerRule :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> IO (Id DOMCSSRule)
ownerRule domcssStyleSheet  =
  sendMsg domcssStyleSheet (mkSelector "ownerRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cssRules@
cssRules :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> IO (Id DOMCSSRuleList)
cssRules domcssStyleSheet  =
  sendMsg domcssStyleSheet (mkSelector "cssRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rules@
rules :: IsDOMCSSStyleSheet domcssStyleSheet => domcssStyleSheet -> IO (Id DOMCSSRuleList)
rules domcssStyleSheet  =
  sendMsg domcssStyleSheet (mkSelector "rules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertRule:index:@
insertRule_indexSelector :: Selector
insertRule_indexSelector = mkSelector "insertRule:index:"

-- | @Selector@ for @deleteRule:@
deleteRuleSelector :: Selector
deleteRuleSelector = mkSelector "deleteRule:"

-- | @Selector@ for @addRule:style:index:@
addRule_style_indexSelector :: Selector
addRule_style_indexSelector = mkSelector "addRule:style:index:"

-- | @Selector@ for @removeRule:@
removeRuleSelector :: Selector
removeRuleSelector = mkSelector "removeRule:"

-- | @Selector@ for @insertRule::@
insertRuleSelector :: Selector
insertRuleSelector = mkSelector "insertRule::"

-- | @Selector@ for @ownerRule@
ownerRuleSelector :: Selector
ownerRuleSelector = mkSelector "ownerRule"

-- | @Selector@ for @cssRules@
cssRulesSelector :: Selector
cssRulesSelector = mkSelector "cssRules"

-- | @Selector@ for @rules@
rulesSelector :: Selector
rulesSelector = mkSelector "rules"

