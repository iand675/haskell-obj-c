{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSMediaRule@.
module ObjC.WebKit.DOMCSSMediaRule
  ( DOMCSSMediaRule
  , IsDOMCSSMediaRule(..)
  , insertRule_index
  , deleteRule
  , insertRule
  , media
  , cssRules
  , insertRule_indexSelector
  , deleteRuleSelector
  , insertRuleSelector
  , mediaSelector
  , cssRulesSelector


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
insertRule_index :: (IsDOMCSSMediaRule domcssMediaRule, IsNSString rule) => domcssMediaRule -> rule -> CUInt -> IO CUInt
insertRule_index domcssMediaRule  rule index =
withObjCPtr rule $ \raw_rule ->
    sendMsg domcssMediaRule (mkSelector "insertRule:index:") retCUInt [argPtr (castPtr raw_rule :: Ptr ()), argCUInt (fromIntegral index)]

-- | @- deleteRule:@
deleteRule :: IsDOMCSSMediaRule domcssMediaRule => domcssMediaRule -> CUInt -> IO ()
deleteRule domcssMediaRule  index =
  sendMsg domcssMediaRule (mkSelector "deleteRule:") retVoid [argCUInt (fromIntegral index)]

-- | @- insertRule::@
insertRule :: (IsDOMCSSMediaRule domcssMediaRule, IsNSString rule) => domcssMediaRule -> rule -> CUInt -> IO CUInt
insertRule domcssMediaRule  rule index =
withObjCPtr rule $ \raw_rule ->
    sendMsg domcssMediaRule (mkSelector "insertRule::") retCUInt [argPtr (castPtr raw_rule :: Ptr ()), argCUInt (fromIntegral index)]

-- | @- media@
media :: IsDOMCSSMediaRule domcssMediaRule => domcssMediaRule -> IO (Id DOMMediaList)
media domcssMediaRule  =
  sendMsg domcssMediaRule (mkSelector "media") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cssRules@
cssRules :: IsDOMCSSMediaRule domcssMediaRule => domcssMediaRule -> IO (Id DOMCSSRuleList)
cssRules domcssMediaRule  =
  sendMsg domcssMediaRule (mkSelector "cssRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertRule:index:@
insertRule_indexSelector :: Selector
insertRule_indexSelector = mkSelector "insertRule:index:"

-- | @Selector@ for @deleteRule:@
deleteRuleSelector :: Selector
deleteRuleSelector = mkSelector "deleteRule:"

-- | @Selector@ for @insertRule::@
insertRuleSelector :: Selector
insertRuleSelector = mkSelector "insertRule::"

-- | @Selector@ for @media@
mediaSelector :: Selector
mediaSelector = mkSelector "media"

-- | @Selector@ for @cssRules@
cssRulesSelector :: Selector
cssRulesSelector = mkSelector "cssRules"

