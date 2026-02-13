{-# LANGUAGE DataKinds #-}
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
  , cssRulesSelector
  , deleteRuleSelector
  , insertRuleSelector
  , insertRule_indexSelector
  , mediaSelector


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
insertRule_index :: (IsDOMCSSMediaRule domcssMediaRule, IsNSString rule) => domcssMediaRule -> rule -> CUInt -> IO CUInt
insertRule_index domcssMediaRule rule index =
  sendMessage domcssMediaRule insertRule_indexSelector (toNSString rule) index

-- | @- deleteRule:@
deleteRule :: IsDOMCSSMediaRule domcssMediaRule => domcssMediaRule -> CUInt -> IO ()
deleteRule domcssMediaRule index =
  sendMessage domcssMediaRule deleteRuleSelector index

-- | @- insertRule::@
insertRule :: (IsDOMCSSMediaRule domcssMediaRule, IsNSString rule) => domcssMediaRule -> rule -> CUInt -> IO CUInt
insertRule domcssMediaRule rule index =
  sendMessage domcssMediaRule insertRuleSelector (toNSString rule) index

-- | @- media@
media :: IsDOMCSSMediaRule domcssMediaRule => domcssMediaRule -> IO (Id DOMMediaList)
media domcssMediaRule =
  sendMessage domcssMediaRule mediaSelector

-- | @- cssRules@
cssRules :: IsDOMCSSMediaRule domcssMediaRule => domcssMediaRule -> IO (Id DOMCSSRuleList)
cssRules domcssMediaRule =
  sendMessage domcssMediaRule cssRulesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertRule:index:@
insertRule_indexSelector :: Selector '[Id NSString, CUInt] CUInt
insertRule_indexSelector = mkSelector "insertRule:index:"

-- | @Selector@ for @deleteRule:@
deleteRuleSelector :: Selector '[CUInt] ()
deleteRuleSelector = mkSelector "deleteRule:"

-- | @Selector@ for @insertRule::@
insertRuleSelector :: Selector '[Id NSString, CUInt] CUInt
insertRuleSelector = mkSelector "insertRule::"

-- | @Selector@ for @media@
mediaSelector :: Selector '[] (Id DOMMediaList)
mediaSelector = mkSelector "media"

-- | @Selector@ for @cssRules@
cssRulesSelector :: Selector '[] (Id DOMCSSRuleList)
cssRulesSelector = mkSelector "cssRules"

