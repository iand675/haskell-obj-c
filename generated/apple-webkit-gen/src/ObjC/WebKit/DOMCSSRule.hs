{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSRule@.
module ObjC.WebKit.DOMCSSRule
  ( DOMCSSRule
  , IsDOMCSSRule(..)
  , type_
  , cssText
  , setCssText
  , parentStyleSheet
  , parentRule
  , cssTextSelector
  , parentRuleSelector
  , parentStyleSheetSelector
  , setCssTextSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsDOMCSSRule domcssRule => domcssRule -> IO CUShort
type_ domcssRule =
  sendMessage domcssRule typeSelector

-- | @- cssText@
cssText :: IsDOMCSSRule domcssRule => domcssRule -> IO (Id NSString)
cssText domcssRule =
  sendMessage domcssRule cssTextSelector

-- | @- setCssText:@
setCssText :: (IsDOMCSSRule domcssRule, IsNSString value) => domcssRule -> value -> IO ()
setCssText domcssRule value =
  sendMessage domcssRule setCssTextSelector (toNSString value)

-- | @- parentStyleSheet@
parentStyleSheet :: IsDOMCSSRule domcssRule => domcssRule -> IO (Id DOMCSSStyleSheet)
parentStyleSheet domcssRule =
  sendMessage domcssRule parentStyleSheetSelector

-- | @- parentRule@
parentRule :: IsDOMCSSRule domcssRule => domcssRule -> IO (Id DOMCSSRule)
parentRule domcssRule =
  sendMessage domcssRule parentRuleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] CUShort
typeSelector = mkSelector "type"

-- | @Selector@ for @cssText@
cssTextSelector :: Selector '[] (Id NSString)
cssTextSelector = mkSelector "cssText"

-- | @Selector@ for @setCssText:@
setCssTextSelector :: Selector '[Id NSString] ()
setCssTextSelector = mkSelector "setCssText:"

-- | @Selector@ for @parentStyleSheet@
parentStyleSheetSelector :: Selector '[] (Id DOMCSSStyleSheet)
parentStyleSheetSelector = mkSelector "parentStyleSheet"

-- | @Selector@ for @parentRule@
parentRuleSelector :: Selector '[] (Id DOMCSSRule)
parentRuleSelector = mkSelector "parentRule"

