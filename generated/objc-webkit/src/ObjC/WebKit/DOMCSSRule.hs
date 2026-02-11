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
  , typeSelector
  , cssTextSelector
  , setCssTextSelector
  , parentStyleSheetSelector
  , parentRuleSelector


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

-- | @- type@
type_ :: IsDOMCSSRule domcssRule => domcssRule -> IO CUShort
type_ domcssRule  =
  fmap fromIntegral $ sendMsg domcssRule (mkSelector "type") retCUInt []

-- | @- cssText@
cssText :: IsDOMCSSRule domcssRule => domcssRule -> IO (Id NSString)
cssText domcssRule  =
  sendMsg domcssRule (mkSelector "cssText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCssText:@
setCssText :: (IsDOMCSSRule domcssRule, IsNSString value) => domcssRule -> value -> IO ()
setCssText domcssRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg domcssRule (mkSelector "setCssText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- parentStyleSheet@
parentStyleSheet :: IsDOMCSSRule domcssRule => domcssRule -> IO (Id DOMCSSStyleSheet)
parentStyleSheet domcssRule  =
  sendMsg domcssRule (mkSelector "parentStyleSheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parentRule@
parentRule :: IsDOMCSSRule domcssRule => domcssRule -> IO (Id DOMCSSRule)
parentRule domcssRule  =
  sendMsg domcssRule (mkSelector "parentRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @cssText@
cssTextSelector :: Selector
cssTextSelector = mkSelector "cssText"

-- | @Selector@ for @setCssText:@
setCssTextSelector :: Selector
setCssTextSelector = mkSelector "setCssText:"

-- | @Selector@ for @parentStyleSheet@
parentStyleSheetSelector :: Selector
parentStyleSheetSelector = mkSelector "parentStyleSheet"

-- | @Selector@ for @parentRule@
parentRuleSelector :: Selector
parentRuleSelector = mkSelector "parentRule"

