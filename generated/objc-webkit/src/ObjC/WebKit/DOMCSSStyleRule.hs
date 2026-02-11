{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSStyleRule@.
module ObjC.WebKit.DOMCSSStyleRule
  ( DOMCSSStyleRule
  , IsDOMCSSStyleRule(..)
  , selectorText
  , setSelectorText
  , style
  , selectorTextSelector
  , setSelectorTextSelector
  , styleSelector


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

-- | @- selectorText@
selectorText :: IsDOMCSSStyleRule domcssStyleRule => domcssStyleRule -> IO (Id NSString)
selectorText domcssStyleRule  =
  sendMsg domcssStyleRule (mkSelector "selectorText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectorText:@
setSelectorText :: (IsDOMCSSStyleRule domcssStyleRule, IsNSString value) => domcssStyleRule -> value -> IO ()
setSelectorText domcssStyleRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg domcssStyleRule (mkSelector "setSelectorText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- style@
style :: IsDOMCSSStyleRule domcssStyleRule => domcssStyleRule -> IO (Id DOMCSSStyleDeclaration)
style domcssStyleRule  =
  sendMsg domcssStyleRule (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectorText@
selectorTextSelector :: Selector
selectorTextSelector = mkSelector "selectorText"

-- | @Selector@ for @setSelectorText:@
setSelectorTextSelector :: Selector
setSelectorTextSelector = mkSelector "setSelectorText:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

