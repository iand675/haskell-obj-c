{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- selectorText@
selectorText :: IsDOMCSSStyleRule domcssStyleRule => domcssStyleRule -> IO (Id NSString)
selectorText domcssStyleRule =
  sendMessage domcssStyleRule selectorTextSelector

-- | @- setSelectorText:@
setSelectorText :: (IsDOMCSSStyleRule domcssStyleRule, IsNSString value) => domcssStyleRule -> value -> IO ()
setSelectorText domcssStyleRule value =
  sendMessage domcssStyleRule setSelectorTextSelector (toNSString value)

-- | @- style@
style :: IsDOMCSSStyleRule domcssStyleRule => domcssStyleRule -> IO (Id DOMCSSStyleDeclaration)
style domcssStyleRule =
  sendMessage domcssStyleRule styleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectorText@
selectorTextSelector :: Selector '[] (Id NSString)
selectorTextSelector = mkSelector "selectorText"

-- | @Selector@ for @setSelectorText:@
setSelectorTextSelector :: Selector '[Id NSString] ()
setSelectorTextSelector = mkSelector "setSelectorText:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] (Id DOMCSSStyleDeclaration)
styleSelector = mkSelector "style"

