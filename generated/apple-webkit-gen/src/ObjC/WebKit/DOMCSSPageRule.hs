{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSPageRule@.
module ObjC.WebKit.DOMCSSPageRule
  ( DOMCSSPageRule
  , IsDOMCSSPageRule(..)
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
selectorText :: IsDOMCSSPageRule domcssPageRule => domcssPageRule -> IO (Id NSString)
selectorText domcssPageRule =
  sendMessage domcssPageRule selectorTextSelector

-- | @- setSelectorText:@
setSelectorText :: (IsDOMCSSPageRule domcssPageRule, IsNSString value) => domcssPageRule -> value -> IO ()
setSelectorText domcssPageRule value =
  sendMessage domcssPageRule setSelectorTextSelector (toNSString value)

-- | @- style@
style :: IsDOMCSSPageRule domcssPageRule => domcssPageRule -> IO (Id DOMCSSStyleDeclaration)
style domcssPageRule =
  sendMessage domcssPageRule styleSelector

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

