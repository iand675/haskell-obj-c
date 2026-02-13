{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSFontFaceRule@.
module ObjC.WebKit.DOMCSSFontFaceRule
  ( DOMCSSFontFaceRule
  , IsDOMCSSFontFaceRule(..)
  , style
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

-- | @- style@
style :: IsDOMCSSFontFaceRule domcssFontFaceRule => domcssFontFaceRule -> IO (Id DOMCSSStyleDeclaration)
style domcssFontFaceRule =
  sendMessage domcssFontFaceRule styleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @style@
styleSelector :: Selector '[] (Id DOMCSSStyleDeclaration)
styleSelector = mkSelector "style"

