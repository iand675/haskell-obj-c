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

-- | @- style@
style :: IsDOMCSSFontFaceRule domcssFontFaceRule => domcssFontFaceRule -> IO (Id DOMCSSStyleDeclaration)
style domcssFontFaceRule  =
  sendMsg domcssFontFaceRule (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

