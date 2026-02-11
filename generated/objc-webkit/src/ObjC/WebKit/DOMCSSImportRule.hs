{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSImportRule@.
module ObjC.WebKit.DOMCSSImportRule
  ( DOMCSSImportRule
  , IsDOMCSSImportRule(..)
  , href
  , media
  , styleSheet
  , hrefSelector
  , mediaSelector
  , styleSheetSelector


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

-- | @- href@
href :: IsDOMCSSImportRule domcssImportRule => domcssImportRule -> IO (Id NSString)
href domcssImportRule  =
  sendMsg domcssImportRule (mkSelector "href") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- media@
media :: IsDOMCSSImportRule domcssImportRule => domcssImportRule -> IO (Id DOMMediaList)
media domcssImportRule  =
  sendMsg domcssImportRule (mkSelector "media") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- styleSheet@
styleSheet :: IsDOMCSSImportRule domcssImportRule => domcssImportRule -> IO (Id DOMCSSStyleSheet)
styleSheet domcssImportRule  =
  sendMsg domcssImportRule (mkSelector "styleSheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @href@
hrefSelector :: Selector
hrefSelector = mkSelector "href"

-- | @Selector@ for @media@
mediaSelector :: Selector
mediaSelector = mkSelector "media"

-- | @Selector@ for @styleSheet@
styleSheetSelector :: Selector
styleSheetSelector = mkSelector "styleSheet"

