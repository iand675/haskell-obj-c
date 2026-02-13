{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- href@
href :: IsDOMCSSImportRule domcssImportRule => domcssImportRule -> IO (Id NSString)
href domcssImportRule =
  sendMessage domcssImportRule hrefSelector

-- | @- media@
media :: IsDOMCSSImportRule domcssImportRule => domcssImportRule -> IO (Id DOMMediaList)
media domcssImportRule =
  sendMessage domcssImportRule mediaSelector

-- | @- styleSheet@
styleSheet :: IsDOMCSSImportRule domcssImportRule => domcssImportRule -> IO (Id DOMCSSStyleSheet)
styleSheet domcssImportRule =
  sendMessage domcssImportRule styleSheetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @href@
hrefSelector :: Selector '[] (Id NSString)
hrefSelector = mkSelector "href"

-- | @Selector@ for @media@
mediaSelector :: Selector '[] (Id DOMMediaList)
mediaSelector = mkSelector "media"

-- | @Selector@ for @styleSheet@
styleSheetSelector :: Selector '[] (Id DOMCSSStyleSheet)
styleSheetSelector = mkSelector "styleSheet"

