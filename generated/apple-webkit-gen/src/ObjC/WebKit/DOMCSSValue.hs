{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSValue@.
module ObjC.WebKit.DOMCSSValue
  ( DOMCSSValue
  , IsDOMCSSValue(..)
  , cssText
  , setCssText
  , cssValueType
  , cssTextSelector
  , cssValueTypeSelector
  , setCssTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cssText@
cssText :: IsDOMCSSValue domcssValue => domcssValue -> IO (Id NSString)
cssText domcssValue =
  sendMessage domcssValue cssTextSelector

-- | @- setCssText:@
setCssText :: (IsDOMCSSValue domcssValue, IsNSString value) => domcssValue -> value -> IO ()
setCssText domcssValue value =
  sendMessage domcssValue setCssTextSelector (toNSString value)

-- | @- cssValueType@
cssValueType :: IsDOMCSSValue domcssValue => domcssValue -> IO CUShort
cssValueType domcssValue =
  sendMessage domcssValue cssValueTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cssText@
cssTextSelector :: Selector '[] (Id NSString)
cssTextSelector = mkSelector "cssText"

-- | @Selector@ for @setCssText:@
setCssTextSelector :: Selector '[Id NSString] ()
setCssTextSelector = mkSelector "setCssText:"

-- | @Selector@ for @cssValueType@
cssValueTypeSelector :: Selector '[] CUShort
cssValueTypeSelector = mkSelector "cssValueType"

