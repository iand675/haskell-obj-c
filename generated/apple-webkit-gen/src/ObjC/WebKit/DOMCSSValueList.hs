{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSValueList@.
module ObjC.WebKit.DOMCSSValueList
  ( DOMCSSValueList
  , IsDOMCSSValueList(..)
  , item
  , length_
  , itemSelector
  , lengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- item:@
item :: IsDOMCSSValueList domcssValueList => domcssValueList -> CUInt -> IO (Id DOMCSSValue)
item domcssValueList index =
  sendMessage domcssValueList itemSelector index

-- | @- length@
length_ :: IsDOMCSSValueList domcssValueList => domcssValueList -> IO CUInt
length_ domcssValueList =
  sendMessage domcssValueList lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMCSSValue)
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

