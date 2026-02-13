{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSRuleList@.
module ObjC.WebKit.DOMCSSRuleList
  ( DOMCSSRuleList
  , IsDOMCSSRuleList(..)
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
item :: IsDOMCSSRuleList domcssRuleList => domcssRuleList -> CUInt -> IO (Id DOMCSSRule)
item domcssRuleList index =
  sendMessage domcssRuleList itemSelector index

-- | @- length@
length_ :: IsDOMCSSRuleList domcssRuleList => domcssRuleList -> IO CUInt
length_ domcssRuleList =
  sendMessage domcssRuleList lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMCSSRule)
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

