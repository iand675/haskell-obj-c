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

-- | @- item:@
item :: IsDOMCSSRuleList domcssRuleList => domcssRuleList -> CUInt -> IO (Id DOMCSSRule)
item domcssRuleList  index =
  sendMsg domcssRuleList (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMCSSRuleList domcssRuleList => domcssRuleList -> IO CUInt
length_ domcssRuleList  =
  sendMsg domcssRuleList (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

