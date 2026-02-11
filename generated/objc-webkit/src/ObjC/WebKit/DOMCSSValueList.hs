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
item :: IsDOMCSSValueList domcssValueList => domcssValueList -> CUInt -> IO (Id DOMCSSValue)
item domcssValueList  index =
  sendMsg domcssValueList (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMCSSValueList domcssValueList => domcssValueList -> IO CUInt
length_ domcssValueList  =
  sendMsg domcssValueList (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

