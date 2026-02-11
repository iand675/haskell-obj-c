{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMRect@.
module ObjC.WebKit.DOMRect
  ( DOMRect
  , IsDOMRect(..)
  , top
  , right
  , bottom
  , left
  , topSelector
  , rightSelector
  , bottomSelector
  , leftSelector


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

-- | @- top@
top :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
top domRect  =
  sendMsg domRect (mkSelector "top") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- right@
right :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
right domRect  =
  sendMsg domRect (mkSelector "right") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bottom@
bottom :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
bottom domRect  =
  sendMsg domRect (mkSelector "bottom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- left@
left :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
left domRect  =
  sendMsg domRect (mkSelector "left") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @top@
topSelector :: Selector
topSelector = mkSelector "top"

-- | @Selector@ for @right@
rightSelector :: Selector
rightSelector = mkSelector "right"

-- | @Selector@ for @bottom@
bottomSelector :: Selector
bottomSelector = mkSelector "bottom"

-- | @Selector@ for @left@
leftSelector :: Selector
leftSelector = mkSelector "left"

