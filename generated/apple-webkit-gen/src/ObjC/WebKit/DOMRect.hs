{-# LANGUAGE DataKinds #-}
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
  , bottomSelector
  , leftSelector
  , rightSelector
  , topSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- top@
top :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
top domRect =
  sendMessage domRect topSelector

-- | @- right@
right :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
right domRect =
  sendMessage domRect rightSelector

-- | @- bottom@
bottom :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
bottom domRect =
  sendMessage domRect bottomSelector

-- | @- left@
left :: IsDOMRect domRect => domRect -> IO (Id DOMCSSPrimitiveValue)
left domRect =
  sendMessage domRect leftSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @top@
topSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
topSelector = mkSelector "top"

-- | @Selector@ for @right@
rightSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
rightSelector = mkSelector "right"

-- | @Selector@ for @bottom@
bottomSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
bottomSelector = mkSelector "bottom"

-- | @Selector@ for @left@
leftSelector :: Selector '[] (Id DOMCSSPrimitiveValue)
leftSelector = mkSelector "left"

