{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLMenuElement@.
module ObjC.WebKit.DOMHTMLMenuElement
  ( DOMHTMLMenuElement
  , IsDOMHTMLMenuElement(..)
  , compact
  , setCompact
  , compactSelector
  , setCompactSelector


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

-- | @- compact@
compact :: IsDOMHTMLMenuElement domhtmlMenuElement => domhtmlMenuElement -> IO Bool
compact domhtmlMenuElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlMenuElement (mkSelector "compact") retCULong []

-- | @- setCompact:@
setCompact :: IsDOMHTMLMenuElement domhtmlMenuElement => domhtmlMenuElement -> Bool -> IO ()
setCompact domhtmlMenuElement  value =
  sendMsg domhtmlMenuElement (mkSelector "setCompact:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector
setCompactSelector = mkSelector "setCompact:"

