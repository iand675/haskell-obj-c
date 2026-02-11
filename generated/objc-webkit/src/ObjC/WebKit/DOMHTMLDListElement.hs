{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLDListElement@.
module ObjC.WebKit.DOMHTMLDListElement
  ( DOMHTMLDListElement
  , IsDOMHTMLDListElement(..)
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
compact :: IsDOMHTMLDListElement domhtmldListElement => domhtmldListElement -> IO Bool
compact domhtmldListElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmldListElement (mkSelector "compact") retCULong []

-- | @- setCompact:@
setCompact :: IsDOMHTMLDListElement domhtmldListElement => domhtmldListElement -> Bool -> IO ()
setCompact domhtmldListElement  value =
  sendMsg domhtmldListElement (mkSelector "setCompact:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector
setCompactSelector = mkSelector "setCompact:"

