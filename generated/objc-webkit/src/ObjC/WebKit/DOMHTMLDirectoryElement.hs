{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLDirectoryElement@.
module ObjC.WebKit.DOMHTMLDirectoryElement
  ( DOMHTMLDirectoryElement
  , IsDOMHTMLDirectoryElement(..)
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
compact :: IsDOMHTMLDirectoryElement domhtmlDirectoryElement => domhtmlDirectoryElement -> IO Bool
compact domhtmlDirectoryElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlDirectoryElement (mkSelector "compact") retCULong []

-- | @- setCompact:@
setCompact :: IsDOMHTMLDirectoryElement domhtmlDirectoryElement => domhtmlDirectoryElement -> Bool -> IO ()
setCompact domhtmlDirectoryElement  value =
  sendMsg domhtmlDirectoryElement (mkSelector "setCompact:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector
setCompactSelector = mkSelector "setCompact:"

