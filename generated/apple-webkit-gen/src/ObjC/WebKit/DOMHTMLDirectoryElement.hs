{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- compact@
compact :: IsDOMHTMLDirectoryElement domhtmlDirectoryElement => domhtmlDirectoryElement -> IO Bool
compact domhtmlDirectoryElement =
  sendMessage domhtmlDirectoryElement compactSelector

-- | @- setCompact:@
setCompact :: IsDOMHTMLDirectoryElement domhtmlDirectoryElement => domhtmlDirectoryElement -> Bool -> IO ()
setCompact domhtmlDirectoryElement value =
  sendMessage domhtmlDirectoryElement setCompactSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector '[] Bool
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector '[Bool] ()
setCompactSelector = mkSelector "setCompact:"

