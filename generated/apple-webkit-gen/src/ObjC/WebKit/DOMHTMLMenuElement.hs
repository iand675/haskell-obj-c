{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- compact@
compact :: IsDOMHTMLMenuElement domhtmlMenuElement => domhtmlMenuElement -> IO Bool
compact domhtmlMenuElement =
  sendMessage domhtmlMenuElement compactSelector

-- | @- setCompact:@
setCompact :: IsDOMHTMLMenuElement domhtmlMenuElement => domhtmlMenuElement -> Bool -> IO ()
setCompact domhtmlMenuElement value =
  sendMessage domhtmlMenuElement setCompactSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector '[] Bool
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector '[Bool] ()
setCompactSelector = mkSelector "setCompact:"

