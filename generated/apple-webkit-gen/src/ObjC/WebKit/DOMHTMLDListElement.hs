{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- compact@
compact :: IsDOMHTMLDListElement domhtmldListElement => domhtmldListElement -> IO Bool
compact domhtmldListElement =
  sendMessage domhtmldListElement compactSelector

-- | @- setCompact:@
setCompact :: IsDOMHTMLDListElement domhtmldListElement => domhtmldListElement -> Bool -> IO ()
setCompact domhtmldListElement value =
  sendMessage domhtmldListElement setCompactSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector '[] Bool
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector '[Bool] ()
setCompactSelector = mkSelector "setCompact:"

