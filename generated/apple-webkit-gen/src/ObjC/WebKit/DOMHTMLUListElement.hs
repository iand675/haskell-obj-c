{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLUListElement@.
module ObjC.WebKit.DOMHTMLUListElement
  ( DOMHTMLUListElement
  , IsDOMHTMLUListElement(..)
  , compact
  , setCompact
  , type_
  , setType
  , compactSelector
  , setCompactSelector
  , setTypeSelector
  , typeSelector


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
compact :: IsDOMHTMLUListElement domhtmluListElement => domhtmluListElement -> IO Bool
compact domhtmluListElement =
  sendMessage domhtmluListElement compactSelector

-- | @- setCompact:@
setCompact :: IsDOMHTMLUListElement domhtmluListElement => domhtmluListElement -> Bool -> IO ()
setCompact domhtmluListElement value =
  sendMessage domhtmluListElement setCompactSelector value

-- | @- type@
type_ :: IsDOMHTMLUListElement domhtmluListElement => domhtmluListElement -> IO (Id NSString)
type_ domhtmluListElement =
  sendMessage domhtmluListElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLUListElement domhtmluListElement, IsNSString value) => domhtmluListElement -> value -> IO ()
setType domhtmluListElement value =
  sendMessage domhtmluListElement setTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector '[] Bool
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector '[Bool] ()
setCompactSelector = mkSelector "setCompact:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

