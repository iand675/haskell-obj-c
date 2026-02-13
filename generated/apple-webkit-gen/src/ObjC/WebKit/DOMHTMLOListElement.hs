{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLOListElement@.
module ObjC.WebKit.DOMHTMLOListElement
  ( DOMHTMLOListElement
  , IsDOMHTMLOListElement(..)
  , compact
  , setCompact
  , start
  , setStart
  , type_
  , setType
  , compactSelector
  , setCompactSelector
  , setStartSelector
  , setTypeSelector
  , startSelector
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
compact :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> IO Bool
compact domhtmloListElement =
  sendMessage domhtmloListElement compactSelector

-- | @- setCompact:@
setCompact :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> Bool -> IO ()
setCompact domhtmloListElement value =
  sendMessage domhtmloListElement setCompactSelector value

-- | @- start@
start :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> IO CInt
start domhtmloListElement =
  sendMessage domhtmloListElement startSelector

-- | @- setStart:@
setStart :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> CInt -> IO ()
setStart domhtmloListElement value =
  sendMessage domhtmloListElement setStartSelector value

-- | @- type@
type_ :: IsDOMHTMLOListElement domhtmloListElement => domhtmloListElement -> IO (Id NSString)
type_ domhtmloListElement =
  sendMessage domhtmloListElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLOListElement domhtmloListElement, IsNSString value) => domhtmloListElement -> value -> IO ()
setType domhtmloListElement value =
  sendMessage domhtmloListElement setTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compact@
compactSelector :: Selector '[] Bool
compactSelector = mkSelector "compact"

-- | @Selector@ for @setCompact:@
setCompactSelector :: Selector '[Bool] ()
setCompactSelector = mkSelector "setCompact:"

-- | @Selector@ for @start@
startSelector :: Selector '[] CInt
startSelector = mkSelector "start"

-- | @Selector@ for @setStart:@
setStartSelector :: Selector '[CInt] ()
setStartSelector = mkSelector "setStart:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

