{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLLIElement@.
module ObjC.WebKit.DOMHTMLLIElement
  ( DOMHTMLLIElement
  , IsDOMHTMLLIElement(..)
  , type_
  , setType
  , value
  , setValue
  , setTypeSelector
  , setValueSelector
  , typeSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsDOMHTMLLIElement domhtmlliElement => domhtmlliElement -> IO (Id NSString)
type_ domhtmlliElement =
  sendMessage domhtmlliElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLLIElement domhtmlliElement, IsNSString value) => domhtmlliElement -> value -> IO ()
setType domhtmlliElement value =
  sendMessage domhtmlliElement setTypeSelector (toNSString value)

-- | @- value@
value :: IsDOMHTMLLIElement domhtmlliElement => domhtmlliElement -> IO CInt
value domhtmlliElement =
  sendMessage domhtmlliElement valueSelector

-- | @- setValue:@
setValue :: IsDOMHTMLLIElement domhtmlliElement => domhtmlliElement -> CInt -> IO ()
setValue domhtmlliElement value =
  sendMessage domhtmlliElement setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CInt
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CInt] ()
setValueSelector = mkSelector "setValue:"

