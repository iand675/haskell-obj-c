{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMAttr@.
module ObjC.WebKit.DOMAttr
  ( DOMAttr
  , IsDOMAttr(..)
  , name
  , specified
  , value
  , setValue
  , ownerElement
  , style
  , nameSelector
  , ownerElementSelector
  , setValueSelector
  , specifiedSelector
  , styleSelector
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

-- | @- name@
name :: IsDOMAttr domAttr => domAttr -> IO (Id NSString)
name domAttr =
  sendMessage domAttr nameSelector

-- | @- specified@
specified :: IsDOMAttr domAttr => domAttr -> IO Bool
specified domAttr =
  sendMessage domAttr specifiedSelector

-- | @- value@
value :: IsDOMAttr domAttr => domAttr -> IO (Id NSString)
value domAttr =
  sendMessage domAttr valueSelector

-- | @- setValue:@
setValue :: (IsDOMAttr domAttr, IsNSString value) => domAttr -> value -> IO ()
setValue domAttr value =
  sendMessage domAttr setValueSelector (toNSString value)

-- | @- ownerElement@
ownerElement :: IsDOMAttr domAttr => domAttr -> IO (Id DOMElement)
ownerElement domAttr =
  sendMessage domAttr ownerElementSelector

-- | @- style@
style :: IsDOMAttr domAttr => domAttr -> IO (Id DOMCSSStyleDeclaration)
style domAttr =
  sendMessage domAttr styleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @specified@
specifiedSelector :: Selector '[] Bool
specifiedSelector = mkSelector "specified"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @ownerElement@
ownerElementSelector :: Selector '[] (Id DOMElement)
ownerElementSelector = mkSelector "ownerElement"

-- | @Selector@ for @style@
styleSelector :: Selector '[] (Id DOMCSSStyleDeclaration)
styleSelector = mkSelector "style"

