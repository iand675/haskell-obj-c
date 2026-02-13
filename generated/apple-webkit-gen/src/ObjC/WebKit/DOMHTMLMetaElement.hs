{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLMetaElement@.
module ObjC.WebKit.DOMHTMLMetaElement
  ( DOMHTMLMetaElement
  , IsDOMHTMLMetaElement(..)
  , content
  , setContent
  , httpEquiv
  , setHttpEquiv
  , name
  , setName
  , scheme
  , setScheme
  , contentSelector
  , httpEquivSelector
  , nameSelector
  , schemeSelector
  , setContentSelector
  , setHttpEquivSelector
  , setNameSelector
  , setSchemeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- content@
content :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
content domhtmlMetaElement =
  sendMessage domhtmlMetaElement contentSelector

-- | @- setContent:@
setContent :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setContent domhtmlMetaElement value =
  sendMessage domhtmlMetaElement setContentSelector (toNSString value)

-- | @- httpEquiv@
httpEquiv :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
httpEquiv domhtmlMetaElement =
  sendMessage domhtmlMetaElement httpEquivSelector

-- | @- setHttpEquiv:@
setHttpEquiv :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setHttpEquiv domhtmlMetaElement value =
  sendMessage domhtmlMetaElement setHttpEquivSelector (toNSString value)

-- | @- name@
name :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
name domhtmlMetaElement =
  sendMessage domhtmlMetaElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setName domhtmlMetaElement value =
  sendMessage domhtmlMetaElement setNameSelector (toNSString value)

-- | @- scheme@
scheme :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
scheme domhtmlMetaElement =
  sendMessage domhtmlMetaElement schemeSelector

-- | @- setScheme:@
setScheme :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setScheme domhtmlMetaElement value =
  sendMessage domhtmlMetaElement setSchemeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector '[Id NSString] ()
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @httpEquiv@
httpEquivSelector :: Selector '[] (Id NSString)
httpEquivSelector = mkSelector "httpEquiv"

-- | @Selector@ for @setHttpEquiv:@
setHttpEquivSelector :: Selector '[Id NSString] ()
setHttpEquivSelector = mkSelector "setHttpEquiv:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @scheme@
schemeSelector :: Selector '[] (Id NSString)
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @setScheme:@
setSchemeSelector :: Selector '[Id NSString] ()
setSchemeSelector = mkSelector "setScheme:"

