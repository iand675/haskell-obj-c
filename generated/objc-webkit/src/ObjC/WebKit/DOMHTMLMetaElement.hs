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
  , setContentSelector
  , httpEquivSelector
  , setHttpEquivSelector
  , nameSelector
  , setNameSelector
  , schemeSelector
  , setSchemeSelector


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

-- | @- content@
content :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
content domhtmlMetaElement  =
  sendMsg domhtmlMetaElement (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContent:@
setContent :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setContent domhtmlMetaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlMetaElement (mkSelector "setContent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- httpEquiv@
httpEquiv :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
httpEquiv domhtmlMetaElement  =
  sendMsg domhtmlMetaElement (mkSelector "httpEquiv") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHttpEquiv:@
setHttpEquiv :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setHttpEquiv domhtmlMetaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlMetaElement (mkSelector "setHttpEquiv:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
name domhtmlMetaElement  =
  sendMsg domhtmlMetaElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setName domhtmlMetaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlMetaElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scheme@
scheme :: IsDOMHTMLMetaElement domhtmlMetaElement => domhtmlMetaElement -> IO (Id NSString)
scheme domhtmlMetaElement  =
  sendMsg domhtmlMetaElement (mkSelector "scheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScheme:@
setScheme :: (IsDOMHTMLMetaElement domhtmlMetaElement, IsNSString value) => domhtmlMetaElement -> value -> IO ()
setScheme domhtmlMetaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlMetaElement (mkSelector "setScheme:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @httpEquiv@
httpEquivSelector :: Selector
httpEquivSelector = mkSelector "httpEquiv"

-- | @Selector@ for @setHttpEquiv:@
setHttpEquivSelector :: Selector
setHttpEquivSelector = mkSelector "setHttpEquiv:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @scheme@
schemeSelector :: Selector
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @setScheme:@
setSchemeSelector :: Selector
setSchemeSelector = mkSelector "setScheme:"

