{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLMapElement@.
module ObjC.WebKit.DOMHTMLMapElement
  ( DOMHTMLMapElement
  , IsDOMHTMLMapElement(..)
  , areas
  , name
  , setName
  , areasSelector
  , nameSelector
  , setNameSelector


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

-- | @- areas@
areas :: IsDOMHTMLMapElement domhtmlMapElement => domhtmlMapElement -> IO (Id DOMHTMLCollection)
areas domhtmlMapElement  =
  sendMsg domhtmlMapElement (mkSelector "areas") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsDOMHTMLMapElement domhtmlMapElement => domhtmlMapElement -> IO (Id NSString)
name domhtmlMapElement  =
  sendMsg domhtmlMapElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLMapElement domhtmlMapElement, IsNSString value) => domhtmlMapElement -> value -> IO ()
setName domhtmlMapElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlMapElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areas@
areasSelector :: Selector
areasSelector = mkSelector "areas"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

