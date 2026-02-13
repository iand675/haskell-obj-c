{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- areas@
areas :: IsDOMHTMLMapElement domhtmlMapElement => domhtmlMapElement -> IO (Id DOMHTMLCollection)
areas domhtmlMapElement =
  sendMessage domhtmlMapElement areasSelector

-- | @- name@
name :: IsDOMHTMLMapElement domhtmlMapElement => domhtmlMapElement -> IO (Id NSString)
name domhtmlMapElement =
  sendMessage domhtmlMapElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLMapElement domhtmlMapElement, IsNSString value) => domhtmlMapElement -> value -> IO ()
setName domhtmlMapElement value =
  sendMessage domhtmlMapElement setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areas@
areasSelector :: Selector '[] (Id DOMHTMLCollection)
areasSelector = mkSelector "areas"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

