{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLEmbedElement@.
module ObjC.WebKit.DOMHTMLEmbedElement
  ( DOMHTMLEmbedElement
  , IsDOMHTMLEmbedElement(..)
  , align
  , setAlign
  , height
  , setHeight
  , name
  , setName
  , src
  , setSrc
  , type_
  , setType
  , width
  , setWidth
  , alignSelector
  , heightSelector
  , nameSelector
  , setAlignSelector
  , setHeightSelector
  , setNameSelector
  , setSrcSelector
  , setTypeSelector
  , setWidthSelector
  , srcSelector
  , typeSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- align@
align :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
align domhtmlEmbedElement =
  sendMessage domhtmlEmbedElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setAlign domhtmlEmbedElement value =
  sendMessage domhtmlEmbedElement setAlignSelector (toNSString value)

-- | @- height@
height :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO CInt
height domhtmlEmbedElement =
  sendMessage domhtmlEmbedElement heightSelector

-- | @- setHeight:@
setHeight :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> CInt -> IO ()
setHeight domhtmlEmbedElement value =
  sendMessage domhtmlEmbedElement setHeightSelector value

-- | @- name@
name :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
name domhtmlEmbedElement =
  sendMessage domhtmlEmbedElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setName domhtmlEmbedElement value =
  sendMessage domhtmlEmbedElement setNameSelector (toNSString value)

-- | @- src@
src :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
src domhtmlEmbedElement =
  sendMessage domhtmlEmbedElement srcSelector

-- | @- setSrc:@
setSrc :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setSrc domhtmlEmbedElement value =
  sendMessage domhtmlEmbedElement setSrcSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
type_ domhtmlEmbedElement =
  sendMessage domhtmlEmbedElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setType domhtmlEmbedElement value =
  sendMessage domhtmlEmbedElement setTypeSelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO CInt
width domhtmlEmbedElement =
  sendMessage domhtmlEmbedElement widthSelector

-- | @- setWidth:@
setWidth :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> CInt -> IO ()
setWidth domhtmlEmbedElement value =
  sendMessage domhtmlEmbedElement setWidthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CInt
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CInt] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @src@
srcSelector :: Selector '[] (Id NSString)
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector '[Id NSString] ()
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CInt
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CInt] ()
setWidthSelector = mkSelector "setWidth:"

