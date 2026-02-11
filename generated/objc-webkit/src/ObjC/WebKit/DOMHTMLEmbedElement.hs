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
  , setAlignSelector
  , heightSelector
  , setHeightSelector
  , nameSelector
  , setNameSelector
  , srcSelector
  , setSrcSelector
  , typeSelector
  , setTypeSelector
  , widthSelector
  , setWidthSelector


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

-- | @- align@
align :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
align domhtmlEmbedElement  =
  sendMsg domhtmlEmbedElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setAlign domhtmlEmbedElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlEmbedElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO CInt
height domhtmlEmbedElement  =
  sendMsg domhtmlEmbedElement (mkSelector "height") retCInt []

-- | @- setHeight:@
setHeight :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> CInt -> IO ()
setHeight domhtmlEmbedElement  value =
  sendMsg domhtmlEmbedElement (mkSelector "setHeight:") retVoid [argCInt (fromIntegral value)]

-- | @- name@
name :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
name domhtmlEmbedElement  =
  sendMsg domhtmlEmbedElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setName domhtmlEmbedElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlEmbedElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- src@
src :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
src domhtmlEmbedElement  =
  sendMsg domhtmlEmbedElement (mkSelector "src") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSrc:@
setSrc :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setSrc domhtmlEmbedElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlEmbedElement (mkSelector "setSrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO (Id NSString)
type_ domhtmlEmbedElement  =
  sendMsg domhtmlEmbedElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLEmbedElement domhtmlEmbedElement, IsNSString value) => domhtmlEmbedElement -> value -> IO ()
setType domhtmlEmbedElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlEmbedElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> IO CInt
width domhtmlEmbedElement  =
  sendMsg domhtmlEmbedElement (mkSelector "width") retCInt []

-- | @- setWidth:@
setWidth :: IsDOMHTMLEmbedElement domhtmlEmbedElement => domhtmlEmbedElement -> CInt -> IO ()
setWidth domhtmlEmbedElement  value =
  sendMsg domhtmlEmbedElement (mkSelector "setWidth:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @src@
srcSelector :: Selector
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

