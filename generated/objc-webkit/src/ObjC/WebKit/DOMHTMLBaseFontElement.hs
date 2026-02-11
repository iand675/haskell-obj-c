{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLBaseFontElement@.
module ObjC.WebKit.DOMHTMLBaseFontElement
  ( DOMHTMLBaseFontElement
  , IsDOMHTMLBaseFontElement(..)
  , color
  , setColor
  , face
  , setFace
  , size
  , setSize
  , colorSelector
  , setColorSelector
  , faceSelector
  , setFaceSelector
  , sizeSelector
  , setSizeSelector


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

-- | @- color@
color :: IsDOMHTMLBaseFontElement domhtmlBaseFontElement => domhtmlBaseFontElement -> IO (Id NSString)
color domhtmlBaseFontElement  =
  sendMsg domhtmlBaseFontElement (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsDOMHTMLBaseFontElement domhtmlBaseFontElement, IsNSString value) => domhtmlBaseFontElement -> value -> IO ()
setColor domhtmlBaseFontElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBaseFontElement (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- face@
face :: IsDOMHTMLBaseFontElement domhtmlBaseFontElement => domhtmlBaseFontElement -> IO (Id NSString)
face domhtmlBaseFontElement  =
  sendMsg domhtmlBaseFontElement (mkSelector "face") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFace:@
setFace :: (IsDOMHTMLBaseFontElement domhtmlBaseFontElement, IsNSString value) => domhtmlBaseFontElement -> value -> IO ()
setFace domhtmlBaseFontElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBaseFontElement (mkSelector "setFace:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- size@
size :: IsDOMHTMLBaseFontElement domhtmlBaseFontElement => domhtmlBaseFontElement -> IO (Id NSString)
size domhtmlBaseFontElement  =
  sendMsg domhtmlBaseFontElement (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsDOMHTMLBaseFontElement domhtmlBaseFontElement, IsNSString value) => domhtmlBaseFontElement -> value -> IO ()
setSize domhtmlBaseFontElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBaseFontElement (mkSelector "setSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @face@
faceSelector :: Selector
faceSelector = mkSelector "face"

-- | @Selector@ for @setFace:@
setFaceSelector :: Selector
setFaceSelector = mkSelector "setFace:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

