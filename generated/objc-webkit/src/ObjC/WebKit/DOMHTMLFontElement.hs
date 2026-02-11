{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLFontElement@.
module ObjC.WebKit.DOMHTMLFontElement
  ( DOMHTMLFontElement
  , IsDOMHTMLFontElement(..)
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
color :: IsDOMHTMLFontElement domhtmlFontElement => domhtmlFontElement -> IO (Id NSString)
color domhtmlFontElement  =
  sendMsg domhtmlFontElement (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsDOMHTMLFontElement domhtmlFontElement, IsNSString value) => domhtmlFontElement -> value -> IO ()
setColor domhtmlFontElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFontElement (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- face@
face :: IsDOMHTMLFontElement domhtmlFontElement => domhtmlFontElement -> IO (Id NSString)
face domhtmlFontElement  =
  sendMsg domhtmlFontElement (mkSelector "face") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFace:@
setFace :: (IsDOMHTMLFontElement domhtmlFontElement, IsNSString value) => domhtmlFontElement -> value -> IO ()
setFace domhtmlFontElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFontElement (mkSelector "setFace:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- size@
size :: IsDOMHTMLFontElement domhtmlFontElement => domhtmlFontElement -> IO (Id NSString)
size domhtmlFontElement  =
  sendMsg domhtmlFontElement (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsDOMHTMLFontElement domhtmlFontElement, IsNSString value) => domhtmlFontElement -> value -> IO ()
setSize domhtmlFontElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFontElement (mkSelector "setSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

