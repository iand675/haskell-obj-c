{-# LANGUAGE DataKinds #-}
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
  , faceSelector
  , setColorSelector
  , setFaceSelector
  , setSizeSelector
  , sizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- color@
color :: IsDOMHTMLBaseFontElement domhtmlBaseFontElement => domhtmlBaseFontElement -> IO (Id NSString)
color domhtmlBaseFontElement =
  sendMessage domhtmlBaseFontElement colorSelector

-- | @- setColor:@
setColor :: (IsDOMHTMLBaseFontElement domhtmlBaseFontElement, IsNSString value) => domhtmlBaseFontElement -> value -> IO ()
setColor domhtmlBaseFontElement value =
  sendMessage domhtmlBaseFontElement setColorSelector (toNSString value)

-- | @- face@
face :: IsDOMHTMLBaseFontElement domhtmlBaseFontElement => domhtmlBaseFontElement -> IO (Id NSString)
face domhtmlBaseFontElement =
  sendMessage domhtmlBaseFontElement faceSelector

-- | @- setFace:@
setFace :: (IsDOMHTMLBaseFontElement domhtmlBaseFontElement, IsNSString value) => domhtmlBaseFontElement -> value -> IO ()
setFace domhtmlBaseFontElement value =
  sendMessage domhtmlBaseFontElement setFaceSelector (toNSString value)

-- | @- size@
size :: IsDOMHTMLBaseFontElement domhtmlBaseFontElement => domhtmlBaseFontElement -> IO (Id NSString)
size domhtmlBaseFontElement =
  sendMessage domhtmlBaseFontElement sizeSelector

-- | @- setSize:@
setSize :: (IsDOMHTMLBaseFontElement domhtmlBaseFontElement, IsNSString value) => domhtmlBaseFontElement -> value -> IO ()
setSize domhtmlBaseFontElement value =
  sendMessage domhtmlBaseFontElement setSizeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSString)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSString] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @face@
faceSelector :: Selector '[] (Id NSString)
faceSelector = mkSelector "face"

-- | @Selector@ for @setFace:@
setFaceSelector :: Selector '[Id NSString] ()
setFaceSelector = mkSelector "setFace:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] (Id NSString)
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[Id NSString] ()
setSizeSelector = mkSelector "setSize:"

