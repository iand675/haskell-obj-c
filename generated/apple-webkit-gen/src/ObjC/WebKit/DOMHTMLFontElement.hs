{-# LANGUAGE DataKinds #-}
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
color :: IsDOMHTMLFontElement domhtmlFontElement => domhtmlFontElement -> IO (Id NSString)
color domhtmlFontElement =
  sendMessage domhtmlFontElement colorSelector

-- | @- setColor:@
setColor :: (IsDOMHTMLFontElement domhtmlFontElement, IsNSString value) => domhtmlFontElement -> value -> IO ()
setColor domhtmlFontElement value =
  sendMessage domhtmlFontElement setColorSelector (toNSString value)

-- | @- face@
face :: IsDOMHTMLFontElement domhtmlFontElement => domhtmlFontElement -> IO (Id NSString)
face domhtmlFontElement =
  sendMessage domhtmlFontElement faceSelector

-- | @- setFace:@
setFace :: (IsDOMHTMLFontElement domhtmlFontElement, IsNSString value) => domhtmlFontElement -> value -> IO ()
setFace domhtmlFontElement value =
  sendMessage domhtmlFontElement setFaceSelector (toNSString value)

-- | @- size@
size :: IsDOMHTMLFontElement domhtmlFontElement => domhtmlFontElement -> IO (Id NSString)
size domhtmlFontElement =
  sendMessage domhtmlFontElement sizeSelector

-- | @- setSize:@
setSize :: (IsDOMHTMLFontElement domhtmlFontElement, IsNSString value) => domhtmlFontElement -> value -> IO ()
setSize domhtmlFontElement value =
  sendMessage domhtmlFontElement setSizeSelector (toNSString value)

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

