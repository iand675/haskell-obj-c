{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHRElement@.
module ObjC.WebKit.DOMHTMLHRElement
  ( DOMHTMLHRElement
  , IsDOMHTMLHRElement(..)
  , align
  , setAlign
  , noShade
  , setNoShade
  , size
  , setSize
  , width
  , setWidth
  , alignSelector
  , noShadeSelector
  , setAlignSelector
  , setNoShadeSelector
  , setSizeSelector
  , setWidthSelector
  , sizeSelector
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
align :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO (Id NSString)
align domhtmlhrElement =
  sendMessage domhtmlhrElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLHRElement domhtmlhrElement, IsNSString value) => domhtmlhrElement -> value -> IO ()
setAlign domhtmlhrElement value =
  sendMessage domhtmlhrElement setAlignSelector (toNSString value)

-- | @- noShade@
noShade :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO Bool
noShade domhtmlhrElement =
  sendMessage domhtmlhrElement noShadeSelector

-- | @- setNoShade:@
setNoShade :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> Bool -> IO ()
setNoShade domhtmlhrElement value =
  sendMessage domhtmlhrElement setNoShadeSelector value

-- | @- size@
size :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO (Id NSString)
size domhtmlhrElement =
  sendMessage domhtmlhrElement sizeSelector

-- | @- setSize:@
setSize :: (IsDOMHTMLHRElement domhtmlhrElement, IsNSString value) => domhtmlhrElement -> value -> IO ()
setSize domhtmlhrElement value =
  sendMessage domhtmlhrElement setSizeSelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO (Id NSString)
width domhtmlhrElement =
  sendMessage domhtmlhrElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLHRElement domhtmlhrElement, IsNSString value) => domhtmlhrElement -> value -> IO ()
setWidth domhtmlhrElement value =
  sendMessage domhtmlhrElement setWidthSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @noShade@
noShadeSelector :: Selector '[] Bool
noShadeSelector = mkSelector "noShade"

-- | @Selector@ for @setNoShade:@
setNoShadeSelector :: Selector '[Bool] ()
setNoShadeSelector = mkSelector "setNoShade:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] (Id NSString)
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[Id NSString] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSString)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSString] ()
setWidthSelector = mkSelector "setWidth:"

