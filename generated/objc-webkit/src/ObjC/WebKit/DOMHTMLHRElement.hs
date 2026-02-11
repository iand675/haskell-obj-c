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
  , setAlignSelector
  , noShadeSelector
  , setNoShadeSelector
  , sizeSelector
  , setSizeSelector
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
align :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO (Id NSString)
align domhtmlhrElement  =
  sendMsg domhtmlhrElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLHRElement domhtmlhrElement, IsNSString value) => domhtmlhrElement -> value -> IO ()
setAlign domhtmlhrElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlhrElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- noShade@
noShade :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO Bool
noShade domhtmlhrElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlhrElement (mkSelector "noShade") retCULong []

-- | @- setNoShade:@
setNoShade :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> Bool -> IO ()
setNoShade domhtmlhrElement  value =
  sendMsg domhtmlhrElement (mkSelector "setNoShade:") retVoid [argCULong (if value then 1 else 0)]

-- | @- size@
size :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO (Id NSString)
size domhtmlhrElement  =
  sendMsg domhtmlhrElement (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsDOMHTMLHRElement domhtmlhrElement, IsNSString value) => domhtmlhrElement -> value -> IO ()
setSize domhtmlhrElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlhrElement (mkSelector "setSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLHRElement domhtmlhrElement => domhtmlhrElement -> IO (Id NSString)
width domhtmlhrElement  =
  sendMsg domhtmlhrElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLHRElement domhtmlhrElement, IsNSString value) => domhtmlhrElement -> value -> IO ()
setWidth domhtmlhrElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlhrElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @noShade@
noShadeSelector :: Selector
noShadeSelector = mkSelector "noShade"

-- | @Selector@ for @setNoShade:@
setNoShadeSelector :: Selector
setNoShadeSelector = mkSelector "setNoShade:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

