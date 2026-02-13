{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLPreElement@.
module ObjC.WebKit.DOMHTMLPreElement
  ( DOMHTMLPreElement
  , IsDOMHTMLPreElement(..)
  , width
  , setWidth
  , wrap
  , setWrap
  , setWidthSelector
  , setWrapSelector
  , widthSelector
  , wrapSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- width@
width :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> IO CInt
width domhtmlPreElement =
  sendMessage domhtmlPreElement widthSelector

-- | @- setWidth:@
setWidth :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> CInt -> IO ()
setWidth domhtmlPreElement value =
  sendMessage domhtmlPreElement setWidthSelector value

-- | @- wrap@
wrap :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> IO Bool
wrap domhtmlPreElement =
  sendMessage domhtmlPreElement wrapSelector

-- | @- setWrap:@
setWrap :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> Bool -> IO ()
setWrap domhtmlPreElement value =
  sendMessage domhtmlPreElement setWrapSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector '[] CInt
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CInt] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @wrap@
wrapSelector :: Selector '[] Bool
wrapSelector = mkSelector "wrap"

-- | @Selector@ for @setWrap:@
setWrapSelector :: Selector '[Bool] ()
setWrapSelector = mkSelector "setWrap:"

