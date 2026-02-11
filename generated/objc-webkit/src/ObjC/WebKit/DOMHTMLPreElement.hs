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
  , widthSelector
  , setWidthSelector
  , wrapSelector
  , setWrapSelector


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

-- | @- width@
width :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> IO CInt
width domhtmlPreElement  =
  sendMsg domhtmlPreElement (mkSelector "width") retCInt []

-- | @- setWidth:@
setWidth :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> CInt -> IO ()
setWidth domhtmlPreElement  value =
  sendMsg domhtmlPreElement (mkSelector "setWidth:") retVoid [argCInt (fromIntegral value)]

-- | @- wrap@
wrap :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> IO Bool
wrap domhtmlPreElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlPreElement (mkSelector "wrap") retCULong []

-- | @- setWrap:@
setWrap :: IsDOMHTMLPreElement domhtmlPreElement => domhtmlPreElement -> Bool -> IO ()
setWrap domhtmlPreElement  value =
  sendMsg domhtmlPreElement (mkSelector "setWrap:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @wrap@
wrapSelector :: Selector
wrapSelector = mkSelector "wrap"

-- | @Selector@ for @setWrap:@
setWrapSelector :: Selector
setWrapSelector = mkSelector "setWrap:"

