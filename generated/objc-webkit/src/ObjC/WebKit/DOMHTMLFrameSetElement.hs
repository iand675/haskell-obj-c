{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLFrameSetElement@.
module ObjC.WebKit.DOMHTMLFrameSetElement
  ( DOMHTMLFrameSetElement
  , IsDOMHTMLFrameSetElement(..)
  , cols
  , setCols
  , rows
  , setRows
  , colsSelector
  , setColsSelector
  , rowsSelector
  , setRowsSelector


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

-- | @- cols@
cols :: IsDOMHTMLFrameSetElement domhtmlFrameSetElement => domhtmlFrameSetElement -> IO (Id NSString)
cols domhtmlFrameSetElement  =
  sendMsg domhtmlFrameSetElement (mkSelector "cols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCols:@
setCols :: (IsDOMHTMLFrameSetElement domhtmlFrameSetElement, IsNSString value) => domhtmlFrameSetElement -> value -> IO ()
setCols domhtmlFrameSetElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameSetElement (mkSelector "setCols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rows@
rows :: IsDOMHTMLFrameSetElement domhtmlFrameSetElement => domhtmlFrameSetElement -> IO (Id NSString)
rows domhtmlFrameSetElement  =
  sendMsg domhtmlFrameSetElement (mkSelector "rows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRows:@
setRows :: (IsDOMHTMLFrameSetElement domhtmlFrameSetElement, IsNSString value) => domhtmlFrameSetElement -> value -> IO ()
setRows domhtmlFrameSetElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameSetElement (mkSelector "setRows:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cols@
colsSelector :: Selector
colsSelector = mkSelector "cols"

-- | @Selector@ for @setCols:@
setColsSelector :: Selector
setColsSelector = mkSelector "setCols:"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

-- | @Selector@ for @setRows:@
setRowsSelector :: Selector
setRowsSelector = mkSelector "setRows:"

