{-# LANGUAGE DataKinds #-}
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
  , rowsSelector
  , setColsSelector
  , setRowsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cols@
cols :: IsDOMHTMLFrameSetElement domhtmlFrameSetElement => domhtmlFrameSetElement -> IO (Id NSString)
cols domhtmlFrameSetElement =
  sendMessage domhtmlFrameSetElement colsSelector

-- | @- setCols:@
setCols :: (IsDOMHTMLFrameSetElement domhtmlFrameSetElement, IsNSString value) => domhtmlFrameSetElement -> value -> IO ()
setCols domhtmlFrameSetElement value =
  sendMessage domhtmlFrameSetElement setColsSelector (toNSString value)

-- | @- rows@
rows :: IsDOMHTMLFrameSetElement domhtmlFrameSetElement => domhtmlFrameSetElement -> IO (Id NSString)
rows domhtmlFrameSetElement =
  sendMessage domhtmlFrameSetElement rowsSelector

-- | @- setRows:@
setRows :: (IsDOMHTMLFrameSetElement domhtmlFrameSetElement, IsNSString value) => domhtmlFrameSetElement -> value -> IO ()
setRows domhtmlFrameSetElement value =
  sendMessage domhtmlFrameSetElement setRowsSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cols@
colsSelector :: Selector '[] (Id NSString)
colsSelector = mkSelector "cols"

-- | @Selector@ for @setCols:@
setColsSelector :: Selector '[Id NSString] ()
setColsSelector = mkSelector "setCols:"

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] (Id NSString)
rowsSelector = mkSelector "rows"

-- | @Selector@ for @setRows:@
setRowsSelector :: Selector '[Id NSString] ()
setRowsSelector = mkSelector "setRows:"

