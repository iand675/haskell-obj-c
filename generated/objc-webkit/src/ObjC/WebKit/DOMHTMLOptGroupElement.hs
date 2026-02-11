{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLOptGroupElement@.
module ObjC.WebKit.DOMHTMLOptGroupElement
  ( DOMHTMLOptGroupElement
  , IsDOMHTMLOptGroupElement(..)
  , disabled
  , setDisabled
  , label
  , setLabel
  , disabledSelector
  , setDisabledSelector
  , labelSelector
  , setLabelSelector


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

-- | @- disabled@
disabled :: IsDOMHTMLOptGroupElement domhtmlOptGroupElement => domhtmlOptGroupElement -> IO Bool
disabled domhtmlOptGroupElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlOptGroupElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLOptGroupElement domhtmlOptGroupElement => domhtmlOptGroupElement -> Bool -> IO ()
setDisabled domhtmlOptGroupElement  value =
  sendMsg domhtmlOptGroupElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- label@
label :: IsDOMHTMLOptGroupElement domhtmlOptGroupElement => domhtmlOptGroupElement -> IO (Id NSString)
label domhtmlOptGroupElement  =
  sendMsg domhtmlOptGroupElement (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsDOMHTMLOptGroupElement domhtmlOptGroupElement, IsNSString value) => domhtmlOptGroupElement -> value -> IO ()
setLabel domhtmlOptGroupElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlOptGroupElement (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

