{-# LANGUAGE DataKinds #-}
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
  , labelSelector
  , setDisabledSelector
  , setLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- disabled@
disabled :: IsDOMHTMLOptGroupElement domhtmlOptGroupElement => domhtmlOptGroupElement -> IO Bool
disabled domhtmlOptGroupElement =
  sendMessage domhtmlOptGroupElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLOptGroupElement domhtmlOptGroupElement => domhtmlOptGroupElement -> Bool -> IO ()
setDisabled domhtmlOptGroupElement value =
  sendMessage domhtmlOptGroupElement setDisabledSelector value

-- | @- label@
label :: IsDOMHTMLOptGroupElement domhtmlOptGroupElement => domhtmlOptGroupElement -> IO (Id NSString)
label domhtmlOptGroupElement =
  sendMessage domhtmlOptGroupElement labelSelector

-- | @- setLabel:@
setLabel :: (IsDOMHTMLOptGroupElement domhtmlOptGroupElement, IsNSString value) => domhtmlOptGroupElement -> value -> IO ()
setLabel domhtmlOptGroupElement value =
  sendMessage domhtmlOptGroupElement setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

