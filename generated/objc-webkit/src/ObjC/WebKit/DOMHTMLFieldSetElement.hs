{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLFieldSetElement@.
module ObjC.WebKit.DOMHTMLFieldSetElement
  ( DOMHTMLFieldSetElement
  , IsDOMHTMLFieldSetElement(..)
  , form
  , formSelector


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

-- | @- form@
form :: IsDOMHTMLFieldSetElement domhtmlFieldSetElement => domhtmlFieldSetElement -> IO (Id DOMHTMLFormElement)
form domhtmlFieldSetElement  =
  sendMsg domhtmlFieldSetElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

