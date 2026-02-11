{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTitleElement@.
module ObjC.WebKit.DOMHTMLTitleElement
  ( DOMHTMLTitleElement
  , IsDOMHTMLTitleElement(..)
  , text
  , setText
  , textSelector
  , setTextSelector


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

-- | @- text@
text :: IsDOMHTMLTitleElement domhtmlTitleElement => domhtmlTitleElement -> IO (Id NSString)
text domhtmlTitleElement  =
  sendMsg domhtmlTitleElement (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setText:@
setText :: (IsDOMHTMLTitleElement domhtmlTitleElement, IsNSString value) => domhtmlTitleElement -> value -> IO ()
setText domhtmlTitleElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTitleElement (mkSelector "setText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector
setTextSelector = mkSelector "setText:"

