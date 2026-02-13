{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTitleElement@.
module ObjC.WebKit.DOMHTMLTitleElement
  ( DOMHTMLTitleElement
  , IsDOMHTMLTitleElement(..)
  , text
  , setText
  , setTextSelector
  , textSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- text@
text :: IsDOMHTMLTitleElement domhtmlTitleElement => domhtmlTitleElement -> IO (Id NSString)
text domhtmlTitleElement =
  sendMessage domhtmlTitleElement textSelector

-- | @- setText:@
setText :: (IsDOMHTMLTitleElement domhtmlTitleElement, IsNSString value) => domhtmlTitleElement -> value -> IO ()
setText domhtmlTitleElement value =
  sendMessage domhtmlTitleElement setTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector '[Id NSString] ()
setTextSelector = mkSelector "setText:"

