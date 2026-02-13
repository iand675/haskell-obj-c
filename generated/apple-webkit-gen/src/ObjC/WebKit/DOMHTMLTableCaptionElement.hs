{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTableCaptionElement@.
module ObjC.WebKit.DOMHTMLTableCaptionElement
  ( DOMHTMLTableCaptionElement
  , IsDOMHTMLTableCaptionElement(..)
  , align
  , setAlign
  , alignSelector
  , setAlignSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- align@
align :: IsDOMHTMLTableCaptionElement domhtmlTableCaptionElement => domhtmlTableCaptionElement -> IO (Id NSString)
align domhtmlTableCaptionElement =
  sendMessage domhtmlTableCaptionElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableCaptionElement domhtmlTableCaptionElement, IsNSString value) => domhtmlTableCaptionElement -> value -> IO ()
setAlign domhtmlTableCaptionElement value =
  sendMessage domhtmlTableCaptionElement setAlignSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

