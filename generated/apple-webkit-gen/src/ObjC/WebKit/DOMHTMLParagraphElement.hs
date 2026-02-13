{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLParagraphElement@.
module ObjC.WebKit.DOMHTMLParagraphElement
  ( DOMHTMLParagraphElement
  , IsDOMHTMLParagraphElement(..)
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
align :: IsDOMHTMLParagraphElement domhtmlParagraphElement => domhtmlParagraphElement -> IO (Id NSString)
align domhtmlParagraphElement =
  sendMessage domhtmlParagraphElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLParagraphElement domhtmlParagraphElement, IsNSString value) => domhtmlParagraphElement -> value -> IO ()
setAlign domhtmlParagraphElement value =
  sendMessage domhtmlParagraphElement setAlignSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

