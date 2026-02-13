{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLDivElement@.
module ObjC.WebKit.DOMHTMLDivElement
  ( DOMHTMLDivElement
  , IsDOMHTMLDivElement(..)
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
align :: IsDOMHTMLDivElement domhtmlDivElement => domhtmlDivElement -> IO (Id NSString)
align domhtmlDivElement =
  sendMessage domhtmlDivElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLDivElement domhtmlDivElement, IsNSString value) => domhtmlDivElement -> value -> IO ()
setAlign domhtmlDivElement value =
  sendMessage domhtmlDivElement setAlignSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

