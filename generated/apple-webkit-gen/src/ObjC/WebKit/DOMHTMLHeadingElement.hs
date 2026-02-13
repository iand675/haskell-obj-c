{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHeadingElement@.
module ObjC.WebKit.DOMHTMLHeadingElement
  ( DOMHTMLHeadingElement
  , IsDOMHTMLHeadingElement(..)
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
align :: IsDOMHTMLHeadingElement domhtmlHeadingElement => domhtmlHeadingElement -> IO (Id NSString)
align domhtmlHeadingElement =
  sendMessage domhtmlHeadingElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLHeadingElement domhtmlHeadingElement, IsNSString value) => domhtmlHeadingElement -> value -> IO ()
setAlign domhtmlHeadingElement value =
  sendMessage domhtmlHeadingElement setAlignSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

