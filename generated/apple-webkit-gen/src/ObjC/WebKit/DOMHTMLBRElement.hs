{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLBRElement@.
module ObjC.WebKit.DOMHTMLBRElement
  ( DOMHTMLBRElement
  , IsDOMHTMLBRElement(..)
  , clear
  , setClear
  , clearSelector
  , setClearSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- clear@
clear :: IsDOMHTMLBRElement domhtmlbrElement => domhtmlbrElement -> IO (Id NSString)
clear domhtmlbrElement =
  sendMessage domhtmlbrElement clearSelector

-- | @- setClear:@
setClear :: (IsDOMHTMLBRElement domhtmlbrElement, IsNSString value) => domhtmlbrElement -> value -> IO ()
setClear domhtmlbrElement value =
  sendMessage domhtmlbrElement setClearSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clear@
clearSelector :: Selector '[] (Id NSString)
clearSelector = mkSelector "clear"

-- | @Selector@ for @setClear:@
setClearSelector :: Selector '[Id NSString] ()
setClearSelector = mkSelector "setClear:"

