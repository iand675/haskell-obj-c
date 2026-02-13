{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLMarqueeElement@.
module ObjC.WebKit.DOMHTMLMarqueeElement
  ( DOMHTMLMarqueeElement
  , IsDOMHTMLMarqueeElement(..)
  , start
  , stop
  , startSelector
  , stopSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- start@
start :: IsDOMHTMLMarqueeElement domhtmlMarqueeElement => domhtmlMarqueeElement -> IO ()
start domhtmlMarqueeElement =
  sendMessage domhtmlMarqueeElement startSelector

-- | @- stop@
stop :: IsDOMHTMLMarqueeElement domhtmlMarqueeElement => domhtmlMarqueeElement -> IO ()
stop domhtmlMarqueeElement =
  sendMessage domhtmlMarqueeElement stopSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

