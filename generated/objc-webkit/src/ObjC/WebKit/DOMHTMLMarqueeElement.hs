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

-- | @- start@
start :: IsDOMHTMLMarqueeElement domhtmlMarqueeElement => domhtmlMarqueeElement -> IO ()
start domhtmlMarqueeElement  =
  sendMsg domhtmlMarqueeElement (mkSelector "start") retVoid []

-- | @- stop@
stop :: IsDOMHTMLMarqueeElement domhtmlMarqueeElement => domhtmlMarqueeElement -> IO ()
stop domhtmlMarqueeElement  =
  sendMsg domhtmlMarqueeElement (mkSelector "stop") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

