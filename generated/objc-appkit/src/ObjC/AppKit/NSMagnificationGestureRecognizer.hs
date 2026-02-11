{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMagnificationGestureRecognizer@.
module ObjC.AppKit.NSMagnificationGestureRecognizer
  ( NSMagnificationGestureRecognizer
  , IsNSMagnificationGestureRecognizer(..)
  , magnification
  , setMagnification
  , magnificationSelector
  , setMagnificationSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- magnification@
magnification :: IsNSMagnificationGestureRecognizer nsMagnificationGestureRecognizer => nsMagnificationGestureRecognizer -> IO CDouble
magnification nsMagnificationGestureRecognizer  =
  sendMsg nsMagnificationGestureRecognizer (mkSelector "magnification") retCDouble []

-- | @- setMagnification:@
setMagnification :: IsNSMagnificationGestureRecognizer nsMagnificationGestureRecognizer => nsMagnificationGestureRecognizer -> CDouble -> IO ()
setMagnification nsMagnificationGestureRecognizer  value =
  sendMsg nsMagnificationGestureRecognizer (mkSelector "setMagnification:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @magnification@
magnificationSelector :: Selector
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector
setMagnificationSelector = mkSelector "setMagnification:"

