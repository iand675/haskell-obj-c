{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- magnification@
magnification :: IsNSMagnificationGestureRecognizer nsMagnificationGestureRecognizer => nsMagnificationGestureRecognizer -> IO CDouble
magnification nsMagnificationGestureRecognizer =
  sendMessage nsMagnificationGestureRecognizer magnificationSelector

-- | @- setMagnification:@
setMagnification :: IsNSMagnificationGestureRecognizer nsMagnificationGestureRecognizer => nsMagnificationGestureRecognizer -> CDouble -> IO ()
setMagnification nsMagnificationGestureRecognizer value =
  sendMessage nsMagnificationGestureRecognizer setMagnificationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @magnification@
magnificationSelector :: Selector '[] CDouble
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector '[CDouble] ()
setMagnificationSelector = mkSelector "setMagnification:"

