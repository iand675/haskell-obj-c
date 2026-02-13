{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSViewAnimation@.
module ObjC.AppKit.NSViewAnimation
  ( NSViewAnimation
  , IsNSViewAnimation(..)
  , initWithViewAnimations
  , viewAnimations
  , setViewAnimations
  , initWithViewAnimationsSelector
  , setViewAnimationsSelector
  , viewAnimationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithViewAnimations:@
initWithViewAnimations :: (IsNSViewAnimation nsViewAnimation, IsNSArray viewAnimations) => nsViewAnimation -> viewAnimations -> IO (Id NSViewAnimation)
initWithViewAnimations nsViewAnimation viewAnimations =
  sendOwnedMessage nsViewAnimation initWithViewAnimationsSelector (toNSArray viewAnimations)

-- | @- viewAnimations@
viewAnimations :: IsNSViewAnimation nsViewAnimation => nsViewAnimation -> IO (Id NSArray)
viewAnimations nsViewAnimation =
  sendMessage nsViewAnimation viewAnimationsSelector

-- | @- setViewAnimations:@
setViewAnimations :: (IsNSViewAnimation nsViewAnimation, IsNSArray value) => nsViewAnimation -> value -> IO ()
setViewAnimations nsViewAnimation value =
  sendMessage nsViewAnimation setViewAnimationsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithViewAnimations:@
initWithViewAnimationsSelector :: Selector '[Id NSArray] (Id NSViewAnimation)
initWithViewAnimationsSelector = mkSelector "initWithViewAnimations:"

-- | @Selector@ for @viewAnimations@
viewAnimationsSelector :: Selector '[] (Id NSArray)
viewAnimationsSelector = mkSelector "viewAnimations"

-- | @Selector@ for @setViewAnimations:@
setViewAnimationsSelector :: Selector '[Id NSArray] ()
setViewAnimationsSelector = mkSelector "setViewAnimations:"

