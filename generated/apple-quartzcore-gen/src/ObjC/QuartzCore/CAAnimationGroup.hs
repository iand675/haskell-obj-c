{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Animation subclass for grouped animations. *
--
-- Generated bindings for @CAAnimationGroup@.
module ObjC.QuartzCore.CAAnimationGroup
  ( CAAnimationGroup
  , IsCAAnimationGroup(..)
  , animations
  , setAnimations
  , animationsSelector
  , setAnimationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- animations@
animations :: IsCAAnimationGroup caAnimationGroup => caAnimationGroup -> IO (Id NSArray)
animations caAnimationGroup =
  sendMessage caAnimationGroup animationsSelector

-- | @- setAnimations:@
setAnimations :: (IsCAAnimationGroup caAnimationGroup, IsNSArray value) => caAnimationGroup -> value -> IO ()
setAnimations caAnimationGroup value =
  sendMessage caAnimationGroup setAnimationsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animations@
animationsSelector :: Selector '[] (Id NSArray)
animationsSelector = mkSelector "animations"

-- | @Selector@ for @setAnimations:@
setAnimationsSelector :: Selector '[Id NSArray] ()
setAnimationsSelector = mkSelector "setAnimations:"

