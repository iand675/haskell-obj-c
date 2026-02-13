{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKOverlayTransitionContext@.
module ObjC.StoreKit.SKOverlayTransitionContext
  ( SKOverlayTransitionContext
  , IsSKOverlayTransitionContext(..)
  , init_
  , new
  , addAnimationBlock
  , addAnimationBlockSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlayTransitionContext skOverlayTransitionContext => skOverlayTransitionContext -> IO (Id SKOverlayTransitionContext)
init_ skOverlayTransitionContext =
  sendOwnedMessage skOverlayTransitionContext initSelector

-- | @+ new@
new :: IO (Id SKOverlayTransitionContext)
new  =
  do
    cls' <- getRequiredClass "SKOverlayTransitionContext"
    sendOwnedClassMessage cls' newSelector

-- | Adds an animation that will be synchronized with an overlay's presentation/dismissal.
--
-- ObjC selector: @- addAnimationBlock:@
addAnimationBlock :: IsSKOverlayTransitionContext skOverlayTransitionContext => skOverlayTransitionContext -> Ptr () -> IO ()
addAnimationBlock skOverlayTransitionContext block =
  sendMessage skOverlayTransitionContext addAnimationBlockSelector block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKOverlayTransitionContext)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SKOverlayTransitionContext)
newSelector = mkSelector "new"

-- | @Selector@ for @addAnimationBlock:@
addAnimationBlockSelector :: Selector '[Ptr ()] ()
addAnimationBlockSelector = mkSelector "addAnimationBlock:"

