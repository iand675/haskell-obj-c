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
  , initSelector
  , newSelector
  , addAnimationBlockSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlayTransitionContext skOverlayTransitionContext => skOverlayTransitionContext -> IO (Id SKOverlayTransitionContext)
init_ skOverlayTransitionContext  =
  sendMsg skOverlayTransitionContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SKOverlayTransitionContext)
new  =
  do
    cls' <- getRequiredClass "SKOverlayTransitionContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Adds an animation that will be synchronized with an overlay's presentation/dismissal.
--
-- ObjC selector: @- addAnimationBlock:@
addAnimationBlock :: IsSKOverlayTransitionContext skOverlayTransitionContext => skOverlayTransitionContext -> Ptr () -> IO ()
addAnimationBlock skOverlayTransitionContext  block =
  sendMsg skOverlayTransitionContext (mkSelector "addAnimationBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @addAnimationBlock:@
addAnimationBlockSelector :: Selector
addAnimationBlockSelector = mkSelector "addAnimationBlock:"

