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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- animations@
animations :: IsCAAnimationGroup caAnimationGroup => caAnimationGroup -> IO (Id NSArray)
animations caAnimationGroup  =
  sendMsg caAnimationGroup (mkSelector "animations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAnimations:@
setAnimations :: (IsCAAnimationGroup caAnimationGroup, IsNSArray value) => caAnimationGroup -> value -> IO ()
setAnimations caAnimationGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg caAnimationGroup (mkSelector "setAnimations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animations@
animationsSelector :: Selector
animationsSelector = mkSelector "animations"

-- | @Selector@ for @setAnimations:@
setAnimationsSelector :: Selector
setAnimationsSelector = mkSelector "setAnimations:"

