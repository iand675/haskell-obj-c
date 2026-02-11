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
  , viewAnimationsSelector
  , setViewAnimationsSelector


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

-- | @- initWithViewAnimations:@
initWithViewAnimations :: (IsNSViewAnimation nsViewAnimation, IsNSArray viewAnimations) => nsViewAnimation -> viewAnimations -> IO (Id NSViewAnimation)
initWithViewAnimations nsViewAnimation  viewAnimations =
withObjCPtr viewAnimations $ \raw_viewAnimations ->
    sendMsg nsViewAnimation (mkSelector "initWithViewAnimations:") (retPtr retVoid) [argPtr (castPtr raw_viewAnimations :: Ptr ())] >>= ownedObject . castPtr

-- | @- viewAnimations@
viewAnimations :: IsNSViewAnimation nsViewAnimation => nsViewAnimation -> IO (Id NSArray)
viewAnimations nsViewAnimation  =
  sendMsg nsViewAnimation (mkSelector "viewAnimations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setViewAnimations:@
setViewAnimations :: (IsNSViewAnimation nsViewAnimation, IsNSArray value) => nsViewAnimation -> value -> IO ()
setViewAnimations nsViewAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsViewAnimation (mkSelector "setViewAnimations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithViewAnimations:@
initWithViewAnimationsSelector :: Selector
initWithViewAnimationsSelector = mkSelector "initWithViewAnimations:"

-- | @Selector@ for @viewAnimations@
viewAnimationsSelector :: Selector
viewAnimationsSelector = mkSelector "viewAnimations"

-- | @Selector@ for @setViewAnimations:@
setViewAnimationsSelector :: Selector
setViewAnimationsSelector = mkSelector "setViewAnimations:"

