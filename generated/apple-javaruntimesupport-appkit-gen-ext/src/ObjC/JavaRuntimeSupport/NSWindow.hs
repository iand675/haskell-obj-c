{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWindow@.
module ObjC.JavaRuntimeSupport.NSWindow
  ( NSWindow
  , IsNSWindow(..)
  , javaAddToOrderingGroup
  , javaRemoveFromOrderingGroup
  , javaAddToOrderingGroupSelector
  , javaRemoveFromOrderingGroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- javaAddToOrderingGroup:@
javaAddToOrderingGroup :: (IsNSWindow nsWindow, IsNSWindow ownedWindow) => nsWindow -> ownedWindow -> IO ()
javaAddToOrderingGroup nsWindow ownedWindow =
  sendMessage nsWindow javaAddToOrderingGroupSelector (toNSWindow ownedWindow)

-- | @- javaRemoveFromOrderingGroup:@
javaRemoveFromOrderingGroup :: (IsNSWindow nsWindow, IsNSWindow ownedWindow) => nsWindow -> ownedWindow -> IO ()
javaRemoveFromOrderingGroup nsWindow ownedWindow =
  sendMessage nsWindow javaRemoveFromOrderingGroupSelector (toNSWindow ownedWindow)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaAddToOrderingGroup:@
javaAddToOrderingGroupSelector :: Selector '[Id NSWindow] ()
javaAddToOrderingGroupSelector = mkSelector "javaAddToOrderingGroup:"

-- | @Selector@ for @javaRemoveFromOrderingGroup:@
javaRemoveFromOrderingGroupSelector :: Selector '[Id NSWindow] ()
javaRemoveFromOrderingGroupSelector = mkSelector "javaRemoveFromOrderingGroup:"

