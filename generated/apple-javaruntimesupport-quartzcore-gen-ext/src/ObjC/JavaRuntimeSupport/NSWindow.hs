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

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- javaAddToOrderingGroup:@
javaAddToOrderingGroup :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
javaAddToOrderingGroup nsWindow  ownedWindow =
    sendMsg nsWindow (mkSelector "javaAddToOrderingGroup:") retVoid [argPtr (castPtr (unRawId ownedWindow) :: Ptr ())]

-- | @- javaRemoveFromOrderingGroup:@
javaRemoveFromOrderingGroup :: IsNSWindow nsWindow => nsWindow -> RawId -> IO ()
javaRemoveFromOrderingGroup nsWindow  ownedWindow =
    sendMsg nsWindow (mkSelector "javaRemoveFromOrderingGroup:") retVoid [argPtr (castPtr (unRawId ownedWindow) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaAddToOrderingGroup:@
javaAddToOrderingGroupSelector :: Selector
javaAddToOrderingGroupSelector = mkSelector "javaAddToOrderingGroup:"

-- | @Selector@ for @javaRemoveFromOrderingGroup:@
javaRemoveFromOrderingGroupSelector :: Selector
javaRemoveFromOrderingGroupSelector = mkSelector "javaRemoveFromOrderingGroup:"

