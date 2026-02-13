{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The VZVirtioSocketListener object represents a listener for the Virtio socket device.
--
-- The listener encompasses a VZVirtioSocketListenerDelegate object.    VZVirtioSocketListener is used with VZVirtioSocketDevice to listen to a particular port.    The delegate is used when a guest connects to a port associated with the listener.
--
-- See: VZVirtioSocketDevice
--
-- See: VZVirtioSocketListenerDelegate
--
-- Generated bindings for @VZVirtioSocketListener@.
module ObjC.Virtualization.VZVirtioSocketListener
  ( VZVirtioSocketListener
  , IsVZVirtioSocketListener(..)
  , delegate
  , setDelegate
  , delegateSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Pointer to a delegate object for the listener.
--
-- ObjC selector: @- delegate@
delegate :: IsVZVirtioSocketListener vzVirtioSocketListener => vzVirtioSocketListener -> IO RawId
delegate vzVirtioSocketListener =
  sendMessage vzVirtioSocketListener delegateSelector

-- | Pointer to a delegate object for the listener.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsVZVirtioSocketListener vzVirtioSocketListener => vzVirtioSocketListener -> RawId -> IO ()
setDelegate vzVirtioSocketListener value =
  sendMessage vzVirtioSocketListener setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

