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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Pointer to a delegate object for the listener.
--
-- ObjC selector: @- delegate@
delegate :: IsVZVirtioSocketListener vzVirtioSocketListener => vzVirtioSocketListener -> IO RawId
delegate vzVirtioSocketListener  =
    fmap (RawId . castPtr) $ sendMsg vzVirtioSocketListener (mkSelector "delegate") (retPtr retVoid) []

-- | Pointer to a delegate object for the listener.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsVZVirtioSocketListener vzVirtioSocketListener => vzVirtioSocketListener -> RawId -> IO ()
setDelegate vzVirtioSocketListener  value =
    sendMsg vzVirtioSocketListener (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

