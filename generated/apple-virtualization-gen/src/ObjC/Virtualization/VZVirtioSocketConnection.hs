{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The VZVirtioSocketConnection object represents a Virtio socket device's connection.
--
-- The connection encompasses a source port, destination port, and an associated file descriptor.
--
-- See: VZVirtioSocketDevice
--
-- Generated bindings for @VZVirtioSocketConnection@.
module ObjC.Virtualization.VZVirtioSocketConnection
  ( VZVirtioSocketConnection
  , IsVZVirtioSocketConnection(..)
  , new
  , init_
  , close
  , destinationPort
  , sourcePort
  , fileDescriptor
  , closeSelector
  , destinationPortSelector
  , fileDescriptorSelector
  , initSelector
  , newSelector
  , sourcePortSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZVirtioSocketConnection)
new  =
  do
    cls' <- getRequiredClass "VZVirtioSocketConnection"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO (Id VZVirtioSocketConnection)
init_ vzVirtioSocketConnection =
  sendOwnedMessage vzVirtioSocketConnection initSelector

-- | Close the file descriptor that's associated with the socket.
--
-- ObjC selector: @- close@
close :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO ()
close vzVirtioSocketConnection =
  sendMessage vzVirtioSocketConnection closeSelector

-- | The destination port number of the connection.
--
-- ObjC selector: @- destinationPort@
destinationPort :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO CUInt
destinationPort vzVirtioSocketConnection =
  sendMessage vzVirtioSocketConnection destinationPortSelector

-- | The source port number of the connection.
--
-- ObjC selector: @- sourcePort@
sourcePort :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO CUInt
sourcePort vzVirtioSocketConnection =
  sendMessage vzVirtioSocketConnection sourcePortSelector

-- | The file descriptor associated with the socket.
--
-- Data is sent by writing to the file descriptor.    Data is received by reading from the file descriptor.    A file descriptor of -1 indicates a closed connection.
--
-- The file descriptor is owned by the VZVirtioSocketConnection. It is automatically closed when the object is destroyed.
--
-- ObjC selector: @- fileDescriptor@
fileDescriptor :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO CInt
fileDescriptor vzVirtioSocketConnection =
  sendMessage vzVirtioSocketConnection fileDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioSocketConnection)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSocketConnection)
initSelector = mkSelector "init"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @destinationPort@
destinationPortSelector :: Selector '[] CUInt
destinationPortSelector = mkSelector "destinationPort"

-- | @Selector@ for @sourcePort@
sourcePortSelector :: Selector '[] CUInt
sourcePortSelector = mkSelector "sourcePort"

-- | @Selector@ for @fileDescriptor@
fileDescriptorSelector :: Selector '[] CInt
fileDescriptorSelector = mkSelector "fileDescriptor"

