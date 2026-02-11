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
  , newSelector
  , initSelector
  , closeSelector
  , destinationPortSelector
  , sourcePortSelector
  , fileDescriptorSelector


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

-- | @+ new@
new :: IO (Id VZVirtioSocketConnection)
new  =
  do
    cls' <- getRequiredClass "VZVirtioSocketConnection"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO (Id VZVirtioSocketConnection)
init_ vzVirtioSocketConnection  =
  sendMsg vzVirtioSocketConnection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Close the file descriptor that's associated with the socket.
--
-- ObjC selector: @- close@
close :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO ()
close vzVirtioSocketConnection  =
  sendMsg vzVirtioSocketConnection (mkSelector "close") retVoid []

-- | The destination port number of the connection.
--
-- ObjC selector: @- destinationPort@
destinationPort :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO CUInt
destinationPort vzVirtioSocketConnection  =
  sendMsg vzVirtioSocketConnection (mkSelector "destinationPort") retCUInt []

-- | The source port number of the connection.
--
-- ObjC selector: @- sourcePort@
sourcePort :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO CUInt
sourcePort vzVirtioSocketConnection  =
  sendMsg vzVirtioSocketConnection (mkSelector "sourcePort") retCUInt []

-- | The file descriptor associated with the socket.
--
-- Data is sent by writing to the file descriptor.    Data is received by reading from the file descriptor.    A file descriptor of -1 indicates a closed connection.
--
-- The file descriptor is owned by the VZVirtioSocketConnection. It is automatically closed when the object is destroyed.
--
-- ObjC selector: @- fileDescriptor@
fileDescriptor :: IsVZVirtioSocketConnection vzVirtioSocketConnection => vzVirtioSocketConnection -> IO CInt
fileDescriptor vzVirtioSocketConnection  =
  sendMsg vzVirtioSocketConnection (mkSelector "fileDescriptor") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @destinationPort@
destinationPortSelector :: Selector
destinationPortSelector = mkSelector "destinationPort"

-- | @Selector@ for @sourcePort@
sourcePortSelector :: Selector
sourcePortSelector = mkSelector "sourcePort"

-- | @Selector@ for @fileDescriptor@
fileDescriptorSelector :: Selector
fileDescriptorSelector = mkSelector "fileDescriptor"

