{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | File handle serial port attachment.
--
-- VZFileHandleSerialPortAttachment defines a serial port attachment from a file handle.    Data written to fileHandleForReading goes to the guest. Data sent from the guest appears on fileHandleForWriting.
--
-- Generated bindings for @VZFileHandleSerialPortAttachment@.
module ObjC.Virtualization.VZFileHandleSerialPortAttachment
  ( VZFileHandleSerialPortAttachment
  , IsVZFileHandleSerialPortAttachment(..)
  , new
  , init_
  , initWithFileHandleForReading_fileHandleForWriting
  , fileHandleForReading
  , fileHandleForWriting
  , fileHandleForReadingSelector
  , fileHandleForWritingSelector
  , initSelector
  , initWithFileHandleForReading_fileHandleForWritingSelector
  , newSelector


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
new :: IO (Id VZFileHandleSerialPortAttachment)
new  =
  do
    cls' <- getRequiredClass "VZFileHandleSerialPortAttachment"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment => vzFileHandleSerialPortAttachment -> IO (Id VZFileHandleSerialPortAttachment)
init_ vzFileHandleSerialPortAttachment =
  sendOwnedMessage vzFileHandleSerialPortAttachment initSelector

-- | Initialize the VZFileHandleSerialPortAttachment from file handles.
--
-- @fileHandleForReading@ — File handle for reading from the file.
--
-- @fileHandleForWriting@ — File handle for writing to the file.
--
-- Each file handle must either be nil or have a valid file descriptor.
--
-- ObjC selector: @- initWithFileHandleForReading:fileHandleForWriting:@
initWithFileHandleForReading_fileHandleForWriting :: (IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment, IsNSFileHandle fileHandleForReading, IsNSFileHandle fileHandleForWriting) => vzFileHandleSerialPortAttachment -> fileHandleForReading -> fileHandleForWriting -> IO (Id VZFileHandleSerialPortAttachment)
initWithFileHandleForReading_fileHandleForWriting vzFileHandleSerialPortAttachment fileHandleForReading fileHandleForWriting =
  sendOwnedMessage vzFileHandleSerialPortAttachment initWithFileHandleForReading_fileHandleForWritingSelector (toNSFileHandle fileHandleForReading) (toNSFileHandle fileHandleForWriting)

-- | File handle for reading from the file.
--
-- Data written to fileHandleForReading goes to the guest.
--
-- ObjC selector: @- fileHandleForReading@
fileHandleForReading :: IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment => vzFileHandleSerialPortAttachment -> IO (Id NSFileHandle)
fileHandleForReading vzFileHandleSerialPortAttachment =
  sendMessage vzFileHandleSerialPortAttachment fileHandleForReadingSelector

-- | File handle for writing to the file.
--
-- Data sent from the guest appears on fileHandleForWriting.
--
-- ObjC selector: @- fileHandleForWriting@
fileHandleForWriting :: IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment => vzFileHandleSerialPortAttachment -> IO (Id NSFileHandle)
fileHandleForWriting vzFileHandleSerialPortAttachment =
  sendMessage vzFileHandleSerialPortAttachment fileHandleForWritingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZFileHandleSerialPortAttachment)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZFileHandleSerialPortAttachment)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFileHandleForReading:fileHandleForWriting:@
initWithFileHandleForReading_fileHandleForWritingSelector :: Selector '[Id NSFileHandle, Id NSFileHandle] (Id VZFileHandleSerialPortAttachment)
initWithFileHandleForReading_fileHandleForWritingSelector = mkSelector "initWithFileHandleForReading:fileHandleForWriting:"

-- | @Selector@ for @fileHandleForReading@
fileHandleForReadingSelector :: Selector '[] (Id NSFileHandle)
fileHandleForReadingSelector = mkSelector "fileHandleForReading"

-- | @Selector@ for @fileHandleForWriting@
fileHandleForWritingSelector :: Selector '[] (Id NSFileHandle)
fileHandleForWritingSelector = mkSelector "fileHandleForWriting"

