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
  , newSelector
  , initSelector
  , initWithFileHandleForReading_fileHandleForWritingSelector
  , fileHandleForReadingSelector
  , fileHandleForWritingSelector


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
new :: IO (Id VZFileHandleSerialPortAttachment)
new  =
  do
    cls' <- getRequiredClass "VZFileHandleSerialPortAttachment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment => vzFileHandleSerialPortAttachment -> IO (Id VZFileHandleSerialPortAttachment)
init_ vzFileHandleSerialPortAttachment  =
  sendMsg vzFileHandleSerialPortAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithFileHandleForReading_fileHandleForWriting vzFileHandleSerialPortAttachment  fileHandleForReading fileHandleForWriting =
withObjCPtr fileHandleForReading $ \raw_fileHandleForReading ->
  withObjCPtr fileHandleForWriting $ \raw_fileHandleForWriting ->
      sendMsg vzFileHandleSerialPortAttachment (mkSelector "initWithFileHandleForReading:fileHandleForWriting:") (retPtr retVoid) [argPtr (castPtr raw_fileHandleForReading :: Ptr ()), argPtr (castPtr raw_fileHandleForWriting :: Ptr ())] >>= ownedObject . castPtr

-- | File handle for reading from the file.
--
-- Data written to fileHandleForReading goes to the guest.
--
-- ObjC selector: @- fileHandleForReading@
fileHandleForReading :: IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment => vzFileHandleSerialPortAttachment -> IO (Id NSFileHandle)
fileHandleForReading vzFileHandleSerialPortAttachment  =
  sendMsg vzFileHandleSerialPortAttachment (mkSelector "fileHandleForReading") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | File handle for writing to the file.
--
-- Data sent from the guest appears on fileHandleForWriting.
--
-- ObjC selector: @- fileHandleForWriting@
fileHandleForWriting :: IsVZFileHandleSerialPortAttachment vzFileHandleSerialPortAttachment => vzFileHandleSerialPortAttachment -> IO (Id NSFileHandle)
fileHandleForWriting vzFileHandleSerialPortAttachment  =
  sendMsg vzFileHandleSerialPortAttachment (mkSelector "fileHandleForWriting") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFileHandleForReading:fileHandleForWriting:@
initWithFileHandleForReading_fileHandleForWritingSelector :: Selector
initWithFileHandleForReading_fileHandleForWritingSelector = mkSelector "initWithFileHandleForReading:fileHandleForWriting:"

-- | @Selector@ for @fileHandleForReading@
fileHandleForReadingSelector :: Selector
fileHandleForReadingSelector = mkSelector "fileHandleForReading"

-- | @Selector@ for @fileHandleForWriting@
fileHandleForWritingSelector :: Selector
fileHandleForWritingSelector = mkSelector "fileHandleForWriting"

