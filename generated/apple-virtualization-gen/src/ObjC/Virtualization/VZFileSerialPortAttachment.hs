{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | File serial port attachment.
--
-- VZFileSerialPortAttachment defines a serial port attachment from a file.    Any data sent by the guest on the serial interface is written to the file.    No data is sent to the guest over serial with this attachment.
--
-- Generated bindings for @VZFileSerialPortAttachment@.
module ObjC.Virtualization.VZFileSerialPortAttachment
  ( VZFileSerialPortAttachment
  , IsVZFileSerialPortAttachment(..)
  , new
  , init_
  , initWithURL_append_error
  , url
  , append
  , appendSelector
  , initSelector
  , initWithURL_append_errorSelector
  , newSelector
  , urlSelector


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
new :: IO (Id VZFileSerialPortAttachment)
new  =
  do
    cls' <- getRequiredClass "VZFileSerialPortAttachment"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZFileSerialPortAttachment vzFileSerialPortAttachment => vzFileSerialPortAttachment -> IO (Id VZFileSerialPortAttachment)
init_ vzFileSerialPortAttachment =
  sendOwnedMessage vzFileSerialPortAttachment initSelector

-- | Initialize the VZFileSerialPortAttachment from a URL of a file.
--
-- @url@ — The URL of the file for the attachment on the local file system.
--
-- @shouldAppend@ — True if the file should be opened in append mode, false otherwise.    When a file is opened in append mode, writing to that file will append to the end of it.
--
-- @error@ — If not nil, used to report errors if initialization fails.
--
-- Returns: A newly initialized VZFileSerialPortAttachment. If an error was encountered returns @nil,@ and @error@ contains the error.
--
-- ObjC selector: @- initWithURL:append:error:@
initWithURL_append_error :: (IsVZFileSerialPortAttachment vzFileSerialPortAttachment, IsNSURL url, IsNSError error_) => vzFileSerialPortAttachment -> url -> Bool -> error_ -> IO (Id VZFileSerialPortAttachment)
initWithURL_append_error vzFileSerialPortAttachment url shouldAppend error_ =
  sendOwnedMessage vzFileSerialPortAttachment initWithURL_append_errorSelector (toNSURL url) shouldAppend (toNSError error_)

-- | The URL of the file for the attachment on the local file system.
--
-- ObjC selector: @- URL@
url :: IsVZFileSerialPortAttachment vzFileSerialPortAttachment => vzFileSerialPortAttachment -> IO (Id NSURL)
url vzFileSerialPortAttachment =
  sendMessage vzFileSerialPortAttachment urlSelector

-- | True if the file should be opened in append mode, false otherwise.
--
-- ObjC selector: @- append@
append :: IsVZFileSerialPortAttachment vzFileSerialPortAttachment => vzFileSerialPortAttachment -> IO Bool
append vzFileSerialPortAttachment =
  sendMessage vzFileSerialPortAttachment appendSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZFileSerialPortAttachment)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZFileSerialPortAttachment)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:append:error:@
initWithURL_append_errorSelector :: Selector '[Id NSURL, Bool, Id NSError] (Id VZFileSerialPortAttachment)
initWithURL_append_errorSelector = mkSelector "initWithURL:append:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @append@
appendSelector :: Selector '[] Bool
appendSelector = mkSelector "append"

