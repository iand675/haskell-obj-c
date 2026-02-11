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
  , newSelector
  , initSelector
  , initWithURL_append_errorSelector
  , urlSelector
  , appendSelector


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
new :: IO (Id VZFileSerialPortAttachment)
new  =
  do
    cls' <- getRequiredClass "VZFileSerialPortAttachment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZFileSerialPortAttachment vzFileSerialPortAttachment => vzFileSerialPortAttachment -> IO (Id VZFileSerialPortAttachment)
init_ vzFileSerialPortAttachment  =
  sendMsg vzFileSerialPortAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithURL_append_error vzFileSerialPortAttachment  url shouldAppend error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vzFileSerialPortAttachment (mkSelector "initWithURL:append:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if shouldAppend then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | The URL of the file for the attachment on the local file system.
--
-- ObjC selector: @- URL@
url :: IsVZFileSerialPortAttachment vzFileSerialPortAttachment => vzFileSerialPortAttachment -> IO (Id NSURL)
url vzFileSerialPortAttachment  =
  sendMsg vzFileSerialPortAttachment (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | True if the file should be opened in append mode, false otherwise.
--
-- ObjC selector: @- append@
append :: IsVZFileSerialPortAttachment vzFileSerialPortAttachment => vzFileSerialPortAttachment -> IO Bool
append vzFileSerialPortAttachment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzFileSerialPortAttachment (mkSelector "append") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:append:error:@
initWithURL_append_errorSelector :: Selector
initWithURL_append_errorSelector = mkSelector "initWithURL:append:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @append@
appendSelector :: Selector
appendSelector = mkSelector "append"

