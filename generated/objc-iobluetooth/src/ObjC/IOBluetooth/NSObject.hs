{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.IOBluetooth.NSObject
  ( NSObject
  , IsNSObject(..)
  , fileTransferServicesConnectionComplete_error
  , fileTransferServicesDisconnectionComplete_error
  , fileTransferServicesAbortComplete_error
  , fileTransferServicesRemoveItemComplete_error_removedItem
  , fileTransferServicesCreateFolderComplete_error_folder
  , fileTransferServicesPathChangeComplete_error_finalPath
  , fileTransferServicesRetrieveFolderListingComplete_error_listing
  , fileTransferServicesFilePreparationComplete_error
  , fileTransferServicesSendFileProgress_transferProgress
  , fileTransferServicesSendFileComplete_error
  , fileTransferServicesCopyRemoteFileProgress_transferProgress
  , fileTransferServicesCopyRemoteFileComplete_error
  , registerIncomingDataListener_refCon
  , write_length
  , withL2CAPChannelRef
  , getL2CAPChannelRef
  , fileTransferServicesConnectionComplete_errorSelector
  , fileTransferServicesDisconnectionComplete_errorSelector
  , fileTransferServicesAbortComplete_errorSelector
  , fileTransferServicesRemoveItemComplete_error_removedItemSelector
  , fileTransferServicesCreateFolderComplete_error_folderSelector
  , fileTransferServicesPathChangeComplete_error_finalPathSelector
  , fileTransferServicesRetrieveFolderListingComplete_error_listingSelector
  , fileTransferServicesFilePreparationComplete_errorSelector
  , fileTransferServicesSendFileProgress_transferProgressSelector
  , fileTransferServicesSendFileComplete_errorSelector
  , fileTransferServicesCopyRemoteFileProgress_transferProgressSelector
  , fileTransferServicesCopyRemoteFileComplete_errorSelector
  , registerIncomingDataListener_refConSelector
  , write_lengthSelector
  , withL2CAPChannelRefSelector
  , getL2CAPChannelRefSelector


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

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | fileTransferServicesConnectionComplete:error:
--
-- The delegate method that corresponds to the connect method
--
-- inError will either be kOBEXSuccess or it will be an error returned by the OBEX Session
--
-- ObjC selector: @- fileTransferServicesConnectionComplete:error:@
fileTransferServicesConnectionComplete_error :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices) => nsObject -> inServices -> CInt -> IO ()
fileTransferServicesConnectionComplete_error nsObject  inServices inError =
withObjCPtr inServices $ \raw_inServices ->
    sendMsg nsObject (mkSelector "fileTransferServicesConnectionComplete:error:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError)]

-- | fileTransferServicesDisconnectionComplete:error:
--
-- The delegate method that corresponds to the disconnect method
--
-- inError will be kOBEXSuccess on success.  This method will also be called if the connection					is lost to the server.  Possible error codes include kOBEXSessionTransportDiedError, 					kOBEXSessionNoTransportError, and kOBEXSessionNotConnectedError.
--
-- ObjC selector: @- fileTransferServicesDisconnectionComplete:error:@
fileTransferServicesDisconnectionComplete_error :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices) => nsObject -> inServices -> CInt -> IO ()
fileTransferServicesDisconnectionComplete_error nsObject  inServices inError =
withObjCPtr inServices $ \raw_inServices ->
    sendMsg nsObject (mkSelector "fileTransferServicesDisconnectionComplete:error:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError)]

-- | fileTransferServicesAbortComplete:error:
--
-- The delegate method that corresponds to the abort method
--
-- Possible inError values are kOBEXSuccess and kOBEXTimeoutError
--
-- ObjC selector: @- fileTransferServicesAbortComplete:error:@
fileTransferServicesAbortComplete_error :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices) => nsObject -> inServices -> CInt -> IO ()
fileTransferServicesAbortComplete_error nsObject  inServices inError =
withObjCPtr inServices $ \raw_inServices ->
    sendMsg nsObject (mkSelector "fileTransferServicesAbortComplete:error:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError)]

-- | fileTransferServicesRemoveItemComplete:error:removedItem:
--
-- The delegate method that corresponds to the removeItemNamed: method.
--
-- @inItemName@ — The name of the remote item that was removed
--
-- ObjC selector: @- fileTransferServicesRemoveItemComplete:error:removedItem:@
fileTransferServicesRemoveItemComplete_error_removedItem :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices, IsNSString inItemName) => nsObject -> inServices -> CInt -> inItemName -> IO ()
fileTransferServicesRemoveItemComplete_error_removedItem nsObject  inServices inError inItemName =
withObjCPtr inServices $ \raw_inServices ->
  withObjCPtr inItemName $ \raw_inItemName ->
      sendMsg nsObject (mkSelector "fileTransferServicesRemoveItemComplete:error:removedItem:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError), argPtr (castPtr raw_inItemName :: Ptr ())]

-- | fileTransferServicesCreateFolderComplete:error:folderName:
--
-- The delegate method that corresponds to the createFolderNamed: method.
--
-- @inFolderName@ — The name of the newly created folder
--
-- ObjC selector: @- fileTransferServicesCreateFolderComplete:error:folder:@
fileTransferServicesCreateFolderComplete_error_folder :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices, IsNSString inFolderName) => nsObject -> inServices -> CInt -> inFolderName -> IO ()
fileTransferServicesCreateFolderComplete_error_folder nsObject  inServices inError inFolderName =
withObjCPtr inServices $ \raw_inServices ->
  withObjCPtr inFolderName $ \raw_inFolderName ->
      sendMsg nsObject (mkSelector "fileTransferServicesCreateFolderComplete:error:folder:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError), argPtr (castPtr raw_inFolderName :: Ptr ())]

-- | fileTransferServicesPathChangeComplete:error:finalPath:
--
-- The delegate method that corresponds to the changeCurrentFolderToRoot:,					changeCurrentFolderBackward:, and changeCurrentFolderForward: methods
--
-- @inPath@ — The current remote path
--
-- ObjC selector: @- fileTransferServicesPathChangeComplete:error:finalPath:@
fileTransferServicesPathChangeComplete_error_finalPath :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices, IsNSString inPath) => nsObject -> inServices -> CInt -> inPath -> IO ()
fileTransferServicesPathChangeComplete_error_finalPath nsObject  inServices inError inPath =
withObjCPtr inServices $ \raw_inServices ->
  withObjCPtr inPath $ \raw_inPath ->
      sendMsg nsObject (mkSelector "fileTransferServicesPathChangeComplete:error:finalPath:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError), argPtr (castPtr raw_inPath :: Ptr ())]

-- | fileTransferServicesRetrieveFolderListingComplete:error:listing:
--
-- The delegate method that corresponds to the retrieveFolderListing method
--
-- @inListing@ — An array of NSDictionary's that detail each file at the current path.  The keys					to this dictionary are defined in the OBEXFileTransferServicesDelegate category.
--
-- ObjC selector: @- fileTransferServicesRetrieveFolderListingComplete:error:listing:@
fileTransferServicesRetrieveFolderListingComplete_error_listing :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices, IsNSArray inListing) => nsObject -> inServices -> CInt -> inListing -> IO ()
fileTransferServicesRetrieveFolderListingComplete_error_listing nsObject  inServices inError inListing =
withObjCPtr inServices $ \raw_inServices ->
  withObjCPtr inListing $ \raw_inListing ->
      sendMsg nsObject (mkSelector "fileTransferServicesRetrieveFolderListingComplete:error:listing:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError), argPtr (castPtr raw_inListing :: Ptr ())]

-- | fileTransferServicesFilePreparationComplete:error:
--
-- The delegate method for receiving information on the preparation of each file to send
--
-- This method will be called before the transfer operation.
--
-- ObjC selector: @- fileTransferServicesFilePreparationComplete:error:@
fileTransferServicesFilePreparationComplete_error :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices) => nsObject -> inServices -> CInt -> IO ()
fileTransferServicesFilePreparationComplete_error nsObject  inServices inError =
withObjCPtr inServices $ \raw_inServices ->
    sendMsg nsObject (mkSelector "fileTransferServicesFilePreparationComplete:error:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError)]

-- | fileTransferServicesSendFileProgress:transferProgress:
--
-- The delegate method for receiving information on the sendFile: transfer
--
-- This method will be called during the transfer operation.
--
-- @inProgressDescription@ — A dictionary containing information on the state of the transfer. The keys					 to this dictionary are defined in the OBEXFileTransferServicesDelegate category.
--
-- ObjC selector: @- fileTransferServicesSendFileProgress:transferProgress:@
fileTransferServicesSendFileProgress_transferProgress :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices, IsNSDictionary inProgressDescription) => nsObject -> inServices -> inProgressDescription -> IO ()
fileTransferServicesSendFileProgress_transferProgress nsObject  inServices inProgressDescription =
withObjCPtr inServices $ \raw_inServices ->
  withObjCPtr inProgressDescription $ \raw_inProgressDescription ->
      sendMsg nsObject (mkSelector "fileTransferServicesSendFileProgress:transferProgress:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argPtr (castPtr raw_inProgressDescription :: Ptr ())]

-- | fileTransferServicesSendFileComplete:error:
--
-- The delegate method that corresponds to the sendFile: method.
--
-- This method will be called when the transfer operation has finished.
--
-- ObjC selector: @- fileTransferServicesSendFileComplete:error:@
fileTransferServicesSendFileComplete_error :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices) => nsObject -> inServices -> CInt -> IO ()
fileTransferServicesSendFileComplete_error nsObject  inServices inError =
withObjCPtr inServices $ \raw_inServices ->
    sendMsg nsObject (mkSelector "fileTransferServicesSendFileComplete:error:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError)]

-- | fileTransferServicesCopyRemoteFileProgress:transferProgress:
--
-- The delegate method for receiving information on the GET transfer
--
-- This method will be called during the transfer operation
--
-- @inProgressDescription@ — A dictionary containing information on the state of the transfer. The keys					to this dictionary are defined in the OBEXFileTransferServicesDelegate category.
--
-- ObjC selector: @- fileTransferServicesCopyRemoteFileProgress:transferProgress:@
fileTransferServicesCopyRemoteFileProgress_transferProgress :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices, IsNSDictionary inProgressDescription) => nsObject -> inServices -> inProgressDescription -> IO ()
fileTransferServicesCopyRemoteFileProgress_transferProgress nsObject  inServices inProgressDescription =
withObjCPtr inServices $ \raw_inServices ->
  withObjCPtr inProgressDescription $ \raw_inProgressDescription ->
      sendMsg nsObject (mkSelector "fileTransferServicesCopyRemoteFileProgress:transferProgress:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argPtr (castPtr raw_inProgressDescription :: Ptr ())]

-- | fileTransferServicesCopyRemoteFileComplete:error:
--
-- The delegate method that corresponds to the getFileNamed:toLocalPathAndName: method
--
-- This method will be called when the transfer operation has finished
--
-- ObjC selector: @- fileTransferServicesCopyRemoteFileComplete:error:@
fileTransferServicesCopyRemoteFileComplete_error :: (IsNSObject nsObject, IsOBEXFileTransferServices inServices) => nsObject -> inServices -> CInt -> IO ()
fileTransferServicesCopyRemoteFileComplete_error nsObject  inServices inError =
withObjCPtr inServices $ \raw_inServices ->
    sendMsg nsObject (mkSelector "fileTransferServicesCopyRemoteFileComplete:error:") retVoid [argPtr (castPtr raw_inServices :: Ptr ()), argCInt (fromIntegral inError)]

-- | @- registerIncomingDataListener:refCon:@
registerIncomingDataListener_refCon :: IsNSObject nsObject => nsObject -> Ptr () -> Ptr () -> IO CInt
registerIncomingDataListener_refCon nsObject  listener refCon =
  sendMsg nsObject (mkSelector "registerIncomingDataListener:refCon:") retCInt [argPtr listener, argPtr refCon]

-- | @- write:length:@
write_length :: IsNSObject nsObject => nsObject -> Ptr () -> CUShort -> IO CInt
write_length nsObject  data_ length_ =
  sendMsg nsObject (mkSelector "write:length:") retCInt [argPtr data_, argCUInt (fromIntegral length_)]

-- | @+ withL2CAPChannelRef:@
withL2CAPChannelRef :: Ptr () -> IO (Id IOBluetoothL2CAPChannel)
withL2CAPChannelRef l2capChannelRef =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "withL2CAPChannelRef:") (retPtr retVoid) [argPtr l2capChannelRef] >>= retainedObject . castPtr

-- | @- getL2CAPChannelRef@
getL2CAPChannelRef :: IsNSObject nsObject => nsObject -> IO (Ptr ())
getL2CAPChannelRef nsObject  =
  fmap castPtr $ sendMsg nsObject (mkSelector "getL2CAPChannelRef") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileTransferServicesConnectionComplete:error:@
fileTransferServicesConnectionComplete_errorSelector :: Selector
fileTransferServicesConnectionComplete_errorSelector = mkSelector "fileTransferServicesConnectionComplete:error:"

-- | @Selector@ for @fileTransferServicesDisconnectionComplete:error:@
fileTransferServicesDisconnectionComplete_errorSelector :: Selector
fileTransferServicesDisconnectionComplete_errorSelector = mkSelector "fileTransferServicesDisconnectionComplete:error:"

-- | @Selector@ for @fileTransferServicesAbortComplete:error:@
fileTransferServicesAbortComplete_errorSelector :: Selector
fileTransferServicesAbortComplete_errorSelector = mkSelector "fileTransferServicesAbortComplete:error:"

-- | @Selector@ for @fileTransferServicesRemoveItemComplete:error:removedItem:@
fileTransferServicesRemoveItemComplete_error_removedItemSelector :: Selector
fileTransferServicesRemoveItemComplete_error_removedItemSelector = mkSelector "fileTransferServicesRemoveItemComplete:error:removedItem:"

-- | @Selector@ for @fileTransferServicesCreateFolderComplete:error:folder:@
fileTransferServicesCreateFolderComplete_error_folderSelector :: Selector
fileTransferServicesCreateFolderComplete_error_folderSelector = mkSelector "fileTransferServicesCreateFolderComplete:error:folder:"

-- | @Selector@ for @fileTransferServicesPathChangeComplete:error:finalPath:@
fileTransferServicesPathChangeComplete_error_finalPathSelector :: Selector
fileTransferServicesPathChangeComplete_error_finalPathSelector = mkSelector "fileTransferServicesPathChangeComplete:error:finalPath:"

-- | @Selector@ for @fileTransferServicesRetrieveFolderListingComplete:error:listing:@
fileTransferServicesRetrieveFolderListingComplete_error_listingSelector :: Selector
fileTransferServicesRetrieveFolderListingComplete_error_listingSelector = mkSelector "fileTransferServicesRetrieveFolderListingComplete:error:listing:"

-- | @Selector@ for @fileTransferServicesFilePreparationComplete:error:@
fileTransferServicesFilePreparationComplete_errorSelector :: Selector
fileTransferServicesFilePreparationComplete_errorSelector = mkSelector "fileTransferServicesFilePreparationComplete:error:"

-- | @Selector@ for @fileTransferServicesSendFileProgress:transferProgress:@
fileTransferServicesSendFileProgress_transferProgressSelector :: Selector
fileTransferServicesSendFileProgress_transferProgressSelector = mkSelector "fileTransferServicesSendFileProgress:transferProgress:"

-- | @Selector@ for @fileTransferServicesSendFileComplete:error:@
fileTransferServicesSendFileComplete_errorSelector :: Selector
fileTransferServicesSendFileComplete_errorSelector = mkSelector "fileTransferServicesSendFileComplete:error:"

-- | @Selector@ for @fileTransferServicesCopyRemoteFileProgress:transferProgress:@
fileTransferServicesCopyRemoteFileProgress_transferProgressSelector :: Selector
fileTransferServicesCopyRemoteFileProgress_transferProgressSelector = mkSelector "fileTransferServicesCopyRemoteFileProgress:transferProgress:"

-- | @Selector@ for @fileTransferServicesCopyRemoteFileComplete:error:@
fileTransferServicesCopyRemoteFileComplete_errorSelector :: Selector
fileTransferServicesCopyRemoteFileComplete_errorSelector = mkSelector "fileTransferServicesCopyRemoteFileComplete:error:"

-- | @Selector@ for @registerIncomingDataListener:refCon:@
registerIncomingDataListener_refConSelector :: Selector
registerIncomingDataListener_refConSelector = mkSelector "registerIncomingDataListener:refCon:"

-- | @Selector@ for @write:length:@
write_lengthSelector :: Selector
write_lengthSelector = mkSelector "write:length:"

-- | @Selector@ for @withL2CAPChannelRef:@
withL2CAPChannelRefSelector :: Selector
withL2CAPChannelRefSelector = mkSelector "withL2CAPChannelRef:"

-- | @Selector@ for @getL2CAPChannelRef@
getL2CAPChannelRefSelector :: Selector
getL2CAPChannelRefSelector = mkSelector "getL2CAPChannelRef"

