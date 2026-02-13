{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OBEXFileTransferServices@.
module ObjC.IOBluetooth.OBEXFileTransferServices
  ( OBEXFileTransferServices
  , IsOBEXFileTransferServices(..)
  , withOBEXSession
  , initWithOBEXSession
  , currentPath
  , isBusy
  , isConnected
  , connectToFTPService
  , connectToObjectPushService
  , disconnect
  , changeCurrentFolderToRoot
  , changeCurrentFolderBackward
  , changeCurrentFolderForwardToPath
  , createFolder
  , removeItem
  , retrieveFolderListing
  , sendFile
  , copyRemoteFile_toLocalPath
  , sendData_type_name
  , getDefaultVCard
  , abort
  , delegate
  , setDelegate
  , abortSelector
  , changeCurrentFolderBackwardSelector
  , changeCurrentFolderForwardToPathSelector
  , changeCurrentFolderToRootSelector
  , connectToFTPServiceSelector
  , connectToObjectPushServiceSelector
  , copyRemoteFile_toLocalPathSelector
  , createFolderSelector
  , currentPathSelector
  , delegateSelector
  , disconnectSelector
  , getDefaultVCardSelector
  , initWithOBEXSessionSelector
  , isBusySelector
  , isConnectedSelector
  , removeItemSelector
  , retrieveFolderListingSelector
  , sendData_type_nameSelector
  , sendFileSelector
  , setDelegateSelector
  , withOBEXSessionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | withOBEXSession:
--
-- Create a new OBEXFileTransferServices object
--
-- This object must be constructed with a valid IOBluetoothOBEXSession. The given 					IOBluetoothOBEXSession does not need to be connected to the remote server.  					This module can be manually connected through the connect(void) method.
--
-- @inOBEXSession@ — A valid IOBluetoothOBEXSession
--
-- Returns: A newly created OBEXFileTransferServices object on success, nil on failure
--
-- ObjC selector: @+ withOBEXSession:@
withOBEXSession :: IsIOBluetoothOBEXSession inOBEXSession => inOBEXSession -> IO (Id OBEXFileTransferServices)
withOBEXSession inOBEXSession =
  do
    cls' <- getRequiredClass "OBEXFileTransferServices"
    sendClassMessage cls' withOBEXSessionSelector (toIOBluetoothOBEXSession inOBEXSession)

-- | initWithOBEXSession:
--
-- Create a new OBEXFileTransferServices object
--
-- This object must be constructed with a valid IOBluetoothOBEXSession. The given 					IOBluetoothOBEXSession does not need to be connected to the remote server.  					OBEXFileTransferServices can be manually connected through the provided connection 					methods.
--
-- @inOBEXSession@ — A valid IOBluetoothOBEXSession
--
-- Returns: A newly created OBEXFileTransferServices object on success, nil on failure
--
-- ObjC selector: @- initWithOBEXSession:@
initWithOBEXSession :: (IsOBEXFileTransferServices obexFileTransferServices, IsIOBluetoothOBEXSession inOBEXSession) => obexFileTransferServices -> inOBEXSession -> IO (Id OBEXFileTransferServices)
initWithOBEXSession obexFileTransferServices inOBEXSession =
  sendOwnedMessage obexFileTransferServices initWithOBEXSessionSelector (toIOBluetoothOBEXSession inOBEXSession)

-- | currentPath
--
-- Get the remote current directory path during an FTP session
--
-- This path is changed with each path-specific command called on OBEXFileTransferServices.
--
-- Returns: The current path being browsed over FTP
--
-- ObjC selector: @- currentPath@
currentPath :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO (Id NSString)
currentPath obexFileTransferServices =
  sendMessage obexFileTransferServices currentPathSelector

-- | isBusy
--
-- Get the action state of the module
--
-- OBEXFileTransferServices will be considered "busy" when an operation in taking place or				has not completed.  Calling abort: on this module will not automatically reset its busy				state.  The user will have to wait for the operation to complete or for the current				operation to timeout.
--
-- Returns: Success or failure code.
--
-- ObjC selector: @- isBusy@
isBusy :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO Bool
isBusy obexFileTransferServices =
  sendMessage obexFileTransferServices isBusySelector

-- | isConnected
--
-- Get the connected state of this module.
--
-- Asks the OBEXSession that was passed to it on creation if it has an open OBEX connection
--
-- Returns: Success or failure code.
--
-- ObjC selector: @- isConnected@
isConnected :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO Bool
isConnected obexFileTransferServices =
  sendMessage obexFileTransferServices isConnectedSelector

-- | connectToFTPService
--
-- Connect to a remote device for FTP operations
--
-- If the OBEXSession given to OBEXFileTransferServices on creation is not connected it				can be manually connected through this method.
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXSessionAlreadyConnectedError, kOBEXNoResourcesError				initially.  Further results returned through the fileTransferServicesConnectionComplete:				delegate method if initially successful.
--
-- ObjC selector: @- connectToFTPService@
connectToFTPService :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
connectToFTPService obexFileTransferServices =
  sendMessage obexFileTransferServices connectToFTPServiceSelector

-- | connectToObjectPushService
--
-- Connect to a remote device for ObjectPush operations.				Most of the FTP functionality of this object will be disabled.
--
-- If the OBEXSession given to OBEXFileTransferServices on creation is not connected it				can be manually connected through this method.
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXSessionAlreadyConnectedError, kOBEXNoResourcesError				initially.  Further results returned through the fileTransferServicesConnectionComplete: 				delegate method if initially successful.
--
-- ObjC selector: @- connectToObjectPushService@
connectToObjectPushService :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
connectToObjectPushService obexFileTransferServices =
  sendMessage obexFileTransferServices connectToObjectPushServiceSelector

-- | disconnect
--
-- Disconnect from the remote device
--
-- The user can manually disconnect the OBEXSession from the remote device if they want				to.  OBEXFileTransferServices will disconnect the OBEXSession at release only if it was responsible 				for opening the connection via a connect method.
--
-- Returns: kOBEXSuccess, kOBEXSessionNotConnectedError, or kOBEXSessionBusyError initially. Further results 				returned through the fileTransferServicesDisconnectionComplete: delegate method if initially successful.
--
-- ObjC selector: @- disconnect@
disconnect :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
disconnect obexFileTransferServices =
  sendMessage obexFileTransferServices disconnectSelector

-- | changeCurrentFolderRoot
--
-- Asynchronously change to the remote root directory
--
-- Equivalent to 'cd ~/'
--
-- Returns: kOBEXSuccess or kOBEXSessionBusyError initially.  Further results				returned through the fileTransferServicesPathChangeComplete: delegate method if initially successful.
--
-- ObjC selector: @- changeCurrentFolderToRoot@
changeCurrentFolderToRoot :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
changeCurrentFolderToRoot obexFileTransferServices =
  sendMessage obexFileTransferServices changeCurrentFolderToRootSelector

-- | changeCurrentFolderBackward
--
-- Change to the directory above the current level if not at the root
--
-- Equivalent to 'cd ..' only if remote path is not already at root.
--
-- Returns: kOBEXSuccess or kOBEXSessionBusyError initially. Further results				returned through the fileTransferServicesPathChangeComplete: delegate method if initially successful.
--
-- ObjC selector: @- changeCurrentFolderBackward@
changeCurrentFolderBackward :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
changeCurrentFolderBackward obexFileTransferServices =
  sendMessage obexFileTransferServices changeCurrentFolderBackwardSelector

-- | changeCurrentFolderForwardToPath:
--
-- Change the remote path
--
-- Equivalent to 'cd dirName'.
--
-- @inDirName@ — The name of the remote folder to be set as current
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError initially. Further results				returned through the fileTransferServicesPathChangeComplete: delegate method if initially successful.
--
-- ObjC selector: @- changeCurrentFolderForwardToPath:@
changeCurrentFolderForwardToPath :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSString inDirName) => obexFileTransferServices -> inDirName -> IO CInt
changeCurrentFolderForwardToPath obexFileTransferServices inDirName =
  sendMessage obexFileTransferServices changeCurrentFolderForwardToPathSelector (toNSString inDirName)

-- | createFolder:
--
-- Create a folder on the remote target
--
-- Equivalent to 'mkdir dirName'.
--
-- @inDirName@ — The name of the folder to be created
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError initially. 				Further results returned through the fileTransferServicesCreateFolderComplete delegate method				if initially successful.
--
-- ObjC selector: @- createFolder:@
createFolder :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSString inDirName) => obexFileTransferServices -> inDirName -> IO CInt
createFolder obexFileTransferServices inDirName =
  sendMessage obexFileTransferServices createFolderSelector (toNSString inDirName)

-- | removeItem:
--
-- Remove a remote item.
--
-- Not supported for use on Apple computer targets
--
-- @inItemName@ — The name of the remote item to be removed
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError initially.  				Further results returned through the fileTransferServicesRemoveItemComplete: delegate method 				if initially successful.
--
-- ObjC selector: @- removeItem:@
removeItem :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSString inItemName) => obexFileTransferServices -> inItemName -> IO CInt
removeItem obexFileTransferServices inItemName =
  sendMessage obexFileTransferServices removeItemSelector (toNSString inItemName)

-- | retrieveFolderListing
--
-- Get a remote directory listing
--
-- Equivalent to 'ls'.
--
-- Returns: kOBEXSuccess or kOBEXSessionBusyError initially.  Further results returned through 				the fileTransferServicesRetrieveFolderListingComplete: delegate method if initially successful.
--
-- ObjC selector: @- retrieveFolderListing@
retrieveFolderListing :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
retrieveFolderListing obexFileTransferServices =
  sendMessage obexFileTransferServices retrieveFolderListingSelector

-- | sendFile:
--
-- Put a local file to the remote target
--
-- Equivalent to 'mv inLocalFilePath remoteCurrentPath'.
--
-- @inLocalPathAndName@ — The name and path of the file to be sent an instance of OBEXFilePut.
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError initially. Further 				results returned through the fileTransferServicesSendComplete: and 				fileTransferServicesSendProgress: delegate methods if initially successful.
--
-- ObjC selector: @- sendFile:@
sendFile :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSString inLocalPathAndName) => obexFileTransferServices -> inLocalPathAndName -> IO CInt
sendFile obexFileTransferServices inLocalPathAndName =
  sendMessage obexFileTransferServices sendFileSelector (toNSString inLocalPathAndName)

-- | copyRemoteFile:toLocalPath:
--
-- Copy a remote file to a local path
--
-- Equivalent to 'cp remotePath/remoteFileName localPathAndName'.
--
-- @inRemoteFileName@ — The name of the remote file to get
--
-- @inLocalPathAndName@ — The path and name of where the received file will go
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError. initially.  Further 				results returned through the fileTransferServicesGetComplete: and 				fileTransferServicesGetProgress: delegate methods if initially successful.
--
-- ObjC selector: @- copyRemoteFile:toLocalPath:@
copyRemoteFile_toLocalPath :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSString inRemoteFileName, IsNSString inLocalPathAndName) => obexFileTransferServices -> inRemoteFileName -> inLocalPathAndName -> IO CInt
copyRemoteFile_toLocalPath obexFileTransferServices inRemoteFileName inLocalPathAndName =
  sendOwnedMessage obexFileTransferServices copyRemoteFile_toLocalPathSelector (toNSString inRemoteFileName) (toNSString inLocalPathAndName)

-- | sendData:type:name:
--
-- Send data to a remote target
--
-- Use this method when you have data to send but no file to read from.
--
-- @inData@ — The data to be sent
--
-- @inType@ — The type of the data to be sent that will be used in the OBEX type header,				usually a mime-type.  For example, use "text/x-vCard" when sending vCards. This 				argument is optional.
--
-- @inName@ — The name of the file that the data can be referenced as.
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError initially. Further 				results returned through the fileTransferServicesSendComplete: and 				fileTransferServicesSendProgress: delegate methods if initially successful.
--
-- ObjC selector: @- sendData:type:name:@
sendData_type_name :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSData inData, IsNSString inType, IsNSString inName) => obexFileTransferServices -> inData -> inType -> inName -> IO CInt
sendData_type_name obexFileTransferServices inData inType inName =
  sendMessage obexFileTransferServices sendData_type_nameSelector (toNSData inData) (toNSString inType) (toNSString inName)

-- | getDefaultVCard:
--
-- Get the remote default VCard, if it is supported
--
-- Some devices such as cellphones and computers support default VCards
--
-- @inLocalPathAndName@ — The path and name of where the received file will go
--
-- Returns: kOBEXSuccess, kOBEXSessionBusyError, or kOBEXBadArgumentError initially.  Further 				results returned through the fileTransferServicesGetComplete: and 				fileTransferServicesGetProgress: delegate methods if initially successful.
--
-- ObjC selector: @- getDefaultVCard:@
getDefaultVCard :: (IsOBEXFileTransferServices obexFileTransferServices, IsNSString inLocalPathAndName) => obexFileTransferServices -> inLocalPathAndName -> IO CInt
getDefaultVCard obexFileTransferServices inLocalPathAndName =
  sendMessage obexFileTransferServices getDefaultVCardSelector (toNSString inLocalPathAndName)

-- | abort
--
-- Abort the current operation
--
-- Attempts send an abort request to the remote device.  Returns the OBEXFileTransferServices				object to an idle state though the state of the remote device is not guaranteed.
--
-- Returns: kOBEXSuccess, or kOBEXGeneralError if no command is in progress. ABORT 				commands can only be sent on our turn, meaning we may have to timeout if the				target side never responds to the command in progress.  In that case this object				will call back with a status of kOBEXTimeoutError and an error. Further results				returned through the fileTransferServicesAbortComplete: delegate method if initially successful.
--
-- ObjC selector: @- abort@
abort :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO CInt
abort obexFileTransferServices =
  sendMessage obexFileTransferServices abortSelector

-- | @- delegate@
delegate :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> IO RawId
delegate obexFileTransferServices =
  sendMessage obexFileTransferServices delegateSelector

-- | @- setDelegate:@
setDelegate :: IsOBEXFileTransferServices obexFileTransferServices => obexFileTransferServices -> RawId -> IO ()
setDelegate obexFileTransferServices value =
  sendMessage obexFileTransferServices setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @withOBEXSession:@
withOBEXSessionSelector :: Selector '[Id IOBluetoothOBEXSession] (Id OBEXFileTransferServices)
withOBEXSessionSelector = mkSelector "withOBEXSession:"

-- | @Selector@ for @initWithOBEXSession:@
initWithOBEXSessionSelector :: Selector '[Id IOBluetoothOBEXSession] (Id OBEXFileTransferServices)
initWithOBEXSessionSelector = mkSelector "initWithOBEXSession:"

-- | @Selector@ for @currentPath@
currentPathSelector :: Selector '[] (Id NSString)
currentPathSelector = mkSelector "currentPath"

-- | @Selector@ for @isBusy@
isBusySelector :: Selector '[] Bool
isBusySelector = mkSelector "isBusy"

-- | @Selector@ for @isConnected@
isConnectedSelector :: Selector '[] Bool
isConnectedSelector = mkSelector "isConnected"

-- | @Selector@ for @connectToFTPService@
connectToFTPServiceSelector :: Selector '[] CInt
connectToFTPServiceSelector = mkSelector "connectToFTPService"

-- | @Selector@ for @connectToObjectPushService@
connectToObjectPushServiceSelector :: Selector '[] CInt
connectToObjectPushServiceSelector = mkSelector "connectToObjectPushService"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector '[] CInt
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @changeCurrentFolderToRoot@
changeCurrentFolderToRootSelector :: Selector '[] CInt
changeCurrentFolderToRootSelector = mkSelector "changeCurrentFolderToRoot"

-- | @Selector@ for @changeCurrentFolderBackward@
changeCurrentFolderBackwardSelector :: Selector '[] CInt
changeCurrentFolderBackwardSelector = mkSelector "changeCurrentFolderBackward"

-- | @Selector@ for @changeCurrentFolderForwardToPath:@
changeCurrentFolderForwardToPathSelector :: Selector '[Id NSString] CInt
changeCurrentFolderForwardToPathSelector = mkSelector "changeCurrentFolderForwardToPath:"

-- | @Selector@ for @createFolder:@
createFolderSelector :: Selector '[Id NSString] CInt
createFolderSelector = mkSelector "createFolder:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector '[Id NSString] CInt
removeItemSelector = mkSelector "removeItem:"

-- | @Selector@ for @retrieveFolderListing@
retrieveFolderListingSelector :: Selector '[] CInt
retrieveFolderListingSelector = mkSelector "retrieveFolderListing"

-- | @Selector@ for @sendFile:@
sendFileSelector :: Selector '[Id NSString] CInt
sendFileSelector = mkSelector "sendFile:"

-- | @Selector@ for @copyRemoteFile:toLocalPath:@
copyRemoteFile_toLocalPathSelector :: Selector '[Id NSString, Id NSString] CInt
copyRemoteFile_toLocalPathSelector = mkSelector "copyRemoteFile:toLocalPath:"

-- | @Selector@ for @sendData:type:name:@
sendData_type_nameSelector :: Selector '[Id NSData, Id NSString, Id NSString] CInt
sendData_type_nameSelector = mkSelector "sendData:type:name:"

-- | @Selector@ for @getDefaultVCard:@
getDefaultVCardSelector :: Selector '[Id NSString] CInt
getDefaultVCardSelector = mkSelector "getDefaultVCard:"

-- | @Selector@ for @abort@
abortSelector :: Selector '[] CInt
abortSelector = mkSelector "abort"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

