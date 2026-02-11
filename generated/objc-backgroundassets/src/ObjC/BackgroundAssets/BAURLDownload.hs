{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BAURLDownload@.
module ObjC.BackgroundAssets.BAURLDownload
  ( BAURLDownload
  , IsBAURLDownload(..)
  , new
  , init_
  , initWithIdentifier_request_fileSize_applicationGroupIdentifier
  , initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_priority
  , initWithIdentifier_request_applicationGroupIdentifier
  , initWithIdentifier_request_applicationGroupIdentifier_priority
  , newSelector
  , initSelector
  , initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector
  , initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector
  , initWithIdentifier_request_applicationGroupIdentifierSelector
  , initWithIdentifier_request_applicationGroupIdentifier_prioritySelector


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

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id BAURLDownload)
new  =
  do
    cls' <- getRequiredClass "BAURLDownload"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsBAURLDownload baurlDownload => baurlDownload -> IO (Id BAURLDownload)
init_ baurlDownload  =
  sendMsg baurlDownload (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Constructs a download object to represent the download of a asset located inside of the provided @request.@
--
-- @identifier@ — A unique identifier that is used to track the download across the app and extension.
--
-- @request@ — The request used to perform the download. The URL provided inside of the request must be a https scheme.
--
-- @fileSize@ — The size of the file to download. This field must be accurate in order to show the user accurate progress during app installation. If the size does not match the file being downloaded, then the download will fail.
--
-- @applicationGroupIdentifier@ — The identifier of the application group that should used to store the finished download.
--
-- ObjC selector: @- initWithIdentifier:request:fileSize:applicationGroupIdentifier:@
initWithIdentifier_request_fileSize_applicationGroupIdentifier :: (IsBAURLDownload baurlDownload, IsNSString identifier, IsNSURLRequest request, IsNSString applicationGroupIdentifier) => baurlDownload -> identifier -> request -> CULong -> applicationGroupIdentifier -> IO (Id BAURLDownload)
initWithIdentifier_request_fileSize_applicationGroupIdentifier baurlDownload  identifier request fileSize applicationGroupIdentifier =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr request $ \raw_request ->
    withObjCPtr applicationGroupIdentifier $ \raw_applicationGroupIdentifier ->
        sendMsg baurlDownload (mkSelector "initWithIdentifier:request:fileSize:applicationGroupIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_request :: Ptr ()), argCULong (fromIntegral fileSize), argPtr (castPtr raw_applicationGroupIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | Constructs a download object to represent the download of a asset located inside of the provided @request.@
--
-- @identifier@ — A unique identifier that is used to track the download across the app and extension.
--
-- @request@ — The request used to perform the download. The URL provided inside of the request must be a https scheme.
--
-- @essential@ — Whether the download is essential. See @BADownload.isEssential.@ Default is false.
--
-- @fileSize@ — The size of the file to download. This field must be accurate in order to show the user accurate progress during app installation. If the size does not match the file being downloaded, then the download will fail.
--
-- @applicationGroupIdentifier@ — The identifier of the application group that should used to store the finished download.
--
-- @priority@ — A priority between @BADownloaderPriorityMin@ - @BADownloaderPriorityMax@ which is used to order the downloads for this process. It is recommended to use  @BADownloaderPriorityDefault@ if download priority does not matter.
--
-- ObjC selector: @- initWithIdentifier:request:essential:fileSize:applicationGroupIdentifier:priority:@
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_priority :: (IsBAURLDownload baurlDownload, IsNSString identifier, IsNSURLRequest request, IsNSString applicationGroupIdentifier) => baurlDownload -> identifier -> request -> Bool -> CULong -> applicationGroupIdentifier -> CLong -> IO (Id BAURLDownload)
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_priority baurlDownload  identifier request essential fileSize applicationGroupIdentifier priority =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr request $ \raw_request ->
    withObjCPtr applicationGroupIdentifier $ \raw_applicationGroupIdentifier ->
        sendMsg baurlDownload (mkSelector "initWithIdentifier:request:essential:fileSize:applicationGroupIdentifier:priority:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_request :: Ptr ()), argCULong (if essential then 1 else 0), argCULong (fromIntegral fileSize), argPtr (castPtr raw_applicationGroupIdentifier :: Ptr ()), argCLong (fromIntegral priority)] >>= ownedObject . castPtr

-- | Constructs a download object to represent the download of a asset located inside of the provided @request.@
--
-- @identifier@ — A unique identifier that is used to track the download across the app and extension.
--
-- @request@ — The request used to perform the download. The URL provided inside of the request must be a https scheme.
--
-- @applicationGroupIdentifier@ — The identifier of the application group that should used to store the finished download.
--
-- ObjC selector: @- initWithIdentifier:request:applicationGroupIdentifier:@
initWithIdentifier_request_applicationGroupIdentifier :: (IsBAURLDownload baurlDownload, IsNSString identifier, IsNSURLRequest request, IsNSString applicationGroupIdentifier) => baurlDownload -> identifier -> request -> applicationGroupIdentifier -> IO (Id BAURLDownload)
initWithIdentifier_request_applicationGroupIdentifier baurlDownload  identifier request applicationGroupIdentifier =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr request $ \raw_request ->
    withObjCPtr applicationGroupIdentifier $ \raw_applicationGroupIdentifier ->
        sendMsg baurlDownload (mkSelector "initWithIdentifier:request:applicationGroupIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_applicationGroupIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | Constructs a download object to represent the download of a asset located inside of the provided @request.@
--
-- @identifier@ — A unique identifier that is used to track the download across the app and extension.
--
-- @request@ — The request used to perform the download. The URL provided inside of the request must be a https scheme.
--
-- @applicationGroupIdentifier@ — The identifier of the application group that should used to store the finished download.
--
-- @priority@ — A priority between @BADownloaderPriorityMin@ - @BADownloaderPriorityMax@ which is used to order the downloads for this process. It is recommended to use  @BADownloaderPriorityDefault@ if download priority does not matter.
--
-- ObjC selector: @- initWithIdentifier:request:applicationGroupIdentifier:priority:@
initWithIdentifier_request_applicationGroupIdentifier_priority :: (IsBAURLDownload baurlDownload, IsNSString identifier, IsNSURLRequest request, IsNSString applicationGroupIdentifier) => baurlDownload -> identifier -> request -> applicationGroupIdentifier -> CLong -> IO (Id BAURLDownload)
initWithIdentifier_request_applicationGroupIdentifier_priority baurlDownload  identifier request applicationGroupIdentifier priority =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr request $ \raw_request ->
    withObjCPtr applicationGroupIdentifier $ \raw_applicationGroupIdentifier ->
        sendMsg baurlDownload (mkSelector "initWithIdentifier:request:applicationGroupIdentifier:priority:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_applicationGroupIdentifier :: Ptr ()), argCLong (fromIntegral priority)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:request:fileSize:applicationGroupIdentifier:@
initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector :: Selector
initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector = mkSelector "initWithIdentifier:request:fileSize:applicationGroupIdentifier:"

-- | @Selector@ for @initWithIdentifier:request:essential:fileSize:applicationGroupIdentifier:priority:@
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector :: Selector
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector = mkSelector "initWithIdentifier:request:essential:fileSize:applicationGroupIdentifier:priority:"

-- | @Selector@ for @initWithIdentifier:request:applicationGroupIdentifier:@
initWithIdentifier_request_applicationGroupIdentifierSelector :: Selector
initWithIdentifier_request_applicationGroupIdentifierSelector = mkSelector "initWithIdentifier:request:applicationGroupIdentifier:"

-- | @Selector@ for @initWithIdentifier:request:applicationGroupIdentifier:priority:@
initWithIdentifier_request_applicationGroupIdentifier_prioritySelector :: Selector
initWithIdentifier_request_applicationGroupIdentifier_prioritySelector = mkSelector "initWithIdentifier:request:applicationGroupIdentifier:priority:"

