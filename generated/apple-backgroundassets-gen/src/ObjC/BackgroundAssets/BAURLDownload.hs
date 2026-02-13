{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithIdentifier_request_applicationGroupIdentifierSelector
  , initWithIdentifier_request_applicationGroupIdentifier_prioritySelector
  , initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector
  , initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id BAURLDownload)
new  =
  do
    cls' <- getRequiredClass "BAURLDownload"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsBAURLDownload baurlDownload => baurlDownload -> IO (Id BAURLDownload)
init_ baurlDownload =
  sendOwnedMessage baurlDownload initSelector

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
initWithIdentifier_request_fileSize_applicationGroupIdentifier baurlDownload identifier request fileSize applicationGroupIdentifier =
  sendOwnedMessage baurlDownload initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector (toNSString identifier) (toNSURLRequest request) fileSize (toNSString applicationGroupIdentifier)

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
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_priority baurlDownload identifier request essential fileSize applicationGroupIdentifier priority =
  sendOwnedMessage baurlDownload initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector (toNSString identifier) (toNSURLRequest request) essential fileSize (toNSString applicationGroupIdentifier) priority

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
initWithIdentifier_request_applicationGroupIdentifier baurlDownload identifier request applicationGroupIdentifier =
  sendOwnedMessage baurlDownload initWithIdentifier_request_applicationGroupIdentifierSelector (toNSString identifier) (toNSURLRequest request) (toNSString applicationGroupIdentifier)

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
initWithIdentifier_request_applicationGroupIdentifier_priority baurlDownload identifier request applicationGroupIdentifier priority =
  sendOwnedMessage baurlDownload initWithIdentifier_request_applicationGroupIdentifier_prioritySelector (toNSString identifier) (toNSURLRequest request) (toNSString applicationGroupIdentifier) priority

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BAURLDownload)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BAURLDownload)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:request:fileSize:applicationGroupIdentifier:@
initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector :: Selector '[Id NSString, Id NSURLRequest, CULong, Id NSString] (Id BAURLDownload)
initWithIdentifier_request_fileSize_applicationGroupIdentifierSelector = mkSelector "initWithIdentifier:request:fileSize:applicationGroupIdentifier:"

-- | @Selector@ for @initWithIdentifier:request:essential:fileSize:applicationGroupIdentifier:priority:@
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector :: Selector '[Id NSString, Id NSURLRequest, Bool, CULong, Id NSString, CLong] (Id BAURLDownload)
initWithIdentifier_request_essential_fileSize_applicationGroupIdentifier_prioritySelector = mkSelector "initWithIdentifier:request:essential:fileSize:applicationGroupIdentifier:priority:"

-- | @Selector@ for @initWithIdentifier:request:applicationGroupIdentifier:@
initWithIdentifier_request_applicationGroupIdentifierSelector :: Selector '[Id NSString, Id NSURLRequest, Id NSString] (Id BAURLDownload)
initWithIdentifier_request_applicationGroupIdentifierSelector = mkSelector "initWithIdentifier:request:applicationGroupIdentifier:"

-- | @Selector@ for @initWithIdentifier:request:applicationGroupIdentifier:priority:@
initWithIdentifier_request_applicationGroupIdentifier_prioritySelector :: Selector '[Id NSString, Id NSURLRequest, Id NSString, CLong] (Id BAURLDownload)
initWithIdentifier_request_applicationGroupIdentifier_prioritySelector = mkSelector "initWithIdentifier:request:applicationGroupIdentifier:priority:"

