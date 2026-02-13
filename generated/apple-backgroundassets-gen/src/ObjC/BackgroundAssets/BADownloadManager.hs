{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BADownloadManager@.
module ObjC.BackgroundAssets.BADownloadManager
  ( BADownloadManager
  , IsBADownloadManager(..)
  , init_
  , new
  , fetchCurrentDownloads
  , scheduleDownload_error
  , performWithExclusiveControl
  , performWithExclusiveControlBeforeDate_performHandler
  , startForegroundDownload_error
  , cancelDownload_error
  , sharedManager
  , delegate
  , setDelegate
  , cancelDownload_errorSelector
  , delegateSelector
  , fetchCurrentDownloadsSelector
  , initSelector
  , newSelector
  , performWithExclusiveControlBeforeDate_performHandlerSelector
  , performWithExclusiveControlSelector
  , scheduleDownload_errorSelector
  , setDelegateSelector
  , sharedManagerSelector
  , startForegroundDownload_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBADownloadManager baDownloadManager => baDownloadManager -> IO (Id BADownloadManager)
init_ baDownloadManager =
  sendOwnedMessage baDownloadManager initSelector

-- | @+ new@
new :: IO (Id BADownloadManager)
new  =
  do
    cls' <- getRequiredClass "BADownloadManager"
    sendOwnedClassMessage cls' newSelector

-- | Fetches current downloads.
--
-- Fetches the current list of scheduled or in-flight downloads queued by your application or extension.
--
-- @error@ — An error representing why the downloads could not be fetched.
--
-- Returns: On success, returns a list of scheduled or in-flight downloads. On failure, returns nil and sets @error.@
--
-- Warning: This method can block and should not be called from the main thread.
--
-- BADownloadManager:fetchCurrentDownloadsWithCompletionHandler
--
-- ObjC selector: @- fetchCurrentDownloads:@
fetchCurrentDownloads :: (IsBADownloadManager baDownloadManager, IsNSError error_) => baDownloadManager -> error_ -> IO (Id NSArray)
fetchCurrentDownloads baDownloadManager error_ =
  sendMessage baDownloadManager fetchCurrentDownloadsSelector (toNSError error_)

-- | Schedules a background download.
--
-- Specifies a download to schedule at a given priority. The download will automatically start at the discretion of the system.
--
-- @download@ — A BADownload object representing a URL to be downloaded.
--
-- @error@ — A NSError representing why the BADownload could not be scheduled.
--
-- Returns: YES if @download@ was scheduled. NO and @error@ set if the download could not be scheduled.
--
-- ObjC selector: @- scheduleDownload:error:@
scheduleDownload_error :: (IsBADownloadManager baDownloadManager, IsBADownload download, IsNSError error_) => baDownloadManager -> download -> error_ -> IO Bool
scheduleDownload_error baDownloadManager download error_ =
  sendMessage baDownloadManager scheduleDownload_errorSelector (toBADownload download) (toNSError error_)

-- | Acquires exclusive access to the BADownloadManager across the app and application extension.
--
-- Acquires exclusive access to the BADownloadManager across the app and application extension. This ensures that your extension and app do not perform operations at the same time. Both the extension and app must use this API to ensure exclusive access.
--
-- @performHandler@ — A block that will be executed once exclusive control is acquired. If an error is non-nil then a problem occurred acquiring exclusive access.
--
-- ObjC selector: @- performWithExclusiveControl:@
performWithExclusiveControl :: IsBADownloadManager baDownloadManager => baDownloadManager -> Ptr () -> IO ()
performWithExclusiveControl baDownloadManager performHandler =
  sendMessage baDownloadManager performWithExclusiveControlSelector performHandler

-- | Acquires exclusive access to the BADownloadManager across the app and application extension.
--
-- Acquires exclusive access to the BADownloadManager across the app and application extension. This ensures that your extension and app do not perform operations at the same time. Both the extension and app must use this API to ensure exclusive access.
--
-- @date@ — A date by which you want exclusive control acquired. If you pass +[NSDate date], control will attempt to be acquired and if it can not be, it will fail instantly.
--
-- @performHandler@ — A block that will be executed once exclusive control is acquired. If an error is non-nil then a problem occurred acquiring exclusive access.
--
-- ObjC selector: @- performWithExclusiveControlBeforeDate:performHandler:@
performWithExclusiveControlBeforeDate_performHandler :: (IsBADownloadManager baDownloadManager, IsNSDate date) => baDownloadManager -> date -> Ptr () -> IO ()
performWithExclusiveControlBeforeDate_performHandler baDownloadManager date performHandler =
  sendMessage baDownloadManager performWithExclusiveControlBeforeDate_performHandlerSelector (toNSDate date) performHandler

-- | Attempts to schedule a BADownload in foreground mode.
--
-- Attempts to schedule a BADownload in foreground mode. This download will start (if it has not been started) immediately regrardlesss of battery or network status. The download will remain in this foreground until the download manager is disconnected. This API only functions if the download manager is created in the application and not the download extension. If this API is called from the download extension, NO will be returned along with a NSError with the settings BAErrorDomain : BAErrorCodeCallFromExtensionNotAllowed. If this API is called from a app while it is in the background, NO will be returned along with a NSError with the settings BAErrorDomain : BAErrorCodeCallFromInactiveProcessNotAllowed.
--
-- ObjC selector: @- startForegroundDownload:error:@
startForegroundDownload_error :: (IsBADownloadManager baDownloadManager, IsBADownload download, IsNSError error_) => baDownloadManager -> download -> error_ -> IO Bool
startForegroundDownload_error baDownloadManager download error_ =
  sendMessage baDownloadManager startForegroundDownload_errorSelector (toBADownload download) (toNSError error_)

-- | Cancels a download.
--
-- Attempts to cancel a BADownload. If the download has not been schduled or has already completed, NO is returned along with a NSError set to BAErrorDomain : BAErrorCodeDownloadNotScheduled.
--
-- Returns: YES if the download is canceled. NO if the download could not be canceled, @error@ will be set with a reason why.
--
-- ObjC selector: @- cancelDownload:error:@
cancelDownload_error :: (IsBADownloadManager baDownloadManager, IsBADownload download, IsNSError error_) => baDownloadManager -> download -> error_ -> IO Bool
cancelDownload_error baDownloadManager download error_ =
  sendMessage baDownloadManager cancelDownload_errorSelector (toBADownload download) (toNSError error_)

-- | Gets the singleton downloader object.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id BADownloadManager)
sharedManager  =
  do
    cls' <- getRequiredClass "BADownloadManager"
    sendClassMessage cls' sharedManagerSelector

-- | A object confroming to BADownloadManagerDelegate to get notified when actions occur.
--
-- ObjC selector: @- delegate@
delegate :: IsBADownloadManager baDownloadManager => baDownloadManager -> IO RawId
delegate baDownloadManager =
  sendMessage baDownloadManager delegateSelector

-- | A object confroming to BADownloadManagerDelegate to get notified when actions occur.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsBADownloadManager baDownloadManager => baDownloadManager -> RawId -> IO ()
setDelegate baDownloadManager value =
  sendMessage baDownloadManager setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BADownloadManager)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BADownloadManager)
newSelector = mkSelector "new"

-- | @Selector@ for @fetchCurrentDownloads:@
fetchCurrentDownloadsSelector :: Selector '[Id NSError] (Id NSArray)
fetchCurrentDownloadsSelector = mkSelector "fetchCurrentDownloads:"

-- | @Selector@ for @scheduleDownload:error:@
scheduleDownload_errorSelector :: Selector '[Id BADownload, Id NSError] Bool
scheduleDownload_errorSelector = mkSelector "scheduleDownload:error:"

-- | @Selector@ for @performWithExclusiveControl:@
performWithExclusiveControlSelector :: Selector '[Ptr ()] ()
performWithExclusiveControlSelector = mkSelector "performWithExclusiveControl:"

-- | @Selector@ for @performWithExclusiveControlBeforeDate:performHandler:@
performWithExclusiveControlBeforeDate_performHandlerSelector :: Selector '[Id NSDate, Ptr ()] ()
performWithExclusiveControlBeforeDate_performHandlerSelector = mkSelector "performWithExclusiveControlBeforeDate:performHandler:"

-- | @Selector@ for @startForegroundDownload:error:@
startForegroundDownload_errorSelector :: Selector '[Id BADownload, Id NSError] Bool
startForegroundDownload_errorSelector = mkSelector "startForegroundDownload:error:"

-- | @Selector@ for @cancelDownload:error:@
cancelDownload_errorSelector :: Selector '[Id BADownload, Id NSError] Bool
cancelDownload_errorSelector = mkSelector "cancelDownload:error:"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id BADownloadManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

