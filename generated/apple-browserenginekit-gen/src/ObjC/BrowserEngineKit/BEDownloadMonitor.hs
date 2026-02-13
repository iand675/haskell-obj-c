{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BEDownloadMonitor@.
module ObjC.BrowserEngineKit.BEDownloadMonitor
  ( BEDownloadMonitor
  , IsBEDownloadMonitor(..)
  , init_
  , new
  , initWithSourceURL_destinationURL_observedProgress_liveActivityAccessToken
  , useDownloadsFolderWithPlaceholderType_finalFileCreatedHandler
  , beginMonitoring
  , resumeMonitoring_completionHandler
  , createAccessToken
  , identifier
  , sourceURL
  , destinationURL
  , beginMonitoringSelector
  , createAccessTokenSelector
  , destinationURLSelector
  , identifierSelector
  , initSelector
  , initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector
  , newSelector
  , resumeMonitoring_completionHandlerSelector
  , sourceURLSelector
  , useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id BEDownloadMonitor)
init_ beDownloadMonitor =
  sendOwnedMessage beDownloadMonitor initSelector

-- | @+ new@
new :: IO (Id BEDownloadMonitor)
new  =
  do
    cls' <- getRequiredClass "BEDownloadMonitor"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:@
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessToken :: (IsBEDownloadMonitor beDownloadMonitor, IsNSURL sourceURL, IsNSURL destinationURL, IsNSProgress observedProgress, IsNSData liveActivityAccessToken) => beDownloadMonitor -> sourceURL -> destinationURL -> observedProgress -> liveActivityAccessToken -> IO (Id BEDownloadMonitor)
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessToken beDownloadMonitor sourceURL destinationURL observedProgress liveActivityAccessToken =
  sendOwnedMessage beDownloadMonitor initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector (toNSURL sourceURL) (toNSURL destinationURL) (toNSProgress observedProgress) (toNSData liveActivityAccessToken)

-- | @- useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:@
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandler :: (IsBEDownloadMonitor beDownloadMonitor, IsUTType type_) => beDownloadMonitor -> type_ -> Ptr () -> IO ()
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandler beDownloadMonitor type_ finalFileCreatedHandler =
  sendMessage beDownloadMonitor useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector (toUTType type_) finalFileCreatedHandler

-- | @- beginMonitoring:@
beginMonitoring :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> Ptr () -> IO ()
beginMonitoring beDownloadMonitor completion =
  sendMessage beDownloadMonitor beginMonitoringSelector completion

-- | @- resumeMonitoring:completionHandler:@
resumeMonitoring_completionHandler :: (IsBEDownloadMonitor beDownloadMonitor, IsNSURL url) => beDownloadMonitor -> url -> Ptr () -> IO ()
resumeMonitoring_completionHandler beDownloadMonitor url completionHandler =
  sendMessage beDownloadMonitor resumeMonitoring_completionHandlerSelector (toNSURL url) completionHandler

-- | @+ createAccessToken@
createAccessToken :: IO (Id NSData)
createAccessToken  =
  do
    cls' <- getRequiredClass "BEDownloadMonitor"
    sendClassMessage cls' createAccessTokenSelector

-- | @- identifier@
identifier :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id NSUUID)
identifier beDownloadMonitor =
  sendMessage beDownloadMonitor identifierSelector

-- | @- sourceURL@
sourceURL :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id NSURL)
sourceURL beDownloadMonitor =
  sendMessage beDownloadMonitor sourceURLSelector

-- | @- destinationURL@
destinationURL :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id NSURL)
destinationURL beDownloadMonitor =
  sendMessage beDownloadMonitor destinationURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BEDownloadMonitor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BEDownloadMonitor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:@
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector :: Selector '[Id NSURL, Id NSURL, Id NSProgress, Id NSData] (Id BEDownloadMonitor)
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector = mkSelector "initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:"

-- | @Selector@ for @useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:@
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector :: Selector '[Id UTType, Ptr ()] ()
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector = mkSelector "useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:"

-- | @Selector@ for @beginMonitoring:@
beginMonitoringSelector :: Selector '[Ptr ()] ()
beginMonitoringSelector = mkSelector "beginMonitoring:"

-- | @Selector@ for @resumeMonitoring:completionHandler:@
resumeMonitoring_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
resumeMonitoring_completionHandlerSelector = mkSelector "resumeMonitoring:completionHandler:"

-- | @Selector@ for @createAccessToken@
createAccessTokenSelector :: Selector '[] (Id NSData)
createAccessTokenSelector = mkSelector "createAccessToken"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @sourceURL@
sourceURLSelector :: Selector '[] (Id NSURL)
sourceURLSelector = mkSelector "sourceURL"

-- | @Selector@ for @destinationURL@
destinationURLSelector :: Selector '[] (Id NSURL)
destinationURLSelector = mkSelector "destinationURL"

