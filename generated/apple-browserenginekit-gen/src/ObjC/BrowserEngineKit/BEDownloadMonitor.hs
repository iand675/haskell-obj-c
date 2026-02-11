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
  , initSelector
  , newSelector
  , initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector
  , useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector
  , beginMonitoringSelector
  , resumeMonitoring_completionHandlerSelector
  , createAccessTokenSelector
  , identifierSelector
  , sourceURLSelector
  , destinationURLSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id BEDownloadMonitor)
init_ beDownloadMonitor  =
    sendMsg beDownloadMonitor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BEDownloadMonitor)
new  =
  do
    cls' <- getRequiredClass "BEDownloadMonitor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:@
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessToken :: (IsBEDownloadMonitor beDownloadMonitor, IsNSURL sourceURL, IsNSURL destinationURL, IsNSProgress observedProgress, IsNSData liveActivityAccessToken) => beDownloadMonitor -> sourceURL -> destinationURL -> observedProgress -> liveActivityAccessToken -> IO (Id BEDownloadMonitor)
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessToken beDownloadMonitor  sourceURL destinationURL observedProgress liveActivityAccessToken =
  withObjCPtr sourceURL $ \raw_sourceURL ->
    withObjCPtr destinationURL $ \raw_destinationURL ->
      withObjCPtr observedProgress $ \raw_observedProgress ->
        withObjCPtr liveActivityAccessToken $ \raw_liveActivityAccessToken ->
            sendMsg beDownloadMonitor (mkSelector "initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:") (retPtr retVoid) [argPtr (castPtr raw_sourceURL :: Ptr ()), argPtr (castPtr raw_destinationURL :: Ptr ()), argPtr (castPtr raw_observedProgress :: Ptr ()), argPtr (castPtr raw_liveActivityAccessToken :: Ptr ())] >>= ownedObject . castPtr

-- | @- useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:@
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandler :: (IsBEDownloadMonitor beDownloadMonitor, IsUTType type_) => beDownloadMonitor -> type_ -> Ptr () -> IO ()
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandler beDownloadMonitor  type_ finalFileCreatedHandler =
  withObjCPtr type_ $ \raw_type_ ->
      sendMsg beDownloadMonitor (mkSelector "useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr finalFileCreatedHandler :: Ptr ())]

-- | @- beginMonitoring:@
beginMonitoring :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> Ptr () -> IO ()
beginMonitoring beDownloadMonitor  completion =
    sendMsg beDownloadMonitor (mkSelector "beginMonitoring:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- resumeMonitoring:completionHandler:@
resumeMonitoring_completionHandler :: (IsBEDownloadMonitor beDownloadMonitor, IsNSURL url) => beDownloadMonitor -> url -> Ptr () -> IO ()
resumeMonitoring_completionHandler beDownloadMonitor  url completionHandler =
  withObjCPtr url $ \raw_url ->
      sendMsg beDownloadMonitor (mkSelector "resumeMonitoring:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ createAccessToken@
createAccessToken :: IO (Id NSData)
createAccessToken  =
  do
    cls' <- getRequiredClass "BEDownloadMonitor"
    sendClassMsg cls' (mkSelector "createAccessToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id NSUUID)
identifier beDownloadMonitor  =
    sendMsg beDownloadMonitor (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceURL@
sourceURL :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id NSURL)
sourceURL beDownloadMonitor  =
    sendMsg beDownloadMonitor (mkSelector "sourceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destinationURL@
destinationURL :: IsBEDownloadMonitor beDownloadMonitor => beDownloadMonitor -> IO (Id NSURL)
destinationURL beDownloadMonitor  =
    sendMsg beDownloadMonitor (mkSelector "destinationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:@
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector :: Selector
initWithSourceURL_destinationURL_observedProgress_liveActivityAccessTokenSelector = mkSelector "initWithSourceURL:destinationURL:observedProgress:liveActivityAccessToken:"

-- | @Selector@ for @useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:@
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector :: Selector
useDownloadsFolderWithPlaceholderType_finalFileCreatedHandlerSelector = mkSelector "useDownloadsFolderWithPlaceholderType:finalFileCreatedHandler:"

-- | @Selector@ for @beginMonitoring:@
beginMonitoringSelector :: Selector
beginMonitoringSelector = mkSelector "beginMonitoring:"

-- | @Selector@ for @resumeMonitoring:completionHandler:@
resumeMonitoring_completionHandlerSelector :: Selector
resumeMonitoring_completionHandlerSelector = mkSelector "resumeMonitoring:completionHandler:"

-- | @Selector@ for @createAccessToken@
createAccessTokenSelector :: Selector
createAccessTokenSelector = mkSelector "createAccessToken"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @sourceURL@
sourceURLSelector :: Selector
sourceURLSelector = mkSelector "sourceURL"

-- | @Selector@ for @destinationURL@
destinationURLSelector :: Selector
destinationURLSelector = mkSelector "destinationURL"

