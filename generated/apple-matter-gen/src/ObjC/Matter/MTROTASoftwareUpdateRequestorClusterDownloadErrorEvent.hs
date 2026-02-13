{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateRequestorClusterDownloadErrorEvent@.
module ObjC.Matter.MTROTASoftwareUpdateRequestorClusterDownloadErrorEvent
  ( MTROTASoftwareUpdateRequestorClusterDownloadErrorEvent
  , IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , bytesDownloaded
  , setBytesDownloaded
  , progressPercent
  , setProgressPercent
  , platformCode
  , setPlatformCode
  , bytesDownloadedSelector
  , platformCodeSelector
  , progressPercentSelector
  , setBytesDownloadedSelector
  , setPlatformCodeSelector
  , setProgressPercentSelector
  , setSoftwareVersionSelector
  , softwareVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- softwareVersion@
softwareVersion :: IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
softwareVersion mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setSoftwareVersion mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent setSoftwareVersionSelector (toNSNumber value)

-- | @- bytesDownloaded@
bytesDownloaded :: IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
bytesDownloaded mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent bytesDownloadedSelector

-- | @- setBytesDownloaded:@
setBytesDownloaded :: (IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setBytesDownloaded mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent setBytesDownloadedSelector (toNSNumber value)

-- | @- progressPercent@
progressPercent :: IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
progressPercent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent progressPercentSelector

-- | @- setProgressPercent:@
setProgressPercent :: (IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setProgressPercent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent setProgressPercentSelector (toNSNumber value)

-- | @- platformCode@
platformCode :: IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
platformCode mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent platformCodeSelector

-- | @- setPlatformCode:@
setPlatformCode :: (IsMTROTASoftwareUpdateRequestorClusterDownloadErrorEvent mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setPlatformCode mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterDownloadErrorEvent setPlatformCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @bytesDownloaded@
bytesDownloadedSelector :: Selector '[] (Id NSNumber)
bytesDownloadedSelector = mkSelector "bytesDownloaded"

-- | @Selector@ for @setBytesDownloaded:@
setBytesDownloadedSelector :: Selector '[Id NSNumber] ()
setBytesDownloadedSelector = mkSelector "setBytesDownloaded:"

-- | @Selector@ for @progressPercent@
progressPercentSelector :: Selector '[] (Id NSNumber)
progressPercentSelector = mkSelector "progressPercent"

-- | @Selector@ for @setProgressPercent:@
setProgressPercentSelector :: Selector '[Id NSNumber] ()
setProgressPercentSelector = mkSelector "setProgressPercent:"

-- | @Selector@ for @platformCode@
platformCodeSelector :: Selector '[] (Id NSNumber)
platformCodeSelector = mkSelector "platformCode"

-- | @Selector@ for @setPlatformCode:@
setPlatformCodeSelector :: Selector '[Id NSNumber] ()
setPlatformCodeSelector = mkSelector "setPlatformCode:"

