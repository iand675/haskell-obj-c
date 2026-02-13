{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSoftwareDiagnosticsClusterSoftwareFaultEvent@.
module ObjC.Matter.MTRSoftwareDiagnosticsClusterSoftwareFaultEvent
  ( MTRSoftwareDiagnosticsClusterSoftwareFaultEvent
  , IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent(..)
  , id_
  , setId
  , name
  , setName
  , faultRecording
  , setFaultRecording
  , faultRecordingSelector
  , idSelector
  , nameSelector
  , setFaultRecordingSelector
  , setIdSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- id@
id_ :: IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> IO (Id NSNumber)
id_ mtrSoftwareDiagnosticsClusterSoftwareFaultEvent =
  sendMessage mtrSoftwareDiagnosticsClusterSoftwareFaultEvent idSelector

-- | @- setId:@
setId :: (IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent, IsNSNumber value) => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> value -> IO ()
setId mtrSoftwareDiagnosticsClusterSoftwareFaultEvent value =
  sendMessage mtrSoftwareDiagnosticsClusterSoftwareFaultEvent setIdSelector (toNSNumber value)

-- | @- name@
name :: IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> IO (Id NSString)
name mtrSoftwareDiagnosticsClusterSoftwareFaultEvent =
  sendMessage mtrSoftwareDiagnosticsClusterSoftwareFaultEvent nameSelector

-- | @- setName:@
setName :: (IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent, IsNSString value) => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> value -> IO ()
setName mtrSoftwareDiagnosticsClusterSoftwareFaultEvent value =
  sendMessage mtrSoftwareDiagnosticsClusterSoftwareFaultEvent setNameSelector (toNSString value)

-- | @- faultRecording@
faultRecording :: IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> IO (Id NSData)
faultRecording mtrSoftwareDiagnosticsClusterSoftwareFaultEvent =
  sendMessage mtrSoftwareDiagnosticsClusterSoftwareFaultEvent faultRecordingSelector

-- | @- setFaultRecording:@
setFaultRecording :: (IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent, IsNSData value) => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> value -> IO ()
setFaultRecording mtrSoftwareDiagnosticsClusterSoftwareFaultEvent value =
  sendMessage mtrSoftwareDiagnosticsClusterSoftwareFaultEvent setFaultRecordingSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector '[] (Id NSNumber)
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector '[Id NSNumber] ()
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @faultRecording@
faultRecordingSelector :: Selector '[] (Id NSData)
faultRecordingSelector = mkSelector "faultRecording"

-- | @Selector@ for @setFaultRecording:@
setFaultRecordingSelector :: Selector '[Id NSData] ()
setFaultRecordingSelector = mkSelector "setFaultRecording:"

