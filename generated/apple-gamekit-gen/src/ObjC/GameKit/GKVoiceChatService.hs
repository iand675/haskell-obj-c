{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This a not a Game Center feature. To support voice chat as part of Game Center online play, see GKVoiceChat.
--
-- Generated bindings for @GKVoiceChatService@.
module ObjC.GameKit.GKVoiceChatService
  ( GKVoiceChatService
  , IsGKVoiceChatService(..)
  , defaultVoiceChatService
  , isVoIPAllowed
  , startVoiceChatWithParticipantID_error
  , stopVoiceChatWithParticipantID
  , acceptCallID_error
  , denyCallID
  , receivedRealTimeData_fromParticipantID
  , receivedData_fromParticipantID
  , client
  , setClient
  , microphoneMuted
  , setMicrophoneMuted
  , remoteParticipantVolume
  , setRemoteParticipantVolume
  , outputMeteringEnabled
  , setOutputMeteringEnabled
  , inputMeteringEnabled
  , setInputMeteringEnabled
  , outputMeterLevel
  , inputMeterLevel
  , acceptCallID_errorSelector
  , clientSelector
  , defaultVoiceChatServiceSelector
  , denyCallIDSelector
  , inputMeterLevelSelector
  , inputMeteringEnabledSelector
  , isVoIPAllowedSelector
  , microphoneMutedSelector
  , outputMeterLevelSelector
  , outputMeteringEnabledSelector
  , receivedData_fromParticipantIDSelector
  , receivedRealTimeData_fromParticipantIDSelector
  , remoteParticipantVolumeSelector
  , setClientSelector
  , setInputMeteringEnabledSelector
  , setMicrophoneMutedSelector
  , setOutputMeteringEnabledSelector
  , setRemoteParticipantVolumeSelector
  , startVoiceChatWithParticipantID_errorSelector
  , stopVoiceChatWithParticipantIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultVoiceChatService@
defaultVoiceChatService :: IO (Id GKVoiceChatService)
defaultVoiceChatService  =
  do
    cls' <- getRequiredClass "GKVoiceChatService"
    sendClassMessage cls' defaultVoiceChatServiceSelector

-- | @+ isVoIPAllowed@
isVoIPAllowed :: IO Bool
isVoIPAllowed  =
  do
    cls' <- getRequiredClass "GKVoiceChatService"
    sendClassMessage cls' isVoIPAllowedSelector

-- | @- startVoiceChatWithParticipantID:error:@
startVoiceChatWithParticipantID_error :: (IsGKVoiceChatService gkVoiceChatService, IsNSString participantID, IsNSError error_) => gkVoiceChatService -> participantID -> error_ -> IO Bool
startVoiceChatWithParticipantID_error gkVoiceChatService participantID error_ =
  sendMessage gkVoiceChatService startVoiceChatWithParticipantID_errorSelector (toNSString participantID) (toNSError error_)

-- | @- stopVoiceChatWithParticipantID:@
stopVoiceChatWithParticipantID :: (IsGKVoiceChatService gkVoiceChatService, IsNSString participantID) => gkVoiceChatService -> participantID -> IO ()
stopVoiceChatWithParticipantID gkVoiceChatService participantID =
  sendMessage gkVoiceChatService stopVoiceChatWithParticipantIDSelector (toNSString participantID)

-- | @- acceptCallID:error:@
acceptCallID_error :: (IsGKVoiceChatService gkVoiceChatService, IsNSError error_) => gkVoiceChatService -> CLong -> error_ -> IO Bool
acceptCallID_error gkVoiceChatService callID error_ =
  sendMessage gkVoiceChatService acceptCallID_errorSelector callID (toNSError error_)

-- | @- denyCallID:@
denyCallID :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> CLong -> IO ()
denyCallID gkVoiceChatService callID =
  sendMessage gkVoiceChatService denyCallIDSelector callID

-- | @- receivedRealTimeData:fromParticipantID:@
receivedRealTimeData_fromParticipantID :: (IsGKVoiceChatService gkVoiceChatService, IsNSData audio, IsNSString participantID) => gkVoiceChatService -> audio -> participantID -> IO ()
receivedRealTimeData_fromParticipantID gkVoiceChatService audio participantID =
  sendMessage gkVoiceChatService receivedRealTimeData_fromParticipantIDSelector (toNSData audio) (toNSString participantID)

-- | @- receivedData:fromParticipantID:@
receivedData_fromParticipantID :: (IsGKVoiceChatService gkVoiceChatService, IsNSData arbitraryData, IsNSString participantID) => gkVoiceChatService -> arbitraryData -> participantID -> IO ()
receivedData_fromParticipantID gkVoiceChatService arbitraryData participantID =
  sendMessage gkVoiceChatService receivedData_fromParticipantIDSelector (toNSData arbitraryData) (toNSString participantID)

-- | @- client@
client :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO RawId
client gkVoiceChatService =
  sendMessage gkVoiceChatService clientSelector

-- | @- setClient:@
setClient :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> RawId -> IO ()
setClient gkVoiceChatService value =
  sendMessage gkVoiceChatService setClientSelector value

-- | @- microphoneMuted@
microphoneMuted :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO Bool
microphoneMuted gkVoiceChatService =
  sendMessage gkVoiceChatService microphoneMutedSelector

-- | @- setMicrophoneMuted:@
setMicrophoneMuted :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> Bool -> IO ()
setMicrophoneMuted gkVoiceChatService value =
  sendMessage gkVoiceChatService setMicrophoneMutedSelector value

-- | @- remoteParticipantVolume@
remoteParticipantVolume :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO CFloat
remoteParticipantVolume gkVoiceChatService =
  sendMessage gkVoiceChatService remoteParticipantVolumeSelector

-- | @- setRemoteParticipantVolume:@
setRemoteParticipantVolume :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> CFloat -> IO ()
setRemoteParticipantVolume gkVoiceChatService value =
  sendMessage gkVoiceChatService setRemoteParticipantVolumeSelector value

-- | @- outputMeteringEnabled@
outputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO Bool
outputMeteringEnabled gkVoiceChatService =
  sendMessage gkVoiceChatService outputMeteringEnabledSelector

-- | @- setOutputMeteringEnabled:@
setOutputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> Bool -> IO ()
setOutputMeteringEnabled gkVoiceChatService value =
  sendMessage gkVoiceChatService setOutputMeteringEnabledSelector value

-- | @- inputMeteringEnabled@
inputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO Bool
inputMeteringEnabled gkVoiceChatService =
  sendMessage gkVoiceChatService inputMeteringEnabledSelector

-- | @- setInputMeteringEnabled:@
setInputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> Bool -> IO ()
setInputMeteringEnabled gkVoiceChatService value =
  sendMessage gkVoiceChatService setInputMeteringEnabledSelector value

-- | @- outputMeterLevel@
outputMeterLevel :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO CFloat
outputMeterLevel gkVoiceChatService =
  sendMessage gkVoiceChatService outputMeterLevelSelector

-- | @- inputMeterLevel@
inputMeterLevel :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO CFloat
inputMeterLevel gkVoiceChatService =
  sendMessage gkVoiceChatService inputMeterLevelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultVoiceChatService@
defaultVoiceChatServiceSelector :: Selector '[] (Id GKVoiceChatService)
defaultVoiceChatServiceSelector = mkSelector "defaultVoiceChatService"

-- | @Selector@ for @isVoIPAllowed@
isVoIPAllowedSelector :: Selector '[] Bool
isVoIPAllowedSelector = mkSelector "isVoIPAllowed"

-- | @Selector@ for @startVoiceChatWithParticipantID:error:@
startVoiceChatWithParticipantID_errorSelector :: Selector '[Id NSString, Id NSError] Bool
startVoiceChatWithParticipantID_errorSelector = mkSelector "startVoiceChatWithParticipantID:error:"

-- | @Selector@ for @stopVoiceChatWithParticipantID:@
stopVoiceChatWithParticipantIDSelector :: Selector '[Id NSString] ()
stopVoiceChatWithParticipantIDSelector = mkSelector "stopVoiceChatWithParticipantID:"

-- | @Selector@ for @acceptCallID:error:@
acceptCallID_errorSelector :: Selector '[CLong, Id NSError] Bool
acceptCallID_errorSelector = mkSelector "acceptCallID:error:"

-- | @Selector@ for @denyCallID:@
denyCallIDSelector :: Selector '[CLong] ()
denyCallIDSelector = mkSelector "denyCallID:"

-- | @Selector@ for @receivedRealTimeData:fromParticipantID:@
receivedRealTimeData_fromParticipantIDSelector :: Selector '[Id NSData, Id NSString] ()
receivedRealTimeData_fromParticipantIDSelector = mkSelector "receivedRealTimeData:fromParticipantID:"

-- | @Selector@ for @receivedData:fromParticipantID:@
receivedData_fromParticipantIDSelector :: Selector '[Id NSData, Id NSString] ()
receivedData_fromParticipantIDSelector = mkSelector "receivedData:fromParticipantID:"

-- | @Selector@ for @client@
clientSelector :: Selector '[] RawId
clientSelector = mkSelector "client"

-- | @Selector@ for @setClient:@
setClientSelector :: Selector '[RawId] ()
setClientSelector = mkSelector "setClient:"

-- | @Selector@ for @microphoneMuted@
microphoneMutedSelector :: Selector '[] Bool
microphoneMutedSelector = mkSelector "microphoneMuted"

-- | @Selector@ for @setMicrophoneMuted:@
setMicrophoneMutedSelector :: Selector '[Bool] ()
setMicrophoneMutedSelector = mkSelector "setMicrophoneMuted:"

-- | @Selector@ for @remoteParticipantVolume@
remoteParticipantVolumeSelector :: Selector '[] CFloat
remoteParticipantVolumeSelector = mkSelector "remoteParticipantVolume"

-- | @Selector@ for @setRemoteParticipantVolume:@
setRemoteParticipantVolumeSelector :: Selector '[CFloat] ()
setRemoteParticipantVolumeSelector = mkSelector "setRemoteParticipantVolume:"

-- | @Selector@ for @outputMeteringEnabled@
outputMeteringEnabledSelector :: Selector '[] Bool
outputMeteringEnabledSelector = mkSelector "outputMeteringEnabled"

-- | @Selector@ for @setOutputMeteringEnabled:@
setOutputMeteringEnabledSelector :: Selector '[Bool] ()
setOutputMeteringEnabledSelector = mkSelector "setOutputMeteringEnabled:"

-- | @Selector@ for @inputMeteringEnabled@
inputMeteringEnabledSelector :: Selector '[] Bool
inputMeteringEnabledSelector = mkSelector "inputMeteringEnabled"

-- | @Selector@ for @setInputMeteringEnabled:@
setInputMeteringEnabledSelector :: Selector '[Bool] ()
setInputMeteringEnabledSelector = mkSelector "setInputMeteringEnabled:"

-- | @Selector@ for @outputMeterLevel@
outputMeterLevelSelector :: Selector '[] CFloat
outputMeterLevelSelector = mkSelector "outputMeterLevel"

-- | @Selector@ for @inputMeterLevel@
inputMeterLevelSelector :: Selector '[] CFloat
inputMeterLevelSelector = mkSelector "inputMeterLevel"

