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
  , defaultVoiceChatServiceSelector
  , isVoIPAllowedSelector
  , startVoiceChatWithParticipantID_errorSelector
  , stopVoiceChatWithParticipantIDSelector
  , acceptCallID_errorSelector
  , denyCallIDSelector
  , receivedRealTimeData_fromParticipantIDSelector
  , receivedData_fromParticipantIDSelector
  , microphoneMutedSelector
  , setMicrophoneMutedSelector
  , remoteParticipantVolumeSelector
  , setRemoteParticipantVolumeSelector
  , outputMeteringEnabledSelector
  , setOutputMeteringEnabledSelector
  , inputMeteringEnabledSelector
  , setInputMeteringEnabledSelector
  , outputMeterLevelSelector
  , inputMeterLevelSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultVoiceChatService@
defaultVoiceChatService :: IO (Id GKVoiceChatService)
defaultVoiceChatService  =
  do
    cls' <- getRequiredClass "GKVoiceChatService"
    sendClassMsg cls' (mkSelector "defaultVoiceChatService") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ isVoIPAllowed@
isVoIPAllowed :: IO Bool
isVoIPAllowed  =
  do
    cls' <- getRequiredClass "GKVoiceChatService"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isVoIPAllowed") retCULong []

-- | @- startVoiceChatWithParticipantID:error:@
startVoiceChatWithParticipantID_error :: (IsGKVoiceChatService gkVoiceChatService, IsNSString participantID, IsNSError error_) => gkVoiceChatService -> participantID -> error_ -> IO Bool
startVoiceChatWithParticipantID_error gkVoiceChatService  participantID error_ =
withObjCPtr participantID $ \raw_participantID ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoiceChatService (mkSelector "startVoiceChatWithParticipantID:error:") retCULong [argPtr (castPtr raw_participantID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- stopVoiceChatWithParticipantID:@
stopVoiceChatWithParticipantID :: (IsGKVoiceChatService gkVoiceChatService, IsNSString participantID) => gkVoiceChatService -> participantID -> IO ()
stopVoiceChatWithParticipantID gkVoiceChatService  participantID =
withObjCPtr participantID $ \raw_participantID ->
    sendMsg gkVoiceChatService (mkSelector "stopVoiceChatWithParticipantID:") retVoid [argPtr (castPtr raw_participantID :: Ptr ())]

-- | @- acceptCallID:error:@
acceptCallID_error :: (IsGKVoiceChatService gkVoiceChatService, IsNSError error_) => gkVoiceChatService -> CLong -> error_ -> IO Bool
acceptCallID_error gkVoiceChatService  callID error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoiceChatService (mkSelector "acceptCallID:error:") retCULong [argCLong (fromIntegral callID), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- denyCallID:@
denyCallID :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> CLong -> IO ()
denyCallID gkVoiceChatService  callID =
  sendMsg gkVoiceChatService (mkSelector "denyCallID:") retVoid [argCLong (fromIntegral callID)]

-- | @- receivedRealTimeData:fromParticipantID:@
receivedRealTimeData_fromParticipantID :: (IsGKVoiceChatService gkVoiceChatService, IsNSData audio, IsNSString participantID) => gkVoiceChatService -> audio -> participantID -> IO ()
receivedRealTimeData_fromParticipantID gkVoiceChatService  audio participantID =
withObjCPtr audio $ \raw_audio ->
  withObjCPtr participantID $ \raw_participantID ->
      sendMsg gkVoiceChatService (mkSelector "receivedRealTimeData:fromParticipantID:") retVoid [argPtr (castPtr raw_audio :: Ptr ()), argPtr (castPtr raw_participantID :: Ptr ())]

-- | @- receivedData:fromParticipantID:@
receivedData_fromParticipantID :: (IsGKVoiceChatService gkVoiceChatService, IsNSData arbitraryData, IsNSString participantID) => gkVoiceChatService -> arbitraryData -> participantID -> IO ()
receivedData_fromParticipantID gkVoiceChatService  arbitraryData participantID =
withObjCPtr arbitraryData $ \raw_arbitraryData ->
  withObjCPtr participantID $ \raw_participantID ->
      sendMsg gkVoiceChatService (mkSelector "receivedData:fromParticipantID:") retVoid [argPtr (castPtr raw_arbitraryData :: Ptr ()), argPtr (castPtr raw_participantID :: Ptr ())]

-- | @- microphoneMuted@
microphoneMuted :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO Bool
microphoneMuted gkVoiceChatService  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoiceChatService (mkSelector "microphoneMuted") retCULong []

-- | @- setMicrophoneMuted:@
setMicrophoneMuted :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> Bool -> IO ()
setMicrophoneMuted gkVoiceChatService  value =
  sendMsg gkVoiceChatService (mkSelector "setMicrophoneMuted:") retVoid [argCULong (if value then 1 else 0)]

-- | @- remoteParticipantVolume@
remoteParticipantVolume :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO CFloat
remoteParticipantVolume gkVoiceChatService  =
  sendMsg gkVoiceChatService (mkSelector "remoteParticipantVolume") retCFloat []

-- | @- setRemoteParticipantVolume:@
setRemoteParticipantVolume :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> CFloat -> IO ()
setRemoteParticipantVolume gkVoiceChatService  value =
  sendMsg gkVoiceChatService (mkSelector "setRemoteParticipantVolume:") retVoid [argCFloat (fromIntegral value)]

-- | @- outputMeteringEnabled@
outputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO Bool
outputMeteringEnabled gkVoiceChatService  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoiceChatService (mkSelector "outputMeteringEnabled") retCULong []

-- | @- setOutputMeteringEnabled:@
setOutputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> Bool -> IO ()
setOutputMeteringEnabled gkVoiceChatService  value =
  sendMsg gkVoiceChatService (mkSelector "setOutputMeteringEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- inputMeteringEnabled@
inputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO Bool
inputMeteringEnabled gkVoiceChatService  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkVoiceChatService (mkSelector "inputMeteringEnabled") retCULong []

-- | @- setInputMeteringEnabled:@
setInputMeteringEnabled :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> Bool -> IO ()
setInputMeteringEnabled gkVoiceChatService  value =
  sendMsg gkVoiceChatService (mkSelector "setInputMeteringEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- outputMeterLevel@
outputMeterLevel :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO CFloat
outputMeterLevel gkVoiceChatService  =
  sendMsg gkVoiceChatService (mkSelector "outputMeterLevel") retCFloat []

-- | @- inputMeterLevel@
inputMeterLevel :: IsGKVoiceChatService gkVoiceChatService => gkVoiceChatService -> IO CFloat
inputMeterLevel gkVoiceChatService  =
  sendMsg gkVoiceChatService (mkSelector "inputMeterLevel") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultVoiceChatService@
defaultVoiceChatServiceSelector :: Selector
defaultVoiceChatServiceSelector = mkSelector "defaultVoiceChatService"

-- | @Selector@ for @isVoIPAllowed@
isVoIPAllowedSelector :: Selector
isVoIPAllowedSelector = mkSelector "isVoIPAllowed"

-- | @Selector@ for @startVoiceChatWithParticipantID:error:@
startVoiceChatWithParticipantID_errorSelector :: Selector
startVoiceChatWithParticipantID_errorSelector = mkSelector "startVoiceChatWithParticipantID:error:"

-- | @Selector@ for @stopVoiceChatWithParticipantID:@
stopVoiceChatWithParticipantIDSelector :: Selector
stopVoiceChatWithParticipantIDSelector = mkSelector "stopVoiceChatWithParticipantID:"

-- | @Selector@ for @acceptCallID:error:@
acceptCallID_errorSelector :: Selector
acceptCallID_errorSelector = mkSelector "acceptCallID:error:"

-- | @Selector@ for @denyCallID:@
denyCallIDSelector :: Selector
denyCallIDSelector = mkSelector "denyCallID:"

-- | @Selector@ for @receivedRealTimeData:fromParticipantID:@
receivedRealTimeData_fromParticipantIDSelector :: Selector
receivedRealTimeData_fromParticipantIDSelector = mkSelector "receivedRealTimeData:fromParticipantID:"

-- | @Selector@ for @receivedData:fromParticipantID:@
receivedData_fromParticipantIDSelector :: Selector
receivedData_fromParticipantIDSelector = mkSelector "receivedData:fromParticipantID:"

-- | @Selector@ for @microphoneMuted@
microphoneMutedSelector :: Selector
microphoneMutedSelector = mkSelector "microphoneMuted"

-- | @Selector@ for @setMicrophoneMuted:@
setMicrophoneMutedSelector :: Selector
setMicrophoneMutedSelector = mkSelector "setMicrophoneMuted:"

-- | @Selector@ for @remoteParticipantVolume@
remoteParticipantVolumeSelector :: Selector
remoteParticipantVolumeSelector = mkSelector "remoteParticipantVolume"

-- | @Selector@ for @setRemoteParticipantVolume:@
setRemoteParticipantVolumeSelector :: Selector
setRemoteParticipantVolumeSelector = mkSelector "setRemoteParticipantVolume:"

-- | @Selector@ for @outputMeteringEnabled@
outputMeteringEnabledSelector :: Selector
outputMeteringEnabledSelector = mkSelector "outputMeteringEnabled"

-- | @Selector@ for @setOutputMeteringEnabled:@
setOutputMeteringEnabledSelector :: Selector
setOutputMeteringEnabledSelector = mkSelector "setOutputMeteringEnabled:"

-- | @Selector@ for @inputMeteringEnabled@
inputMeteringEnabledSelector :: Selector
inputMeteringEnabledSelector = mkSelector "inputMeteringEnabled"

-- | @Selector@ for @setInputMeteringEnabled:@
setInputMeteringEnabledSelector :: Selector
setInputMeteringEnabledSelector = mkSelector "setInputMeteringEnabled:"

-- | @Selector@ for @outputMeterLevel@
outputMeterLevelSelector :: Selector
outputMeterLevelSelector = mkSelector "outputMeterLevel"

-- | @Selector@ for @inputMeterLevel@
inputMeterLevelSelector :: Selector
inputMeterLevelSelector = mkSelector "inputMeterLevel"

