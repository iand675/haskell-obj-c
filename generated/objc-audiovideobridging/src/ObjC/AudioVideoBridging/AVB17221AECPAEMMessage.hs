{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPAEMMessage
--
-- AVB17221AECPAEMMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, AVDECC Entity Model message.
--
-- AVB17221AECPAEMMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), AVDECC Entity Model (AEM) message. This class is a concrete subclass of AVB17221AECPMessage which provides support for the AEM messages.
--
-- Generated bindings for @AVB17221AECPAEMMessage@.
module ObjC.AudioVideoBridging.AVB17221AECPAEMMessage
  ( AVB17221AECPAEMMessage
  , IsAVB17221AECPAEMMessage(..)
  , commandMessage
  , responseMessage
  , responseMessageFromCommandMessage
  , commandType
  , setCommandType
  , unsolicited
  , setUnsolicited
  , controllerRequest
  , setControllerRequest
  , commandSpecificData
  , setCommandSpecificData
  , commandMessageSelector
  , responseMessageSelector
  , responseMessageFromCommandMessageSelector
  , commandTypeSelector
  , setCommandTypeSelector
  , unsolicitedSelector
  , setUnsolicitedSelector
  , controllerRequestSelector
  , setControllerRequestSelector
  , commandSpecificDataSelector
  , setCommandSpecificDataSelector

  -- * Enum types
  , AVB17221AEMCommandType(AVB17221AEMCommandType)
  , pattern AVB17221AEMCommandTypeAcquireEntity
  , pattern AVB17221AEMCommandTypeLockEntity
  , pattern AVB17221AEMCommandTypeEntityAvailable
  , pattern AVB17221AEMCommandTypeControllerAvailable
  , pattern AVB17221AEMCommandTypeReadDescriptor
  , pattern AVB17221AEMCommandTypeWriteDescriptor
  , pattern AVB17221AEMCommandTypeSetConfiguration
  , pattern AVB17221AEMCommandTypeGetConfiguration
  , pattern AVB17221AEMCommandTypeSetStreamFormat
  , pattern AVB17221AEMCommandTypeGetStreamFormat
  , pattern AVB17221AEMCommandTypeSetVideoFormat
  , pattern AVB17221AEMCommandTypeGetVideoFormat
  , pattern AVB17221AEMCommandTypeSetSensorFormat
  , pattern AVB17221AEMCommandTypeGetSensorFormat
  , pattern AVB17221AEMCommandTypeSetStreamInfo
  , pattern AVB17221AEMCommandTypeGetStreamInfo
  , pattern AVB17221AEMCommandTypeSetName
  , pattern AVB17221AEMCommandTypeGetName
  , pattern AVB17221AEMCommandTypeSetAssociationID
  , pattern AVB17221AEMCommandTypeGetAssociationID
  , pattern AVB17221AEMCommandTypeSetSamplingRate
  , pattern AVB17221AEMCommandTypeGetSamplingRate
  , pattern AVB17221AEMCommandTypeSetClockSource
  , pattern AVB17221AEMCommandTypeGetClockSource
  , pattern AVB17221AEMCommandTypeSetControl
  , pattern AVB17221AEMCommandTypeGetControl
  , pattern AVB17221AEMCommandTypeIncrementControl
  , pattern AVB17221AEMCommandTypeDecrementControl
  , pattern AVB17221AEMCommandTypeSetSignalSelector
  , pattern AVB17221AEMCommandTypeGetSignalSelector
  , pattern AVB17221AEMCommandTypeSetMixer
  , pattern AVB17221AEMCommandTypeGetMixer
  , pattern AVB17221AEMCommandTypeSetMatrix
  , pattern AVB17221AEMCommandTypeGetMatrix
  , pattern AVB17221AEMCommandTypeStartStreaming
  , pattern AVB17221AEMCommandTypeStopStreaming
  , pattern AVB17221AEMCommandTypeRegisterUnsolicitedNotification
  , pattern AVB17221AEMCommandTypeDeregisterUnsolicitedNotification
  , pattern AVB17221AEMCommandTypeIdentifyNotification
  , pattern AVB17221AEMCommandTypeGetAVBInfo
  , pattern AVB17221AEMCommandTypeGetASPath
  , pattern AVB17221AEMCommandTypeGetCounters
  , pattern AVB17221AEMCommandTypeReboot
  , pattern AVB17221AEMCommandTypeGetAudioMap
  , pattern AVB17221AEMCommandTypeAddAudioMapping
  , pattern AVB17221AEMCommandTypeRemoveAudioMapping
  , pattern AVB17221AEMCommandTypeGetVideoMap
  , pattern AVB17221AEMCommandTypeAddVideoMapping
  , pattern AVB17221AEMCommandTypeRemoveVideoMapping
  , pattern AVB17221AEMCommandTypeGetSensorMap
  , pattern AVB17221AEMCommandTypeAddSensorMapping
  , pattern AVB17221AEMCommandTypeRemoveSensorMapping
  , pattern AVB17221AEMCommandTypeStartOperation
  , pattern AVB17221AEMCommandTypeAbortOperation
  , pattern AVB17221AEMCommandTypeOperationStatus
  , pattern AVB17221AEMCommandTypeAuthenticationAddKey
  , pattern AVB17221AEMCommandTypeAuthenticationDeleteKey
  , pattern AVB17221AEMCommandTypeAuthenticationGetKeyList
  , pattern AVB17221AEMCommandTypeAuthenticationGetKey
  , pattern AVB17221AEMCommandTypeAuthenticationAddKeyToChain
  , pattern AVB17221AEMCommandTypeAuthenticationDeleteKeyFromChain
  , pattern AVB17221AEMCommandTypeAuthenticationGetKeychainList
  , pattern AVB17221AEMCommandTypeAuthenticationGetIdentity
  , pattern AVB17221AEMCommandTypeAuthenticationAddToken
  , pattern AVB17221AEMCommandTypeAuthenticationDeleteToken
  , pattern AVB17221AEMCommandTypeAuthenticate
  , pattern AVB17221AEMCommandTypeDeauthenticate
  , pattern AVB17221AEMCommandTypeEnableTransportSecurity
  , pattern AVB17221AEMCommandTypeDisableTransportSecurity
  , pattern AVB17221AEMCommandTypeEnableStreamEncryption
  , pattern AVB17221AEMCommandTypeDisableStreamEncryption
  , pattern AVB17221AEMCommandTypeSetMemoryObjectLength
  , pattern AVB17221AEMCommandTypeGetMemoryObjectLength
  , pattern AVB17221AEMCommandTypeSetStreamBackup
  , pattern AVB17221AEMCommandTypeGetStreamBackup
  , pattern AVB17221AEMCommandTypeGetDynamicInfo
  , pattern AVB17221AEMCommandTypeSetMaxTransitTime
  , pattern AVB17221AEMCommandTypeGetMaxTransitTime
  , pattern AVB17221AEMCommandTypeSetSamplingRateRange
  , pattern AVB17221AEMCommandTypeGetSamplingRateRange
  , pattern AVB17221AEMCommandTypeSetPTPInstanceInfo
  , pattern AVB17221AEMCommandTypeGetPTPInstanceInfo
  , pattern AVB17221AEMCommandTypeGetPTPInstanceExtendedInfo
  , pattern AVB17221AEMCommandTypeGetPTPInstanceGrandmasterInfo
  , pattern AVB17221AEMCommandTypeGetPTPInstancePathCount
  , pattern AVB17221AEMCommandTypeGetPTPInstancePathTrace
  , pattern AVB17221AEMCommandTypeGetPTPInstancePerformanceMonitoringCount
  , pattern AVB17221AEMCommandTypeGetPTPInstancePerformanceMonitoringRecord
  , pattern AVB17221AEMCommandTypeSetPTPPortInitialIntervals
  , pattern AVB17221AEMCommandTypeGetPTPPortInitialIntervals
  , pattern AVB17221AEMCommandTypeGetPTPPortCurrentIntervals
  , pattern AVB17221AEMCommandTypeSetPTPPortRemoteIntervals
  , pattern AVB17221AEMCommandTypeGetPTPPortRemoteIntervals
  , pattern AVB17221AEMCommandTypeSetPTPPortInfo
  , pattern AVB17221AEMCommandTypeGetPTPPortInfo
  , pattern AVB17221AEMCommandTypeSetPTPPortOverrides
  , pattern AVB17221AEMCommandTypeGetPTPPortOverrides
  , pattern AVB17221AEMCommandTypeGetPTPPortPDelayMonitoringCount
  , pattern AVB17221AEMCommandTypeGetPTPPortPDelayMonitoringRecord
  , pattern AVB17221AEMCommandTypeGetPTPPortPerformanceMonitoringCount
  , pattern AVB17221AEMCommandTypeGetPTPPortPerformanceMonitoringRecord
  , pattern AVB17221AEMCommandTypeGetPathLatency
  , pattern AVB17221AEMCommandTypeAuthenticationGetNonce
  , pattern AVB17221AEMCommandTypeAuthenticationAddKeyNonce

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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.AudioVideoBridging.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | commandMessage
--
-- This method returns an AVB17221AECPAEMMessage instance setup as an AEM command.
--
-- Returns: An AVB17221AECPAEMMessage instance pre-setup as an AEM command.
--
-- ObjC selector: @+ commandMessage@
commandMessage :: IO (Id AVB17221AECPAEMMessage)
commandMessage  =
  do
    cls' <- getRequiredClass "AVB17221AECPAEMMessage"
    sendClassMsg cls' (mkSelector "commandMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | responseMessage
--
-- This method returns an AVB17221AECPAEMMessage instance setup as an AEM response.
--
-- Returns: An AVB17221AECPAEMMessage instance pre-setup as an AEM response.
--
-- ObjC selector: @+ responseMessage@
responseMessage :: IO (Id AVB17221AECPAEMMessage)
responseMessage  =
  do
    cls' <- getRequiredClass "AVB17221AECPAEMMessage"
    sendClassMsg cls' (mkSelector "responseMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | responseMessageFromCommandMessage
--
-- This method returns an AVB17221AECPAEMMessage instance setup as an AEM response with the appropriate info copied from the supplied command message.
--
-- Returns: An AVB17221AECPAEMMessage instance setup as an AEM response with all info copied from the command message..
--
-- ObjC selector: @+ responseMessageFromCommandMessage:@
responseMessageFromCommandMessage :: IsAVB17221AECPAEMMessage commandMessage => commandMessage -> IO (Id AVB17221AECPAEMMessage)
responseMessageFromCommandMessage commandMessage =
  do
    cls' <- getRequiredClass "AVB17221AECPAEMMessage"
    withObjCPtr commandMessage $ \raw_commandMessage ->
      sendClassMsg cls' (mkSelector "responseMessageFromCommandMessage:") (retPtr retVoid) [argPtr (castPtr raw_commandMessage :: Ptr ())] >>= retainedObject . castPtr

-- | commandType
--
-- The command_type field of the AECP AEM message.
--
-- ObjC selector: @- commandType@
commandType :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> IO AVB17221AEMCommandType
commandType avB17221AECPAEMMessage  =
  fmap (coerce :: CUInt -> AVB17221AEMCommandType) $ sendMsg avB17221AECPAEMMessage (mkSelector "commandType") retCUInt []

-- | commandType
--
-- The command_type field of the AECP AEM message.
--
-- ObjC selector: @- setCommandType:@
setCommandType :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> AVB17221AEMCommandType -> IO ()
setCommandType avB17221AECPAEMMessage  value =
  sendMsg avB17221AECPAEMMessage (mkSelector "setCommandType:") retVoid [argCUInt (coerce value)]

-- | unsolicited
--
-- The u field of the AECP AEM message.
--
-- ObjC selector: @- unsolicited@
unsolicited :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> IO Bool
unsolicited avB17221AECPAEMMessage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPAEMMessage (mkSelector "unsolicited") retCULong []

-- | unsolicited
--
-- The u field of the AECP AEM message.
--
-- ObjC selector: @- setUnsolicited:@
setUnsolicited :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> Bool -> IO ()
setUnsolicited avB17221AECPAEMMessage  value =
  sendMsg avB17221AECPAEMMessage (mkSelector "setUnsolicited:") retVoid [argCULong (if value then 1 else 0)]

-- | controllerRequest
--
-- The cr field of the AECP AEM message.
--
-- ObjC selector: @- controllerRequest@
controllerRequest :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> IO Bool
controllerRequest avB17221AECPAEMMessage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avB17221AECPAEMMessage (mkSelector "controllerRequest") retCULong []

-- | controllerRequest
--
-- The cr field of the AECP AEM message.
--
-- ObjC selector: @- setControllerRequest:@
setControllerRequest :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> Bool -> IO ()
setControllerRequest avB17221AECPAEMMessage  value =
  sendMsg avB17221AECPAEMMessage (mkSelector "setControllerRequest:") retVoid [argCULong (if value then 1 else 0)]

-- | commandSpecificData
--
-- The command_specific_data field of the AECP AEM message.
--
-- ObjC selector: @- commandSpecificData@
commandSpecificData :: IsAVB17221AECPAEMMessage avB17221AECPAEMMessage => avB17221AECPAEMMessage -> IO (Id NSData)
commandSpecificData avB17221AECPAEMMessage  =
  sendMsg avB17221AECPAEMMessage (mkSelector "commandSpecificData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | commandSpecificData
--
-- The command_specific_data field of the AECP AEM message.
--
-- ObjC selector: @- setCommandSpecificData:@
setCommandSpecificData :: (IsAVB17221AECPAEMMessage avB17221AECPAEMMessage, IsNSData value) => avB17221AECPAEMMessage -> value -> IO ()
setCommandSpecificData avB17221AECPAEMMessage  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221AECPAEMMessage (mkSelector "setCommandSpecificData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandMessage@
commandMessageSelector :: Selector
commandMessageSelector = mkSelector "commandMessage"

-- | @Selector@ for @responseMessage@
responseMessageSelector :: Selector
responseMessageSelector = mkSelector "responseMessage"

-- | @Selector@ for @responseMessageFromCommandMessage:@
responseMessageFromCommandMessageSelector :: Selector
responseMessageFromCommandMessageSelector = mkSelector "responseMessageFromCommandMessage:"

-- | @Selector@ for @commandType@
commandTypeSelector :: Selector
commandTypeSelector = mkSelector "commandType"

-- | @Selector@ for @setCommandType:@
setCommandTypeSelector :: Selector
setCommandTypeSelector = mkSelector "setCommandType:"

-- | @Selector@ for @unsolicited@
unsolicitedSelector :: Selector
unsolicitedSelector = mkSelector "unsolicited"

-- | @Selector@ for @setUnsolicited:@
setUnsolicitedSelector :: Selector
setUnsolicitedSelector = mkSelector "setUnsolicited:"

-- | @Selector@ for @controllerRequest@
controllerRequestSelector :: Selector
controllerRequestSelector = mkSelector "controllerRequest"

-- | @Selector@ for @setControllerRequest:@
setControllerRequestSelector :: Selector
setControllerRequestSelector = mkSelector "setControllerRequest:"

-- | @Selector@ for @commandSpecificData@
commandSpecificDataSelector :: Selector
commandSpecificDataSelector = mkSelector "commandSpecificData"

-- | @Selector@ for @setCommandSpecificData:@
setCommandSpecificDataSelector :: Selector
setCommandSpecificDataSelector = mkSelector "setCommandSpecificData:"

