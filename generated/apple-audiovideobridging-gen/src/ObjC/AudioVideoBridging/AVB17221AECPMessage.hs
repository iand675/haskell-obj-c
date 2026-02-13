{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221AECPMessage
--
-- AVB17221AECPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol message.
--
-- AVB17221AECPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP) message.				This class is a abstract class providing the support for the common format shared between the different				AECP message types.
--
-- Generated bindings for @AVB17221AECPMessage@.
module ObjC.AudioVideoBridging.AVB17221AECPMessage
  ( AVB17221AECPMessage
  , IsAVB17221AECPMessage(..)
  , avB17221AECPMessageErrorForStatusCode
  , errorForStatusCode
  , messageType
  , setMessageType
  , status
  , setStatus
  , targetEntityID
  , setTargetEntityID
  , controllerEntityID
  , setControllerEntityID
  , sequenceID
  , setSequenceID
  , sourceMAC
  , setSourceMAC
  , avB17221AECPMessageErrorForStatusCodeSelector
  , controllerEntityIDSelector
  , errorForStatusCodeSelector
  , messageTypeSelector
  , sequenceIDSelector
  , setControllerEntityIDSelector
  , setMessageTypeSelector
  , setSequenceIDSelector
  , setSourceMACSelector
  , setStatusSelector
  , setTargetEntityIDSelector
  , sourceMACSelector
  , statusSelector
  , targetEntityIDSelector

  -- * Enum types
  , AVB17221AECPMessageType(AVB17221AECPMessageType)
  , pattern AVB17221AECPMessageTypeAEMCommand
  , pattern AVB17221AECPMessageTypeAEMResponse
  , pattern AVB17221AECPMessageTypeAddressAccessCommand
  , pattern AVB17221AECPMessageTypeAddressAccessResponse
  , pattern AVB17221AECPMessageTypeLegacyAVCCommand
  , pattern AVB17221AECPMessageTypeLegacyAVCResponse
  , pattern AVB17221AECPMessageTypeVendorUniqueCommand
  , pattern AVB17221AECPMessageTypeVendorUniqueResponse
  , AVB17221AECPStatusCode(AVB17221AECPStatusCode)
  , pattern AVB17221AECPStatusSuccess
  , pattern AVB17221AECPStatusNotImplemented
  , pattern AVB17221AECPStatusNoSuchDescriptor
  , pattern AVB17221AECPStatusEntityLocked
  , pattern AVB17221AECPStatusEntityAcquired
  , pattern AVB17221AECPStatusNotAuthorized
  , pattern AVB17221AECPStatusInsufficientPrivileges
  , pattern AVB17221AECPStatusBadArguments
  , pattern AVB17221AECPStatusNoResources
  , pattern AVB17221AECPStatusInProgress
  , pattern AVB17221AECPStatusEntityMisbehaving
  , pattern AVB17221AECPStatusNotSupported
  , pattern AVB17221AECPStatusStreamIsRunning
  , pattern AVB17221AECPStatusAddressAccessAddressTooLow
  , pattern AVB17221AECPStatusAddressAccessAddressTooHigh
  , pattern AVB17221AECPStatusAddressAccessAddressInvalid
  , pattern AVB17221AECPStatusAddressAccessTLVInvalid
  , pattern AVB17221AECPStatusAddressAccessDataInvalid
  , pattern AVB17221AECPStatusAddressAccessUnsupported
  , pattern AVB17221AECPStatusAVCFailure

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.AudioVideoBridging.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | errorForStatusCode:
--
-- This method returns an NSError filled out with an appropriate description for the passed in status code.
--
-- Returns: An NSError instance within the AVBErrorDomain with the status code and an appropriate description.				Will return nil if status code is success or in progress.
--
-- ObjC selector: @+ errorForStatusCode:@
avB17221AECPMessageErrorForStatusCode :: AVB17221AECPStatusCode -> IO (Id NSError)
avB17221AECPMessageErrorForStatusCode statusCode =
  do
    cls' <- getRequiredClass "AVB17221AECPMessage"
    sendClassMessage cls' avB17221AECPMessageErrorForStatusCodeSelector statusCode

-- | errorForStatusCode
--
-- This method returns an NSError filled out with an appropriate description for the message's status code.
--
-- Returns: An NSError instance within the AVBErrorDomain with the status code and an appropriate description.				Will return nil if status code is success or in progress.
--
-- ObjC selector: @- errorForStatusCode@
errorForStatusCode :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO (Id NSError)
errorForStatusCode avB17221AECPMessage =
  sendMessage avB17221AECPMessage errorForStatusCodeSelector

-- | messageType
--
-- The message_type field of the AECP message.
--
-- ObjC selector: @- messageType@
messageType :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO AVB17221AECPMessageType
messageType avB17221AECPMessage =
  sendMessage avB17221AECPMessage messageTypeSelector

-- | messageType
--
-- The message_type field of the AECP message.
--
-- ObjC selector: @- setMessageType:@
setMessageType :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> AVB17221AECPMessageType -> IO ()
setMessageType avB17221AECPMessage value =
  sendMessage avB17221AECPMessage setMessageTypeSelector value

-- | status
--
-- The status field of the AECP message.
--
-- ObjC selector: @- status@
status :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO AVB17221AECPStatusCode
status avB17221AECPMessage =
  sendMessage avB17221AECPMessage statusSelector

-- | status
--
-- The status field of the AECP message.
--
-- ObjC selector: @- setStatus:@
setStatus :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> AVB17221AECPStatusCode -> IO ()
setStatus avB17221AECPMessage value =
  sendMessage avB17221AECPMessage setStatusSelector value

-- | targetEntityID
--
-- The target_entity_id field of the AECP message.
--
-- ObjC selector: @- targetEntityID@
targetEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO CULong
targetEntityID avB17221AECPMessage =
  sendMessage avB17221AECPMessage targetEntityIDSelector

-- | targetEntityID
--
-- The target_entity_id field of the AECP message.
--
-- ObjC selector: @- setTargetEntityID:@
setTargetEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> CULong -> IO ()
setTargetEntityID avB17221AECPMessage value =
  sendMessage avB17221AECPMessage setTargetEntityIDSelector value

-- | controllerEntityID
--
-- The controller_entity_id field of the AECP message.
--
-- ObjC selector: @- controllerEntityID@
controllerEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO CULong
controllerEntityID avB17221AECPMessage =
  sendMessage avB17221AECPMessage controllerEntityIDSelector

-- | controllerEntityID
--
-- The controller_entity_id field of the AECP message.
--
-- ObjC selector: @- setControllerEntityID:@
setControllerEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> CULong -> IO ()
setControllerEntityID avB17221AECPMessage value =
  sendMessage avB17221AECPMessage setControllerEntityIDSelector value

-- | sequenceID
--
-- The sequence_id field of the AECP message.
--
-- ObjC selector: @- sequenceID@
sequenceID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO CUShort
sequenceID avB17221AECPMessage =
  sendMessage avB17221AECPMessage sequenceIDSelector

-- | sequenceID
--
-- The sequence_id field of the AECP message.
--
-- ObjC selector: @- setSequenceID:@
setSequenceID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> CUShort -> IO ()
setSequenceID avB17221AECPMessage value =
  sendMessage avB17221AECPMessage setSequenceIDSelector value

-- | sourceMAC
--
-- The source_mac field of the AECP message.
--
-- ObjC selector: @- sourceMAC@
sourceMAC :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO (Id AVBMACAddress)
sourceMAC avB17221AECPMessage =
  sendMessage avB17221AECPMessage sourceMACSelector

-- | sourceMAC
--
-- The source_mac field of the AECP message.
--
-- ObjC selector: @- setSourceMAC:@
setSourceMAC :: (IsAVB17221AECPMessage avB17221AECPMessage, IsAVBMACAddress value) => avB17221AECPMessage -> value -> IO ()
setSourceMAC avB17221AECPMessage value =
  sendMessage avB17221AECPMessage setSourceMACSelector (toAVBMACAddress value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorForStatusCode:@
avB17221AECPMessageErrorForStatusCodeSelector :: Selector '[AVB17221AECPStatusCode] (Id NSError)
avB17221AECPMessageErrorForStatusCodeSelector = mkSelector "errorForStatusCode:"

-- | @Selector@ for @errorForStatusCode@
errorForStatusCodeSelector :: Selector '[] (Id NSError)
errorForStatusCodeSelector = mkSelector "errorForStatusCode"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector '[] AVB17221AECPMessageType
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @setMessageType:@
setMessageTypeSelector :: Selector '[AVB17221AECPMessageType] ()
setMessageTypeSelector = mkSelector "setMessageType:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVB17221AECPStatusCode
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[AVB17221AECPStatusCode] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @targetEntityID@
targetEntityIDSelector :: Selector '[] CULong
targetEntityIDSelector = mkSelector "targetEntityID"

-- | @Selector@ for @setTargetEntityID:@
setTargetEntityIDSelector :: Selector '[CULong] ()
setTargetEntityIDSelector = mkSelector "setTargetEntityID:"

-- | @Selector@ for @controllerEntityID@
controllerEntityIDSelector :: Selector '[] CULong
controllerEntityIDSelector = mkSelector "controllerEntityID"

-- | @Selector@ for @setControllerEntityID:@
setControllerEntityIDSelector :: Selector '[CULong] ()
setControllerEntityIDSelector = mkSelector "setControllerEntityID:"

-- | @Selector@ for @sequenceID@
sequenceIDSelector :: Selector '[] CUShort
sequenceIDSelector = mkSelector "sequenceID"

-- | @Selector@ for @setSequenceID:@
setSequenceIDSelector :: Selector '[CUShort] ()
setSequenceIDSelector = mkSelector "setSequenceID:"

-- | @Selector@ for @sourceMAC@
sourceMACSelector :: Selector '[] (Id AVBMACAddress)
sourceMACSelector = mkSelector "sourceMAC"

-- | @Selector@ for @setSourceMAC:@
setSourceMACSelector :: Selector '[Id AVBMACAddress] ()
setSourceMACSelector = mkSelector "setSourceMAC:"

