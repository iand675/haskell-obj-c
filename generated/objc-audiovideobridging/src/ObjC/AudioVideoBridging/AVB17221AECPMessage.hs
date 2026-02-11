{-# LANGUAGE PatternSynonyms #-}
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
  , errorForStatusCodeSelector
  , errorForStatusCodeSelector
  , messageTypeSelector
  , setMessageTypeSelector
  , statusSelector
  , setStatusSelector
  , targetEntityIDSelector
  , setTargetEntityIDSelector
  , controllerEntityIDSelector
  , setControllerEntityIDSelector
  , sequenceIDSelector
  , setSequenceIDSelector
  , sourceMACSelector
  , setSourceMACSelector

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
    sendClassMsg cls' (mkSelector "errorForStatusCode:") (retPtr retVoid) [argCUChar (coerce statusCode)] >>= retainedObject . castPtr

-- | errorForStatusCode
--
-- This method returns an NSError filled out with an appropriate description for the message's status code.
--
-- Returns: An NSError instance within the AVBErrorDomain with the status code and an appropriate description.				Will return nil if status code is success or in progress.
--
-- ObjC selector: @- errorForStatusCode@
errorForStatusCode :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO (Id NSError)
errorForStatusCode avB17221AECPMessage  =
  sendMsg avB17221AECPMessage (mkSelector "errorForStatusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | messageType
--
-- The message_type field of the AECP message.
--
-- ObjC selector: @- messageType@
messageType :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO AVB17221AECPMessageType
messageType avB17221AECPMessage  =
  fmap (coerce :: CUChar -> AVB17221AECPMessageType) $ sendMsg avB17221AECPMessage (mkSelector "messageType") retCUChar []

-- | messageType
--
-- The message_type field of the AECP message.
--
-- ObjC selector: @- setMessageType:@
setMessageType :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> AVB17221AECPMessageType -> IO ()
setMessageType avB17221AECPMessage  value =
  sendMsg avB17221AECPMessage (mkSelector "setMessageType:") retVoid [argCUChar (coerce value)]

-- | status
--
-- The status field of the AECP message.
--
-- ObjC selector: @- status@
status :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO AVB17221AECPStatusCode
status avB17221AECPMessage  =
  fmap (coerce :: CUChar -> AVB17221AECPStatusCode) $ sendMsg avB17221AECPMessage (mkSelector "status") retCUChar []

-- | status
--
-- The status field of the AECP message.
--
-- ObjC selector: @- setStatus:@
setStatus :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> AVB17221AECPStatusCode -> IO ()
setStatus avB17221AECPMessage  value =
  sendMsg avB17221AECPMessage (mkSelector "setStatus:") retVoid [argCUChar (coerce value)]

-- | targetEntityID
--
-- The target_entity_id field of the AECP message.
--
-- ObjC selector: @- targetEntityID@
targetEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO CULong
targetEntityID avB17221AECPMessage  =
  sendMsg avB17221AECPMessage (mkSelector "targetEntityID") retCULong []

-- | targetEntityID
--
-- The target_entity_id field of the AECP message.
--
-- ObjC selector: @- setTargetEntityID:@
setTargetEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> CULong -> IO ()
setTargetEntityID avB17221AECPMessage  value =
  sendMsg avB17221AECPMessage (mkSelector "setTargetEntityID:") retVoid [argCULong (fromIntegral value)]

-- | controllerEntityID
--
-- The controller_entity_id field of the AECP message.
--
-- ObjC selector: @- controllerEntityID@
controllerEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO CULong
controllerEntityID avB17221AECPMessage  =
  sendMsg avB17221AECPMessage (mkSelector "controllerEntityID") retCULong []

-- | controllerEntityID
--
-- The controller_entity_id field of the AECP message.
--
-- ObjC selector: @- setControllerEntityID:@
setControllerEntityID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> CULong -> IO ()
setControllerEntityID avB17221AECPMessage  value =
  sendMsg avB17221AECPMessage (mkSelector "setControllerEntityID:") retVoid [argCULong (fromIntegral value)]

-- | sequenceID
--
-- The sequence_id field of the AECP message.
--
-- ObjC selector: @- sequenceID@
sequenceID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO CUShort
sequenceID avB17221AECPMessage  =
  fmap fromIntegral $ sendMsg avB17221AECPMessage (mkSelector "sequenceID") retCUInt []

-- | sequenceID
--
-- The sequence_id field of the AECP message.
--
-- ObjC selector: @- setSequenceID:@
setSequenceID :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> CUShort -> IO ()
setSequenceID avB17221AECPMessage  value =
  sendMsg avB17221AECPMessage (mkSelector "setSequenceID:") retVoid [argCUInt (fromIntegral value)]

-- | sourceMAC
--
-- The source_mac field of the AECP message.
--
-- ObjC selector: @- sourceMAC@
sourceMAC :: IsAVB17221AECPMessage avB17221AECPMessage => avB17221AECPMessage -> IO (Id AVBMACAddress)
sourceMAC avB17221AECPMessage  =
  sendMsg avB17221AECPMessage (mkSelector "sourceMAC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceMAC
--
-- The source_mac field of the AECP message.
--
-- ObjC selector: @- setSourceMAC:@
setSourceMAC :: (IsAVB17221AECPMessage avB17221AECPMessage, IsAVBMACAddress value) => avB17221AECPMessage -> value -> IO ()
setSourceMAC avB17221AECPMessage  value =
withObjCPtr value $ \raw_value ->
    sendMsg avB17221AECPMessage (mkSelector "setSourceMAC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorForStatusCode:@
errorForStatusCodeSelector :: Selector
errorForStatusCodeSelector = mkSelector "errorForStatusCode:"

-- | @Selector@ for @errorForStatusCode@
errorForStatusCodeSelector :: Selector
errorForStatusCodeSelector = mkSelector "errorForStatusCode"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @setMessageType:@
setMessageTypeSelector :: Selector
setMessageTypeSelector = mkSelector "setMessageType:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @targetEntityID@
targetEntityIDSelector :: Selector
targetEntityIDSelector = mkSelector "targetEntityID"

-- | @Selector@ for @setTargetEntityID:@
setTargetEntityIDSelector :: Selector
setTargetEntityIDSelector = mkSelector "setTargetEntityID:"

-- | @Selector@ for @controllerEntityID@
controllerEntityIDSelector :: Selector
controllerEntityIDSelector = mkSelector "controllerEntityID"

-- | @Selector@ for @setControllerEntityID:@
setControllerEntityIDSelector :: Selector
setControllerEntityIDSelector = mkSelector "setControllerEntityID:"

-- | @Selector@ for @sequenceID@
sequenceIDSelector :: Selector
sequenceIDSelector = mkSelector "sequenceID"

-- | @Selector@ for @setSequenceID:@
setSequenceIDSelector :: Selector
setSequenceIDSelector = mkSelector "setSequenceID:"

-- | @Selector@ for @sourceMAC@
sourceMACSelector :: Selector
sourceMACSelector = mkSelector "sourceMAC"

-- | @Selector@ for @setSourceMAC:@
setSourceMACSelector :: Selector
setSourceMACSelector = mkSelector "setSourceMAC:"

