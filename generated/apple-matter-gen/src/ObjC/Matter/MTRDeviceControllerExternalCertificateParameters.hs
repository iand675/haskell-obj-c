{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceControllerExternalCertificateParameters@.
module ObjC.Matter.MTRDeviceControllerExternalCertificateParameters
  ( MTRDeviceControllerExternalCertificateParameters
  , IsMTRDeviceControllerExternalCertificateParameters(..)
  , init_
  , new
  , initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate
  , rootCertificate
  , initSelector
  , initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector
  , newSelector
  , rootCertificateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceControllerExternalCertificateParameters mtrDeviceControllerExternalCertificateParameters => mtrDeviceControllerExternalCertificateParameters -> IO (Id MTRDeviceControllerExternalCertificateParameters)
init_ mtrDeviceControllerExternalCertificateParameters =
  sendOwnedMessage mtrDeviceControllerExternalCertificateParameters initSelector

-- | @+ new@
new :: IO (Id MTRDeviceControllerExternalCertificateParameters)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerExternalCertificateParameters"
    sendOwnedClassMessage cls' newSelector

-- | Prepare to initialize a controller that is not able to sign operational certificates itself, and therefore needs to be provided with a complete operational certificate chain.
--
-- A controller created from MTRDeviceControllerStartupParams initialized with this method will not be able to commission devices unless operationalCertificateIssuer and operationalCertificateIssuerQueue are set.
--
-- The fabric id and node id to use for the controller will be derived from the provided operationalCertificate.
--
-- @storageDelegate@ — The storage to use for the controller.  This will be                        called into on storageDelegateQueue.
--
-- @storageDelegateQueue@ — The queue for calls into storageDelegate.  See                             MTRDeviceControllerStorageDelegate documentation                             for the rules about what work is allowed to be                             done on this queue.
--
-- @uniqueIdentifier@ — The unique id to assign to the controller.
--
-- @vendorID@ — The vendor ID (allocated by the Connectivity Standards Alliance) for                 this controller. Must not be the "standard" vendor id (0).
--
-- @ipk@ — The Identity Protection Key. Must be 16 bytes in length.
--
-- @intermediateCertificate@ — Must be nil if operationalCertificate is                                directly signed by rootCertificate.  Otherwise                                must be the certificate that signed                                operationalCertificate.
--
-- ObjC selector: @- initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate :: (IsMTRDeviceControllerExternalCertificateParameters mtrDeviceControllerExternalCertificateParameters, IsNSObject storageDelegateQueue, IsNSUUID uniqueIdentifier, IsNSData ipk, IsNSNumber vendorID, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate) => mtrDeviceControllerExternalCertificateParameters -> RawId -> storageDelegateQueue -> uniqueIdentifier -> ipk -> vendorID -> RawId -> operationalCertificate -> intermediateCertificate -> rootCertificate -> IO (Id MTRDeviceControllerExternalCertificateParameters)
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate mtrDeviceControllerExternalCertificateParameters storageDelegate storageDelegateQueue uniqueIdentifier ipk vendorID operationalKeypair operationalCertificate intermediateCertificate rootCertificate =
  sendOwnedMessage mtrDeviceControllerExternalCertificateParameters initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector storageDelegate (toNSObject storageDelegateQueue) (toNSUUID uniqueIdentifier) (toNSData ipk) (toNSNumber vendorID) operationalKeypair (toNSData operationalCertificate) (toNSData intermediateCertificate) (toNSData rootCertificate)

-- | The root certificate we were initialized with.
--
-- ObjC selector: @- rootCertificate@
rootCertificate :: IsMTRDeviceControllerExternalCertificateParameters mtrDeviceControllerExternalCertificateParameters => mtrDeviceControllerExternalCertificateParameters -> IO (Id NSData)
rootCertificate mtrDeviceControllerExternalCertificateParameters =
  sendMessage mtrDeviceControllerExternalCertificateParameters rootCertificateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceControllerExternalCertificateParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceControllerExternalCertificateParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector :: Selector '[RawId, Id NSObject, Id NSUUID, Id NSData, Id NSNumber, RawId, Id NSData, Id NSData, Id NSData] (Id MTRDeviceControllerExternalCertificateParameters)
initWithStorageDelegate_storageDelegateQueue_uniqueIdentifier_ipk_vendorID_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector = mkSelector "initWithStorageDelegate:storageDelegateQueue:uniqueIdentifier:ipk:vendorID:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector '[] (Id NSData)
rootCertificateSelector = mkSelector "rootCertificate"

