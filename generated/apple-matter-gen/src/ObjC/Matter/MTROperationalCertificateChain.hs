{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of the operational certificate chain for a node.
--
-- Generated bindings for @MTROperationalCertificateChain@.
module ObjC.Matter.MTROperationalCertificateChain
  ( MTROperationalCertificateChain
  , IsMTROperationalCertificateChain(..)
  , init_
  , new
  , initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubject
  , operationalCertificate
  , setOperationalCertificate
  , intermediateCertificate
  , setIntermediateCertificate
  , rootCertificate
  , setRootCertificate
  , adminSubject
  , setAdminSubject
  , adminSubjectSelector
  , initSelector
  , initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector
  , intermediateCertificateSelector
  , newSelector
  , operationalCertificateSelector
  , rootCertificateSelector
  , setAdminSubjectSelector
  , setIntermediateCertificateSelector
  , setOperationalCertificateSelector
  , setRootCertificateSelector


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
init_ :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id MTROperationalCertificateChain)
init_ mtrOperationalCertificateChain =
  sendOwnedMessage mtrOperationalCertificateChain initSelector

-- | @+ new@
new :: IO (Id MTROperationalCertificateChain)
new  =
  do
    cls' <- getRequiredClass "MTROperationalCertificateChain"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:@
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubject :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate, IsNSNumber adminSubject) => mtrOperationalCertificateChain -> operationalCertificate -> intermediateCertificate -> rootCertificate -> adminSubject -> IO (Id MTROperationalCertificateChain)
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubject mtrOperationalCertificateChain operationalCertificate intermediateCertificate rootCertificate adminSubject =
  sendOwnedMessage mtrOperationalCertificateChain initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector (toNSData operationalCertificate) (toNSData intermediateCertificate) (toNSData rootCertificate) (toNSNumber adminSubject)

-- | @- operationalCertificate@
operationalCertificate :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSData)
operationalCertificate mtrOperationalCertificateChain =
  sendMessage mtrOperationalCertificateChain operationalCertificateSelector

-- | @- setOperationalCertificate:@
setOperationalCertificate :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData value) => mtrOperationalCertificateChain -> value -> IO ()
setOperationalCertificate mtrOperationalCertificateChain value =
  sendMessage mtrOperationalCertificateChain setOperationalCertificateSelector (toNSData value)

-- | A nil intermediateCertificate means there is no intermediate.
--
-- ObjC selector: @- intermediateCertificate@
intermediateCertificate :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSData)
intermediateCertificate mtrOperationalCertificateChain =
  sendMessage mtrOperationalCertificateChain intermediateCertificateSelector

-- | A nil intermediateCertificate means there is no intermediate.
--
-- ObjC selector: @- setIntermediateCertificate:@
setIntermediateCertificate :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData value) => mtrOperationalCertificateChain -> value -> IO ()
setIntermediateCertificate mtrOperationalCertificateChain value =
  sendMessage mtrOperationalCertificateChain setIntermediateCertificateSelector (toNSData value)

-- | @- rootCertificate@
rootCertificate :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSData)
rootCertificate mtrOperationalCertificateChain =
  sendMessage mtrOperationalCertificateChain rootCertificateSelector

-- | @- setRootCertificate:@
setRootCertificate :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSData value) => mtrOperationalCertificateChain -> value -> IO ()
setRootCertificate mtrOperationalCertificateChain value =
  sendMessage mtrOperationalCertificateChain setRootCertificateSelector (toNSData value)

-- | adminSubject is passed to the device as part of the AddNOC command.  A nil adminSubject means the node id of the relevant MTRDeviceController will be used.
--
-- ObjC selector: @- adminSubject@
adminSubject :: IsMTROperationalCertificateChain mtrOperationalCertificateChain => mtrOperationalCertificateChain -> IO (Id NSNumber)
adminSubject mtrOperationalCertificateChain =
  sendMessage mtrOperationalCertificateChain adminSubjectSelector

-- | adminSubject is passed to the device as part of the AddNOC command.  A nil adminSubject means the node id of the relevant MTRDeviceController will be used.
--
-- ObjC selector: @- setAdminSubject:@
setAdminSubject :: (IsMTROperationalCertificateChain mtrOperationalCertificateChain, IsNSNumber value) => mtrOperationalCertificateChain -> value -> IO ()
setAdminSubject mtrOperationalCertificateChain value =
  sendMessage mtrOperationalCertificateChain setAdminSubjectSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTROperationalCertificateChain)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTROperationalCertificateChain)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:@
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector :: Selector '[Id NSData, Id NSData, Id NSData, Id NSNumber] (Id MTROperationalCertificateChain)
initWithOperationalCertificate_intermediateCertificate_rootCertificate_adminSubjectSelector = mkSelector "initWithOperationalCertificate:intermediateCertificate:rootCertificate:adminSubject:"

-- | @Selector@ for @operationalCertificate@
operationalCertificateSelector :: Selector '[] (Id NSData)
operationalCertificateSelector = mkSelector "operationalCertificate"

-- | @Selector@ for @setOperationalCertificate:@
setOperationalCertificateSelector :: Selector '[Id NSData] ()
setOperationalCertificateSelector = mkSelector "setOperationalCertificate:"

-- | @Selector@ for @intermediateCertificate@
intermediateCertificateSelector :: Selector '[] (Id NSData)
intermediateCertificateSelector = mkSelector "intermediateCertificate"

-- | @Selector@ for @setIntermediateCertificate:@
setIntermediateCertificateSelector :: Selector '[Id NSData] ()
setIntermediateCertificateSelector = mkSelector "setIntermediateCertificate:"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector '[] (Id NSData)
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @setRootCertificate:@
setRootCertificateSelector :: Selector '[Id NSData] ()
setRootCertificateSelector = mkSelector "setRootCertificate:"

-- | @Selector@ for @adminSubject@
adminSubjectSelector :: Selector '[] (Id NSNumber)
adminSubjectSelector = mkSelector "adminSubject"

-- | @Selector@ for @setAdminSubject:@
setAdminSubjectSelector :: Selector '[Id NSNumber] ()
setAdminSubjectSelector = mkSelector "setAdminSubject:"

