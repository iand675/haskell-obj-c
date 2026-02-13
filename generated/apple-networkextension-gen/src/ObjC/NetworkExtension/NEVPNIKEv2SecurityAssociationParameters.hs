{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEVPNIKEv2SecurityAssociationParameters
--
-- The NEVPNIKEv2SecurityAssociationParameters class declares the programmatic interface of an object that manages parameters for an IPSec Security Association
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEVPNIKEv2SecurityAssociationParameters@.
module ObjC.NetworkExtension.NEVPNIKEv2SecurityAssociationParameters
  ( NEVPNIKEv2SecurityAssociationParameters
  , IsNEVPNIKEv2SecurityAssociationParameters(..)
  , encryptionAlgorithm
  , setEncryptionAlgorithm
  , integrityAlgorithm
  , setIntegrityAlgorithm
  , diffieHellmanGroup
  , setDiffieHellmanGroup
  , postQuantumKeyExchangeMethods
  , setPostQuantumKeyExchangeMethods
  , lifetimeMinutes
  , setLifetimeMinutes
  , diffieHellmanGroupSelector
  , encryptionAlgorithmSelector
  , integrityAlgorithmSelector
  , lifetimeMinutesSelector
  , postQuantumKeyExchangeMethodsSelector
  , setDiffieHellmanGroupSelector
  , setEncryptionAlgorithmSelector
  , setIntegrityAlgorithmSelector
  , setLifetimeMinutesSelector
  , setPostQuantumKeyExchangeMethodsSelector

  -- * Enum types
  , NEVPNIKEv2DiffieHellmanGroup(NEVPNIKEv2DiffieHellmanGroup)
  , pattern NEVPNIKEv2DiffieHellmanGroupInvalid
  , pattern NEVPNIKEv2DiffieHellmanGroup1
  , pattern NEVPNIKEv2DiffieHellmanGroup2
  , pattern NEVPNIKEv2DiffieHellmanGroup5
  , pattern NEVPNIKEv2DiffieHellmanGroup14
  , pattern NEVPNIKEv2DiffieHellmanGroup15
  , pattern NEVPNIKEv2DiffieHellmanGroup16
  , pattern NEVPNIKEv2DiffieHellmanGroup17
  , pattern NEVPNIKEv2DiffieHellmanGroup18
  , pattern NEVPNIKEv2DiffieHellmanGroup19
  , pattern NEVPNIKEv2DiffieHellmanGroup20
  , pattern NEVPNIKEv2DiffieHellmanGroup21
  , pattern NEVPNIKEv2DiffieHellmanGroup31
  , pattern NEVPNIKEv2DiffieHellmanGroup32
  , NEVPNIKEv2EncryptionAlgorithm(NEVPNIKEv2EncryptionAlgorithm)
  , pattern NEVPNIKEv2EncryptionAlgorithmDES
  , pattern NEVPNIKEv2EncryptionAlgorithm3DES
  , pattern NEVPNIKEv2EncryptionAlgorithmAES128
  , pattern NEVPNIKEv2EncryptionAlgorithmAES256
  , pattern NEVPNIKEv2EncryptionAlgorithmAES128GCM
  , pattern NEVPNIKEv2EncryptionAlgorithmAES256GCM
  , pattern NEVPNIKEv2EncryptionAlgorithmChaCha20Poly1305
  , NEVPNIKEv2IntegrityAlgorithm(NEVPNIKEv2IntegrityAlgorithm)
  , pattern NEVPNIKEv2IntegrityAlgorithmSHA96
  , pattern NEVPNIKEv2IntegrityAlgorithmSHA160
  , pattern NEVPNIKEv2IntegrityAlgorithmSHA256
  , pattern NEVPNIKEv2IntegrityAlgorithmSHA384
  , pattern NEVPNIKEv2IntegrityAlgorithmSHA512

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | encryptionAlgorithm
--
-- The algorithm used by the Security Association to encrypt and decrypt data. On macOS and iOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256 starting in macOS 11 and iOS 14. Prior to that the default was NEVPNIKEv2EncryptionAlgorithm3DES. On tvOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256GCM.
--
-- ObjC selector: @- encryptionAlgorithm@
encryptionAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO NEVPNIKEv2EncryptionAlgorithm
encryptionAlgorithm nevpnikEv2SecurityAssociationParameters =
  sendMessage nevpnikEv2SecurityAssociationParameters encryptionAlgorithmSelector

-- | encryptionAlgorithm
--
-- The algorithm used by the Security Association to encrypt and decrypt data. On macOS and iOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256 starting in macOS 11 and iOS 14. Prior to that the default was NEVPNIKEv2EncryptionAlgorithm3DES. On tvOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256GCM.
--
-- ObjC selector: @- setEncryptionAlgorithm:@
setEncryptionAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> NEVPNIKEv2EncryptionAlgorithm -> IO ()
setEncryptionAlgorithm nevpnikEv2SecurityAssociationParameters value =
  sendMessage nevpnikEv2SecurityAssociationParameters setEncryptionAlgorithmSelector value

-- | integrityAlgorithm
--
-- The algorithm used by the Security Association to verify the integrity of data.  The IKE psedo-random function algorithm will be inferred based on the integrity algorithm. Default is NEVPNIKEv2IntegrityAlgorithmSHA256 starting in macOS 11, iOS 14, and tvOS 17.  Prior to that the default was NEVPNIKEv2IntegrityAlgorithmSHA96.
--
-- ObjC selector: @- integrityAlgorithm@
integrityAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO NEVPNIKEv2IntegrityAlgorithm
integrityAlgorithm nevpnikEv2SecurityAssociationParameters =
  sendMessage nevpnikEv2SecurityAssociationParameters integrityAlgorithmSelector

-- | integrityAlgorithm
--
-- The algorithm used by the Security Association to verify the integrity of data.  The IKE psedo-random function algorithm will be inferred based on the integrity algorithm. Default is NEVPNIKEv2IntegrityAlgorithmSHA256 starting in macOS 11, iOS 14, and tvOS 17.  Prior to that the default was NEVPNIKEv2IntegrityAlgorithmSHA96.
--
-- ObjC selector: @- setIntegrityAlgorithm:@
setIntegrityAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> NEVPNIKEv2IntegrityAlgorithm -> IO ()
setIntegrityAlgorithm nevpnikEv2SecurityAssociationParameters value =
  sendMessage nevpnikEv2SecurityAssociationParameters setIntegrityAlgorithmSelector value

-- | diffieHellmanGroup
--
-- The Diffie Hellman group used by the Security Association. Default is NEVPNIKEv2DiffieHellmanGroup14 starting in macOS 11, iOS 14, and tvOS 17. Prior to that the default was NEVPNIKEv2DiffieHellmanGroup2.
--
-- ObjC selector: @- diffieHellmanGroup@
diffieHellmanGroup :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO NEVPNIKEv2DiffieHellmanGroup
diffieHellmanGroup nevpnikEv2SecurityAssociationParameters =
  sendMessage nevpnikEv2SecurityAssociationParameters diffieHellmanGroupSelector

-- | diffieHellmanGroup
--
-- The Diffie Hellman group used by the Security Association. Default is NEVPNIKEv2DiffieHellmanGroup14 starting in macOS 11, iOS 14, and tvOS 17. Prior to that the default was NEVPNIKEv2DiffieHellmanGroup2.
--
-- ObjC selector: @- setDiffieHellmanGroup:@
setDiffieHellmanGroup :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> NEVPNIKEv2DiffieHellmanGroup -> IO ()
setDiffieHellmanGroup nevpnikEv2SecurityAssociationParameters value =
  sendMessage nevpnikEv2SecurityAssociationParameters setDiffieHellmanGroupSelector value

-- | postQuantumKeyExchangeMethods
--
-- The post-quantum key exchange method(s) used by the Security Association, if any. Values are taken from NEVPNIKEv2PostQuantumKeyExchangeMethod. Up to 7 methods may be specified, mapping to ADDKE1 - ADDKE7 from RFC 9370.
--
-- ObjC selector: @- postQuantumKeyExchangeMethods@
postQuantumKeyExchangeMethods :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO (Id NSArray)
postQuantumKeyExchangeMethods nevpnikEv2SecurityAssociationParameters =
  sendMessage nevpnikEv2SecurityAssociationParameters postQuantumKeyExchangeMethodsSelector

-- | postQuantumKeyExchangeMethods
--
-- The post-quantum key exchange method(s) used by the Security Association, if any. Values are taken from NEVPNIKEv2PostQuantumKeyExchangeMethod. Up to 7 methods may be specified, mapping to ADDKE1 - ADDKE7 from RFC 9370.
--
-- ObjC selector: @- setPostQuantumKeyExchangeMethods:@
setPostQuantumKeyExchangeMethods :: (IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters, IsNSArray value) => nevpnikEv2SecurityAssociationParameters -> value -> IO ()
setPostQuantumKeyExchangeMethods nevpnikEv2SecurityAssociationParameters value =
  sendMessage nevpnikEv2SecurityAssociationParameters setPostQuantumKeyExchangeMethodsSelector (toNSArray value)

-- | lifetimeMinutes
--
-- The life time of the Security Association, in minutes. Default is 60 for IKE Security Associations, and 30 for Child Security Associations. Before the lifetime is reached, IKEv2 will attempt to rekey the Security Association to maintain the connection.
--
-- ObjC selector: @- lifetimeMinutes@
lifetimeMinutes :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO CInt
lifetimeMinutes nevpnikEv2SecurityAssociationParameters =
  sendMessage nevpnikEv2SecurityAssociationParameters lifetimeMinutesSelector

-- | lifetimeMinutes
--
-- The life time of the Security Association, in minutes. Default is 60 for IKE Security Associations, and 30 for Child Security Associations. Before the lifetime is reached, IKEv2 will attempt to rekey the Security Association to maintain the connection.
--
-- ObjC selector: @- setLifetimeMinutes:@
setLifetimeMinutes :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> CInt -> IO ()
setLifetimeMinutes nevpnikEv2SecurityAssociationParameters value =
  sendMessage nevpnikEv2SecurityAssociationParameters setLifetimeMinutesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encryptionAlgorithm@
encryptionAlgorithmSelector :: Selector '[] NEVPNIKEv2EncryptionAlgorithm
encryptionAlgorithmSelector = mkSelector "encryptionAlgorithm"

-- | @Selector@ for @setEncryptionAlgorithm:@
setEncryptionAlgorithmSelector :: Selector '[NEVPNIKEv2EncryptionAlgorithm] ()
setEncryptionAlgorithmSelector = mkSelector "setEncryptionAlgorithm:"

-- | @Selector@ for @integrityAlgorithm@
integrityAlgorithmSelector :: Selector '[] NEVPNIKEv2IntegrityAlgorithm
integrityAlgorithmSelector = mkSelector "integrityAlgorithm"

-- | @Selector@ for @setIntegrityAlgorithm:@
setIntegrityAlgorithmSelector :: Selector '[NEVPNIKEv2IntegrityAlgorithm] ()
setIntegrityAlgorithmSelector = mkSelector "setIntegrityAlgorithm:"

-- | @Selector@ for @diffieHellmanGroup@
diffieHellmanGroupSelector :: Selector '[] NEVPNIKEv2DiffieHellmanGroup
diffieHellmanGroupSelector = mkSelector "diffieHellmanGroup"

-- | @Selector@ for @setDiffieHellmanGroup:@
setDiffieHellmanGroupSelector :: Selector '[NEVPNIKEv2DiffieHellmanGroup] ()
setDiffieHellmanGroupSelector = mkSelector "setDiffieHellmanGroup:"

-- | @Selector@ for @postQuantumKeyExchangeMethods@
postQuantumKeyExchangeMethodsSelector :: Selector '[] (Id NSArray)
postQuantumKeyExchangeMethodsSelector = mkSelector "postQuantumKeyExchangeMethods"

-- | @Selector@ for @setPostQuantumKeyExchangeMethods:@
setPostQuantumKeyExchangeMethodsSelector :: Selector '[Id NSArray] ()
setPostQuantumKeyExchangeMethodsSelector = mkSelector "setPostQuantumKeyExchangeMethods:"

-- | @Selector@ for @lifetimeMinutes@
lifetimeMinutesSelector :: Selector '[] CInt
lifetimeMinutesSelector = mkSelector "lifetimeMinutes"

-- | @Selector@ for @setLifetimeMinutes:@
setLifetimeMinutesSelector :: Selector '[CInt] ()
setLifetimeMinutesSelector = mkSelector "setLifetimeMinutes:"

