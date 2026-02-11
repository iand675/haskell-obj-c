{-# LANGUAGE PatternSynonyms #-}
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
  , encryptionAlgorithmSelector
  , setEncryptionAlgorithmSelector
  , integrityAlgorithmSelector
  , setIntegrityAlgorithmSelector
  , diffieHellmanGroupSelector
  , setDiffieHellmanGroupSelector
  , postQuantumKeyExchangeMethodsSelector
  , setPostQuantumKeyExchangeMethodsSelector
  , lifetimeMinutesSelector
  , setLifetimeMinutesSelector

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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | encryptionAlgorithm
--
-- The algorithm used by the Security Association to encrypt and decrypt data. On macOS and iOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256 starting in macOS 11 and iOS 14. Prior to that the default was NEVPNIKEv2EncryptionAlgorithm3DES. On tvOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256GCM.
--
-- ObjC selector: @- encryptionAlgorithm@
encryptionAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO NEVPNIKEv2EncryptionAlgorithm
encryptionAlgorithm nevpnikEv2SecurityAssociationParameters  =
    fmap (coerce :: CLong -> NEVPNIKEv2EncryptionAlgorithm) $ sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "encryptionAlgorithm") retCLong []

-- | encryptionAlgorithm
--
-- The algorithm used by the Security Association to encrypt and decrypt data. On macOS and iOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256 starting in macOS 11 and iOS 14. Prior to that the default was NEVPNIKEv2EncryptionAlgorithm3DES. On tvOS, the default is NEVPNIKEv2EncryptionAlgorithmAES256GCM.
--
-- ObjC selector: @- setEncryptionAlgorithm:@
setEncryptionAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> NEVPNIKEv2EncryptionAlgorithm -> IO ()
setEncryptionAlgorithm nevpnikEv2SecurityAssociationParameters  value =
    sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "setEncryptionAlgorithm:") retVoid [argCLong (coerce value)]

-- | integrityAlgorithm
--
-- The algorithm used by the Security Association to verify the integrity of data.  The IKE psedo-random function algorithm will be inferred based on the integrity algorithm. Default is NEVPNIKEv2IntegrityAlgorithmSHA256 starting in macOS 11, iOS 14, and tvOS 17.  Prior to that the default was NEVPNIKEv2IntegrityAlgorithmSHA96.
--
-- ObjC selector: @- integrityAlgorithm@
integrityAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO NEVPNIKEv2IntegrityAlgorithm
integrityAlgorithm nevpnikEv2SecurityAssociationParameters  =
    fmap (coerce :: CLong -> NEVPNIKEv2IntegrityAlgorithm) $ sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "integrityAlgorithm") retCLong []

-- | integrityAlgorithm
--
-- The algorithm used by the Security Association to verify the integrity of data.  The IKE psedo-random function algorithm will be inferred based on the integrity algorithm. Default is NEVPNIKEv2IntegrityAlgorithmSHA256 starting in macOS 11, iOS 14, and tvOS 17.  Prior to that the default was NEVPNIKEv2IntegrityAlgorithmSHA96.
--
-- ObjC selector: @- setIntegrityAlgorithm:@
setIntegrityAlgorithm :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> NEVPNIKEv2IntegrityAlgorithm -> IO ()
setIntegrityAlgorithm nevpnikEv2SecurityAssociationParameters  value =
    sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "setIntegrityAlgorithm:") retVoid [argCLong (coerce value)]

-- | diffieHellmanGroup
--
-- The Diffie Hellman group used by the Security Association. Default is NEVPNIKEv2DiffieHellmanGroup14 starting in macOS 11, iOS 14, and tvOS 17. Prior to that the default was NEVPNIKEv2DiffieHellmanGroup2.
--
-- ObjC selector: @- diffieHellmanGroup@
diffieHellmanGroup :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO NEVPNIKEv2DiffieHellmanGroup
diffieHellmanGroup nevpnikEv2SecurityAssociationParameters  =
    fmap (coerce :: CLong -> NEVPNIKEv2DiffieHellmanGroup) $ sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "diffieHellmanGroup") retCLong []

-- | diffieHellmanGroup
--
-- The Diffie Hellman group used by the Security Association. Default is NEVPNIKEv2DiffieHellmanGroup14 starting in macOS 11, iOS 14, and tvOS 17. Prior to that the default was NEVPNIKEv2DiffieHellmanGroup2.
--
-- ObjC selector: @- setDiffieHellmanGroup:@
setDiffieHellmanGroup :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> NEVPNIKEv2DiffieHellmanGroup -> IO ()
setDiffieHellmanGroup nevpnikEv2SecurityAssociationParameters  value =
    sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "setDiffieHellmanGroup:") retVoid [argCLong (coerce value)]

-- | postQuantumKeyExchangeMethods
--
-- The post-quantum key exchange method(s) used by the Security Association, if any. Values are taken from NEVPNIKEv2PostQuantumKeyExchangeMethod. Up to 7 methods may be specified, mapping to ADDKE1 - ADDKE7 from RFC 9370.
--
-- ObjC selector: @- postQuantumKeyExchangeMethods@
postQuantumKeyExchangeMethods :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO (Id NSArray)
postQuantumKeyExchangeMethods nevpnikEv2SecurityAssociationParameters  =
    sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "postQuantumKeyExchangeMethods") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | postQuantumKeyExchangeMethods
--
-- The post-quantum key exchange method(s) used by the Security Association, if any. Values are taken from NEVPNIKEv2PostQuantumKeyExchangeMethod. Up to 7 methods may be specified, mapping to ADDKE1 - ADDKE7 from RFC 9370.
--
-- ObjC selector: @- setPostQuantumKeyExchangeMethods:@
setPostQuantumKeyExchangeMethods :: (IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters, IsNSArray value) => nevpnikEv2SecurityAssociationParameters -> value -> IO ()
setPostQuantumKeyExchangeMethods nevpnikEv2SecurityAssociationParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "setPostQuantumKeyExchangeMethods:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | lifetimeMinutes
--
-- The life time of the Security Association, in minutes. Default is 60 for IKE Security Associations, and 30 for Child Security Associations. Before the lifetime is reached, IKEv2 will attempt to rekey the Security Association to maintain the connection.
--
-- ObjC selector: @- lifetimeMinutes@
lifetimeMinutes :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> IO CInt
lifetimeMinutes nevpnikEv2SecurityAssociationParameters  =
    sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "lifetimeMinutes") retCInt []

-- | lifetimeMinutes
--
-- The life time of the Security Association, in minutes. Default is 60 for IKE Security Associations, and 30 for Child Security Associations. Before the lifetime is reached, IKEv2 will attempt to rekey the Security Association to maintain the connection.
--
-- ObjC selector: @- setLifetimeMinutes:@
setLifetimeMinutes :: IsNEVPNIKEv2SecurityAssociationParameters nevpnikEv2SecurityAssociationParameters => nevpnikEv2SecurityAssociationParameters -> CInt -> IO ()
setLifetimeMinutes nevpnikEv2SecurityAssociationParameters  value =
    sendMsg nevpnikEv2SecurityAssociationParameters (mkSelector "setLifetimeMinutes:") retVoid [argCInt value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encryptionAlgorithm@
encryptionAlgorithmSelector :: Selector
encryptionAlgorithmSelector = mkSelector "encryptionAlgorithm"

-- | @Selector@ for @setEncryptionAlgorithm:@
setEncryptionAlgorithmSelector :: Selector
setEncryptionAlgorithmSelector = mkSelector "setEncryptionAlgorithm:"

-- | @Selector@ for @integrityAlgorithm@
integrityAlgorithmSelector :: Selector
integrityAlgorithmSelector = mkSelector "integrityAlgorithm"

-- | @Selector@ for @setIntegrityAlgorithm:@
setIntegrityAlgorithmSelector :: Selector
setIntegrityAlgorithmSelector = mkSelector "setIntegrityAlgorithm:"

-- | @Selector@ for @diffieHellmanGroup@
diffieHellmanGroupSelector :: Selector
diffieHellmanGroupSelector = mkSelector "diffieHellmanGroup"

-- | @Selector@ for @setDiffieHellmanGroup:@
setDiffieHellmanGroupSelector :: Selector
setDiffieHellmanGroupSelector = mkSelector "setDiffieHellmanGroup:"

-- | @Selector@ for @postQuantumKeyExchangeMethods@
postQuantumKeyExchangeMethodsSelector :: Selector
postQuantumKeyExchangeMethodsSelector = mkSelector "postQuantumKeyExchangeMethods"

-- | @Selector@ for @setPostQuantumKeyExchangeMethods:@
setPostQuantumKeyExchangeMethodsSelector :: Selector
setPostQuantumKeyExchangeMethodsSelector = mkSelector "setPostQuantumKeyExchangeMethods:"

-- | @Selector@ for @lifetimeMinutes@
lifetimeMinutesSelector :: Selector
lifetimeMinutesSelector = mkSelector "lifetimeMinutes"

-- | @Selector@ for @setLifetimeMinutes:@
setLifetimeMinutesSelector :: Selector
setLifetimeMinutesSelector = mkSelector "setLifetimeMinutes:"

