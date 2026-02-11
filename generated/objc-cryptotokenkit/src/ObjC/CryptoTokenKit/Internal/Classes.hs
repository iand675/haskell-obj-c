{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CryptoTokenKit.Internal.Classes (
    module ObjC.CryptoTokenKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- TKSmartCard ----------

-- | Represents SmartCard inserted in the slot. Once the card is physically removed from the slot, the session object is invalid and will always fail the operation invoked on it.  In order to communicate with the card, an exclusive session must be established.
-- 
-- Phantom type for @TKSmartCard@.
data TKSmartCard

instance IsObjCObject (Id TKSmartCard) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCard"

class IsNSObject a => IsTKSmartCard a where
  toTKSmartCard :: a -> Id TKSmartCard

instance IsTKSmartCard (Id TKSmartCard) where
  toTKSmartCard = unsafeCastId

instance IsNSObject (Id TKSmartCard) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardATR ----------

-- | Represents parsed SmartCard ATR.  Provides routine for parsing byte stream or NSData with binary ATR and accessors to parsed ATR parts.
-- 
-- Phantom type for @TKSmartCardATR@.
data TKSmartCardATR

instance IsObjCObject (Id TKSmartCardATR) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardATR"

class IsNSObject a => IsTKSmartCardATR a where
  toTKSmartCardATR :: a -> Id TKSmartCardATR

instance IsTKSmartCardATR (Id TKSmartCardATR) where
  toTKSmartCardATR = unsafeCastId

instance IsNSObject (Id TKSmartCardATR) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardATRInterfaceGroup ----------

-- | Represents single interface-bytes group of ATR.
-- 
-- Phantom type for @TKSmartCardATRInterfaceGroup@.
data TKSmartCardATRInterfaceGroup

instance IsObjCObject (Id TKSmartCardATRInterfaceGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardATRInterfaceGroup"

class IsNSObject a => IsTKSmartCardATRInterfaceGroup a where
  toTKSmartCardATRInterfaceGroup :: a -> Id TKSmartCardATRInterfaceGroup

instance IsTKSmartCardATRInterfaceGroup (Id TKSmartCardATRInterfaceGroup) where
  toTKSmartCardATRInterfaceGroup = unsafeCastId

instance IsNSObject (Id TKSmartCardATRInterfaceGroup) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardPINFormat ----------

-- | Specifies PIN formatting properties.
-- 
-- Phantom type for @TKSmartCardPINFormat@.
data TKSmartCardPINFormat

instance IsObjCObject (Id TKSmartCardPINFormat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardPINFormat"

class IsNSObject a => IsTKSmartCardPINFormat a where
  toTKSmartCardPINFormat :: a -> Id TKSmartCardPINFormat

instance IsTKSmartCardPINFormat (Id TKSmartCardPINFormat) where
  toTKSmartCardPINFormat = unsafeCastId

instance IsNSObject (Id TKSmartCardPINFormat) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardSlot ----------

-- | Represents single slot which can contain SmartCard.
-- 
-- Phantom type for @TKSmartCardSlot@.
data TKSmartCardSlot

instance IsObjCObject (Id TKSmartCardSlot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardSlot"

class IsNSObject a => IsTKSmartCardSlot a where
  toTKSmartCardSlot :: a -> Id TKSmartCardSlot

instance IsTKSmartCardSlot (Id TKSmartCardSlot) where
  toTKSmartCardSlot = unsafeCastId

instance IsNSObject (Id TKSmartCardSlot) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardSlotManager ----------

-- | Represents pool of SmartCard reader slots.
-- 
-- Phantom type for @TKSmartCardSlotManager@.
data TKSmartCardSlotManager

instance IsObjCObject (Id TKSmartCardSlotManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardSlotManager"

class IsNSObject a => IsTKSmartCardSlotManager a where
  toTKSmartCardSlotManager :: a -> Id TKSmartCardSlotManager

instance IsTKSmartCardSlotManager (Id TKSmartCardSlotManager) where
  toTKSmartCardSlotManager = unsafeCastId

instance IsNSObject (Id TKSmartCardSlotManager) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardSlotNFCSession ----------

-- | NFC session that's related to NFC smart card slot which was created.
--
-- Lifetime of this session object is tied to the NFC smart card slot lifetime             and once the NFC slot disappears (eg. after a user cancellation, calling end session, or an NFC timeout)             the functions will start to fail and return @TKErrorCodeObjectNotFound@ error.
-- 
-- Phantom type for @TKSmartCardSlotNFCSession@.
data TKSmartCardSlotNFCSession

instance IsObjCObject (Id TKSmartCardSlotNFCSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardSlotNFCSession"

class IsNSObject a => IsTKSmartCardSlotNFCSession a where
  toTKSmartCardSlotNFCSession :: a -> Id TKSmartCardSlotNFCSession

instance IsTKSmartCardSlotNFCSession (Id TKSmartCardSlotNFCSession) where
  toTKSmartCardSlotNFCSession = unsafeCastId

instance IsNSObject (Id TKSmartCardSlotNFCSession) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardTokenRegistrationManager ----------

-- | Provides a centralized management system for registering and unregistering smartcards using their token IDs.
--
-- @Registered smartcard@ keeps its itself accessible via Keychain and system will automatically invoke an NFC slot when a cryptographic operation is required and asks to provide the registered card.
-- 
-- Phantom type for @TKSmartCardTokenRegistrationManager@.
data TKSmartCardTokenRegistrationManager

instance IsObjCObject (Id TKSmartCardTokenRegistrationManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardTokenRegistrationManager"

class IsNSObject a => IsTKSmartCardTokenRegistrationManager a where
  toTKSmartCardTokenRegistrationManager :: a -> Id TKSmartCardTokenRegistrationManager

instance IsTKSmartCardTokenRegistrationManager (Id TKSmartCardTokenRegistrationManager) where
  toTKSmartCardTokenRegistrationManager = unsafeCastId

instance IsNSObject (Id TKSmartCardTokenRegistrationManager) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardUserInteraction ----------

-- | Represents handle to a user interaction involving the SmartCard reader.
--
-- It is a proxy object obtained as a result of invoking the userInteractionFor*** family of methods in TKSmartCardSlot and TKSmartCard.
-- 
-- Phantom type for @TKSmartCardUserInteraction@.
data TKSmartCardUserInteraction

instance IsObjCObject (Id TKSmartCardUserInteraction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardUserInteraction"

class IsNSObject a => IsTKSmartCardUserInteraction a where
  toTKSmartCardUserInteraction :: a -> Id TKSmartCardUserInteraction

instance IsTKSmartCardUserInteraction (Id TKSmartCardUserInteraction) where
  toTKSmartCardUserInteraction = unsafeCastId

instance IsNSObject (Id TKSmartCardUserInteraction) where
  toNSObject = unsafeCastId

-- ---------- TKTLVRecord ----------

-- | Base class representing Tag-Length-Value record. Every record has its tag and binary value represented as NSData instance.  Allows retrieving record's tag, value (as NSData object) and binary representation of the record. Existing subclasses implement assorted encodings - TKBERTLVRecord, TKSimpleTLVRecord and TKCompactTLVRecord.
-- 
-- Phantom type for @TKTLVRecord@.
data TKTLVRecord

instance IsObjCObject (Id TKTLVRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTLVRecord"

class IsNSObject a => IsTKTLVRecord a where
  toTKTLVRecord :: a -> Id TKTLVRecord

instance IsTKTLVRecord (Id TKTLVRecord) where
  toTKTLVRecord = unsafeCastId

instance IsNSObject (Id TKTLVRecord) where
  toNSObject = unsafeCastId

-- ---------- TKToken ----------

-- | Class representing single token.  When implementing SmartCard based token, it is recommended to inherit the implementation from TKSmartCardToken.  Token object serves as synchronization point, all operations invoked upon token and all its sessions are serialized.
-- 
-- Phantom type for @TKToken@.
data TKToken

instance IsObjCObject (Id TKToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKToken"

class IsNSObject a => IsTKToken a where
  toTKToken :: a -> Id TKToken

instance IsTKToken (Id TKToken) where
  toTKToken = unsafeCastId

instance IsNSObject (Id TKToken) where
  toNSObject = unsafeCastId

-- ---------- TKTokenAuthOperation ----------

-- | Context of a pending authentication operation.
-- 
-- Phantom type for @TKTokenAuthOperation@.
data TKTokenAuthOperation

instance IsObjCObject (Id TKTokenAuthOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenAuthOperation"

class IsNSObject a => IsTKTokenAuthOperation a where
  toTKTokenAuthOperation :: a -> Id TKTokenAuthOperation

instance IsTKTokenAuthOperation (Id TKTokenAuthOperation) where
  toTKTokenAuthOperation = unsafeCastId

instance IsNSObject (Id TKTokenAuthOperation) where
  toNSObject = unsafeCastId

-- ---------- TKTokenConfiguration ----------

-- | Holds configuration of one token identified by unique token's instanceID
-- 
-- Phantom type for @TKTokenConfiguration@.
data TKTokenConfiguration

instance IsObjCObject (Id TKTokenConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenConfiguration"

class IsNSObject a => IsTKTokenConfiguration a where
  toTKTokenConfiguration :: a -> Id TKTokenConfiguration

instance IsTKTokenConfiguration (Id TKTokenConfiguration) where
  toTKTokenConfiguration = unsafeCastId

instance IsNSObject (Id TKTokenConfiguration) where
  toNSObject = unsafeCastId

-- ---------- TKTokenDriver ----------

-- | Base class for token drivers.  SmartCard token drivers should use TKSmartCardTokenDriver subclass.
-- 
-- Phantom type for @TKTokenDriver@.
data TKTokenDriver

instance IsObjCObject (Id TKTokenDriver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenDriver"

class IsNSObject a => IsTKTokenDriver a where
  toTKTokenDriver :: a -> Id TKTokenDriver

instance IsTKTokenDriver (Id TKTokenDriver) where
  toTKTokenDriver = unsafeCastId

instance IsNSObject (Id TKTokenDriver) where
  toNSObject = unsafeCastId

-- ---------- TKTokenDriverConfiguration ----------

-- | Holds configuration of one class of tokens
-- 
-- Phantom type for @TKTokenDriverConfiguration@.
data TKTokenDriverConfiguration

instance IsObjCObject (Id TKTokenDriverConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenDriverConfiguration"

class IsNSObject a => IsTKTokenDriverConfiguration a where
  toTKTokenDriverConfiguration :: a -> Id TKTokenDriverConfiguration

instance IsTKTokenDriverConfiguration (Id TKTokenDriverConfiguration) where
  toTKTokenDriverConfiguration = unsafeCastId

instance IsNSObject (Id TKTokenDriverConfiguration) where
  toNSObject = unsafeCastId

-- ---------- TKTokenKeyAlgorithm ----------

-- | TKTokenKeyAlgorithm Encapsulates cryptographic algorithm, possibly with additional associated required algorithms.
--
-- An algorithm supported by a key can be usually described by one value of @SecKeyAlgorithm@ enumeration.  However, some tokens (notably smartcards) require that input data for the operation are in generic format, but that generic format must be formatted according to some more specific algorithm.  An example for this would be token accepting raw data for cryptographic signature but requiring that raw data are formatted according to PKCS1 padding rules.  To express such requirement, TKTokenKeyAlgorithm defines target algorithm (@kSecKeyAlgorithmRSASignatureRaw@ in our example) and a set of other algorithms which were used (continuing example above, @kSecKeyAlgorithmRSASignatureDigestPKCS1v15SHA1@ will be reported as supported).
-- 
-- Phantom type for @TKTokenKeyAlgorithm@.
data TKTokenKeyAlgorithm

instance IsObjCObject (Id TKTokenKeyAlgorithm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenKeyAlgorithm"

class IsNSObject a => IsTKTokenKeyAlgorithm a where
  toTKTokenKeyAlgorithm :: a -> Id TKTokenKeyAlgorithm

instance IsTKTokenKeyAlgorithm (Id TKTokenKeyAlgorithm) where
  toTKTokenKeyAlgorithm = unsafeCastId

instance IsNSObject (Id TKTokenKeyAlgorithm) where
  toNSObject = unsafeCastId

-- ---------- TKTokenKeyExchangeParameters ----------

-- | TKTokenKeyExchangeParameters Encapsulates parameters needed for performing specific Key Exchange operation types.
-- 
-- Phantom type for @TKTokenKeyExchangeParameters@.
data TKTokenKeyExchangeParameters

instance IsObjCObject (Id TKTokenKeyExchangeParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenKeyExchangeParameters"

class IsNSObject a => IsTKTokenKeyExchangeParameters a where
  toTKTokenKeyExchangeParameters :: a -> Id TKTokenKeyExchangeParameters

instance IsTKTokenKeyExchangeParameters (Id TKTokenKeyExchangeParameters) where
  toTKTokenKeyExchangeParameters = unsafeCastId

instance IsNSObject (Id TKTokenKeyExchangeParameters) where
  toNSObject = unsafeCastId

-- ---------- TKTokenKeychainContents ----------

-- | Contains TKTokenKeychainItem instances (keys and certificates) which represent keychain state (i.e. set of items) of specific token.
-- 
-- Phantom type for @TKTokenKeychainContents@.
data TKTokenKeychainContents

instance IsObjCObject (Id TKTokenKeychainContents) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenKeychainContents"

class IsNSObject a => IsTKTokenKeychainContents a where
  toTKTokenKeychainContents :: a -> Id TKTokenKeychainContents

instance IsTKTokenKeychainContents (Id TKTokenKeychainContents) where
  toTKTokenKeychainContents = unsafeCastId

instance IsNSObject (Id TKTokenKeychainContents) where
  toNSObject = unsafeCastId

-- ---------- TKTokenKeychainItem ----------

-- | TKTokenKeychainItem
--
-- Base interface for propagation token's items into the keychain.
-- 
-- Phantom type for @TKTokenKeychainItem@.
data TKTokenKeychainItem

instance IsObjCObject (Id TKTokenKeychainItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenKeychainItem"

class IsNSObject a => IsTKTokenKeychainItem a where
  toTKTokenKeychainItem :: a -> Id TKTokenKeychainItem

instance IsTKTokenKeychainItem (Id TKTokenKeychainItem) where
  toTKTokenKeychainItem = unsafeCastId

instance IsNSObject (Id TKTokenKeychainItem) where
  toNSObject = unsafeCastId

-- ---------- TKTokenSession ----------

-- | TKTokenSession represents token session which shares authentication status.
--
-- Token implementation must inherit its own session implementation from TKTokenSession (or its subclass TKSmartCardTokenSession in case of SmartCard tokens).
--
-- TKTokenSession should keep an authentication state of the token.  Authentication status (e.g. entered PIN to unlock SmartCard) should not be shared across borders of single TKTokenSession instance.
--
-- TKTokenSession is always instantiated by TKToken when framework detects access to the token from new authentication session.
-- 
-- Phantom type for @TKTokenSession@.
data TKTokenSession

instance IsObjCObject (Id TKTokenSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenSession"

class IsNSObject a => IsTKTokenSession a where
  toTKTokenSession :: a -> Id TKTokenSession

instance IsTKTokenSession (Id TKTokenSession) where
  toTKTokenSession = unsafeCastId

instance IsNSObject (Id TKTokenSession) where
  toNSObject = unsafeCastId

-- ---------- TKTokenWatcher ----------

-- | Phantom type for @TKTokenWatcher@.
data TKTokenWatcher

instance IsObjCObject (Id TKTokenWatcher) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenWatcher"

class IsNSObject a => IsTKTokenWatcher a where
  toTKTokenWatcher :: a -> Id TKTokenWatcher

instance IsTKTokenWatcher (Id TKTokenWatcher) where
  toTKTokenWatcher = unsafeCastId

instance IsNSObject (Id TKTokenWatcher) where
  toNSObject = unsafeCastId

-- ---------- TKTokenWatcherTokenInfo ----------

-- | Phantom type for @TKTokenWatcherTokenInfo@.
data TKTokenWatcherTokenInfo

instance IsObjCObject (Id TKTokenWatcherTokenInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenWatcherTokenInfo"

class IsNSObject a => IsTKTokenWatcherTokenInfo a where
  toTKTokenWatcherTokenInfo :: a -> Id TKTokenWatcherTokenInfo

instance IsTKTokenWatcherTokenInfo (Id TKTokenWatcherTokenInfo) where
  toTKTokenWatcherTokenInfo = unsafeCastId

instance IsNSObject (Id TKTokenWatcherTokenInfo) where
  toNSObject = unsafeCastId

-- ---------- TKSmartCardUserInteractionForPINOperation ----------

-- | User interaction for the secure PIN operations on the SmartCard reader.
--
-- Note: Result is available after the interaction has been successfully completed.
-- 
-- Phantom type for @TKSmartCardUserInteractionForPINOperation@.
data TKSmartCardUserInteractionForPINOperation

instance IsObjCObject (Id TKSmartCardUserInteractionForPINOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardUserInteractionForPINOperation"

class IsTKSmartCardUserInteraction a => IsTKSmartCardUserInteractionForPINOperation a where
  toTKSmartCardUserInteractionForPINOperation :: a -> Id TKSmartCardUserInteractionForPINOperation

instance IsTKSmartCardUserInteractionForPINOperation (Id TKSmartCardUserInteractionForPINOperation) where
  toTKSmartCardUserInteractionForPINOperation = unsafeCastId

instance IsNSObject (Id TKSmartCardUserInteractionForPINOperation) where
  toNSObject = unsafeCastId

instance IsTKSmartCardUserInteraction (Id TKSmartCardUserInteractionForPINOperation) where
  toTKSmartCardUserInteraction = unsafeCastId

-- ---------- TKBERTLVRecord ----------

-- | TKBERTLVRecord implements encoding using BER-TLV encoding rules. It is able to parse BER-encoded data and always produces DER-encoded data. No interpretation of tag values is made, all values are treated only as NSData irrespective of the tag.
-- 
-- Phantom type for @TKBERTLVRecord@.
data TKBERTLVRecord

instance IsObjCObject (Id TKBERTLVRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKBERTLVRecord"

class IsTKTLVRecord a => IsTKBERTLVRecord a where
  toTKBERTLVRecord :: a -> Id TKBERTLVRecord

instance IsTKBERTLVRecord (Id TKBERTLVRecord) where
  toTKBERTLVRecord = unsafeCastId

instance IsNSObject (Id TKBERTLVRecord) where
  toNSObject = unsafeCastId

instance IsTKTLVRecord (Id TKBERTLVRecord) where
  toTKTLVRecord = unsafeCastId

-- ---------- TKCompactTLVRecord ----------

-- | TKCompactTLVRecord implements Compact-TLV encoding according to ISO7816-4 Tag is number in range <0..15> encoded as high 4 bits of initial byte, length is number in range <0..15> encoded as low 4 bits of initial byte.  Value immediatelly follows leading byte.
-- 
-- Phantom type for @TKCompactTLVRecord@.
data TKCompactTLVRecord

instance IsObjCObject (Id TKCompactTLVRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKCompactTLVRecord"

class IsTKTLVRecord a => IsTKCompactTLVRecord a where
  toTKCompactTLVRecord :: a -> Id TKCompactTLVRecord

instance IsTKCompactTLVRecord (Id TKCompactTLVRecord) where
  toTKCompactTLVRecord = unsafeCastId

instance IsNSObject (Id TKCompactTLVRecord) where
  toNSObject = unsafeCastId

instance IsTKTLVRecord (Id TKCompactTLVRecord) where
  toTKTLVRecord = unsafeCastId

-- ---------- TKSimpleTLVRecord ----------

-- | TKSimpleTLVRecord implements Simple-TLV encoding according to ISO7816-4. Tag is number in range <1..254> encoded as single byte, length is either single byte specifying length 0-254 or 3 bytes encoded as 0xff followed by 2 bytes of big-endian encoded number.
-- 
-- Phantom type for @TKSimpleTLVRecord@.
data TKSimpleTLVRecord

instance IsObjCObject (Id TKSimpleTLVRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSimpleTLVRecord"

class IsTKTLVRecord a => IsTKSimpleTLVRecord a where
  toTKSimpleTLVRecord :: a -> Id TKSimpleTLVRecord

instance IsTKSimpleTLVRecord (Id TKSimpleTLVRecord) where
  toTKSimpleTLVRecord = unsafeCastId

instance IsNSObject (Id TKSimpleTLVRecord) where
  toNSObject = unsafeCastId

instance IsTKTLVRecord (Id TKSimpleTLVRecord) where
  toTKTLVRecord = unsafeCastId

-- ---------- TKSmartCardToken ----------

-- | TKSmartCardToken base class for implementing SmartCard based token.
--
-- When implementing SmartCard token extension, subclass TKSmartCardToken and implement TKTokenDelegate on it.
-- 
-- Phantom type for @TKSmartCardToken@.
data TKSmartCardToken

instance IsObjCObject (Id TKSmartCardToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardToken"

class IsTKToken a => IsTKSmartCardToken a where
  toTKSmartCardToken :: a -> Id TKSmartCardToken

instance IsTKSmartCardToken (Id TKSmartCardToken) where
  toTKSmartCardToken = unsafeCastId

instance IsNSObject (Id TKSmartCardToken) where
  toNSObject = unsafeCastId

instance IsTKToken (Id TKSmartCardToken) where
  toTKToken = unsafeCastId

-- ---------- TKTokenPasswordAuthOperation ----------

-- | Context of a password authentication operation.
-- 
-- Phantom type for @TKTokenPasswordAuthOperation@.
data TKTokenPasswordAuthOperation

instance IsObjCObject (Id TKTokenPasswordAuthOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenPasswordAuthOperation"

class IsTKTokenAuthOperation a => IsTKTokenPasswordAuthOperation a where
  toTKTokenPasswordAuthOperation :: a -> Id TKTokenPasswordAuthOperation

instance IsTKTokenPasswordAuthOperation (Id TKTokenPasswordAuthOperation) where
  toTKTokenPasswordAuthOperation = unsafeCastId

instance IsNSObject (Id TKTokenPasswordAuthOperation) where
  toNSObject = unsafeCastId

instance IsTKTokenAuthOperation (Id TKTokenPasswordAuthOperation) where
  toTKTokenAuthOperation = unsafeCastId

-- ---------- TKTokenSmartCardPINAuthOperation ----------

-- | Context of a SmartCard PIN authentication operation.
-- 
-- Phantom type for @TKTokenSmartCardPINAuthOperation@.
data TKTokenSmartCardPINAuthOperation

instance IsObjCObject (Id TKTokenSmartCardPINAuthOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenSmartCardPINAuthOperation"

class IsTKTokenAuthOperation a => IsTKTokenSmartCardPINAuthOperation a where
  toTKTokenSmartCardPINAuthOperation :: a -> Id TKTokenSmartCardPINAuthOperation

instance IsTKTokenSmartCardPINAuthOperation (Id TKTokenSmartCardPINAuthOperation) where
  toTKTokenSmartCardPINAuthOperation = unsafeCastId

instance IsNSObject (Id TKTokenSmartCardPINAuthOperation) where
  toNSObject = unsafeCastId

instance IsTKTokenAuthOperation (Id TKTokenSmartCardPINAuthOperation) where
  toTKTokenAuthOperation = unsafeCastId

-- ---------- TKSmartCardTokenDriver ----------

-- | TKSmartCardTokenDriver represents driver for specific SmartCard type.
-- 
-- Phantom type for @TKSmartCardTokenDriver@.
data TKSmartCardTokenDriver

instance IsObjCObject (Id TKSmartCardTokenDriver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardTokenDriver"

class IsTKTokenDriver a => IsTKSmartCardTokenDriver a where
  toTKSmartCardTokenDriver :: a -> Id TKSmartCardTokenDriver

instance IsTKSmartCardTokenDriver (Id TKSmartCardTokenDriver) where
  toTKSmartCardTokenDriver = unsafeCastId

instance IsNSObject (Id TKSmartCardTokenDriver) where
  toNSObject = unsafeCastId

instance IsTKTokenDriver (Id TKSmartCardTokenDriver) where
  toTKTokenDriver = unsafeCastId

-- ---------- TKTokenKeychainCertificate ----------

-- | TKTokenKeychainCertificate
--
-- Interface for propagation token's certificates into the keychain.
-- 
-- Phantom type for @TKTokenKeychainCertificate@.
data TKTokenKeychainCertificate

instance IsObjCObject (Id TKTokenKeychainCertificate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenKeychainCertificate"

class IsTKTokenKeychainItem a => IsTKTokenKeychainCertificate a where
  toTKTokenKeychainCertificate :: a -> Id TKTokenKeychainCertificate

instance IsTKTokenKeychainCertificate (Id TKTokenKeychainCertificate) where
  toTKTokenKeychainCertificate = unsafeCastId

instance IsNSObject (Id TKTokenKeychainCertificate) where
  toNSObject = unsafeCastId

instance IsTKTokenKeychainItem (Id TKTokenKeychainCertificate) where
  toTKTokenKeychainItem = unsafeCastId

-- ---------- TKTokenKeychainKey ----------

-- | TKTokenKeychainKey
--
-- Interface for propagation token's keys into the keychain.
-- 
-- Phantom type for @TKTokenKeychainKey@.
data TKTokenKeychainKey

instance IsObjCObject (Id TKTokenKeychainKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKTokenKeychainKey"

class IsTKTokenKeychainItem a => IsTKTokenKeychainKey a where
  toTKTokenKeychainKey :: a -> Id TKTokenKeychainKey

instance IsTKTokenKeychainKey (Id TKTokenKeychainKey) where
  toTKTokenKeychainKey = unsafeCastId

instance IsNSObject (Id TKTokenKeychainKey) where
  toNSObject = unsafeCastId

instance IsTKTokenKeychainItem (Id TKTokenKeychainKey) where
  toTKTokenKeychainItem = unsafeCastId

-- ---------- TKSmartCardTokenSession ----------

-- | TKSmartCardTokenSession represents token session based on SmartCard token.
--
-- When implementing SmartCard token extension, subclass TKSmartCardTokenSession and implement TKTokenSessionDelegate on it.  Use #token property to get access and send APDUs to the underlying SmartCard.
-- 
-- Phantom type for @TKSmartCardTokenSession@.
data TKSmartCardTokenSession

instance IsObjCObject (Id TKSmartCardTokenSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardTokenSession"

class IsTKTokenSession a => IsTKSmartCardTokenSession a where
  toTKSmartCardTokenSession :: a -> Id TKSmartCardTokenSession

instance IsTKSmartCardTokenSession (Id TKSmartCardTokenSession) where
  toTKSmartCardTokenSession = unsafeCastId

instance IsNSObject (Id TKSmartCardTokenSession) where
  toNSObject = unsafeCastId

instance IsTKTokenSession (Id TKSmartCardTokenSession) where
  toTKTokenSession = unsafeCastId

-- ---------- TKSmartCardUserInteractionForSecurePINChange ----------

-- | User interaction for the secure PIN change on the SmartCard reader.
--
-- Note: Result is available after the interaction has been successfully completed.
-- 
-- Phantom type for @TKSmartCardUserInteractionForSecurePINChange@.
data TKSmartCardUserInteractionForSecurePINChange

instance IsObjCObject (Id TKSmartCardUserInteractionForSecurePINChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardUserInteractionForSecurePINChange"

class IsTKSmartCardUserInteractionForPINOperation a => IsTKSmartCardUserInteractionForSecurePINChange a where
  toTKSmartCardUserInteractionForSecurePINChange :: a -> Id TKSmartCardUserInteractionForSecurePINChange

instance IsTKSmartCardUserInteractionForSecurePINChange (Id TKSmartCardUserInteractionForSecurePINChange) where
  toTKSmartCardUserInteractionForSecurePINChange = unsafeCastId

instance IsNSObject (Id TKSmartCardUserInteractionForSecurePINChange) where
  toNSObject = unsafeCastId

instance IsTKSmartCardUserInteraction (Id TKSmartCardUserInteractionForSecurePINChange) where
  toTKSmartCardUserInteraction = unsafeCastId

instance IsTKSmartCardUserInteractionForPINOperation (Id TKSmartCardUserInteractionForSecurePINChange) where
  toTKSmartCardUserInteractionForPINOperation = unsafeCastId

-- ---------- TKSmartCardUserInteractionForSecurePINVerification ----------

-- | User interaction for the secure PIN verification on the SmartCard reader.
--
-- Note: Result is available after the interaction has been successfully completed.
-- 
-- Phantom type for @TKSmartCardUserInteractionForSecurePINVerification@.
data TKSmartCardUserInteractionForSecurePINVerification

instance IsObjCObject (Id TKSmartCardUserInteractionForSecurePINVerification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "TKSmartCardUserInteractionForSecurePINVerification"

class IsTKSmartCardUserInteractionForPINOperation a => IsTKSmartCardUserInteractionForSecurePINVerification a where
  toTKSmartCardUserInteractionForSecurePINVerification :: a -> Id TKSmartCardUserInteractionForSecurePINVerification

instance IsTKSmartCardUserInteractionForSecurePINVerification (Id TKSmartCardUserInteractionForSecurePINVerification) where
  toTKSmartCardUserInteractionForSecurePINVerification = unsafeCastId

instance IsNSObject (Id TKSmartCardUserInteractionForSecurePINVerification) where
  toNSObject = unsafeCastId

instance IsTKSmartCardUserInteraction (Id TKSmartCardUserInteractionForSecurePINVerification) where
  toTKSmartCardUserInteraction = unsafeCastId

instance IsTKSmartCardUserInteractionForPINOperation (Id TKSmartCardUserInteractionForSecurePINVerification) where
  toTKSmartCardUserInteractionForPINOperation = unsafeCastId
