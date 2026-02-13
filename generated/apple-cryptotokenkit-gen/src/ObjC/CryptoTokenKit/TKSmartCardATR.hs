{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents parsed SmartCard ATR.  Provides routine for parsing byte stream or NSData with binary ATR and accessors to parsed ATR parts.
--
-- Generated bindings for @TKSmartCardATR@.
module ObjC.CryptoTokenKit.TKSmartCardATR
  ( TKSmartCardATR
  , IsTKSmartCardATR(..)
  , initWithBytes
  , initWithSource
  , interfaceGroupAtIndex
  , interfaceGroupForProtocol
  , bytes
  , protocols
  , historicalBytes
  , historicalRecords
  , bytesSelector
  , historicalBytesSelector
  , historicalRecordsSelector
  , initWithBytesSelector
  , initWithSourceSelector
  , interfaceGroupAtIndexSelector
  , interfaceGroupForProtocolSelector
  , protocolsSelector

  -- * Enum types
  , TKSmartCardProtocol(TKSmartCardProtocol)
  , pattern TKSmartCardProtocolNone
  , pattern TKSmartCardProtocolT0
  , pattern TKSmartCardProtocolT1
  , pattern TKSmartCardProtocolT15
  , pattern TKSmartCardProtocolAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Parses ATR from binary data block
--
-- @bytes@ — Data containing full valid ATR
--
-- Returns: Parsed ATR instance, or nil when #bytes do not contain valid ATR.
--
-- ObjC selector: @- initWithBytes:@
initWithBytes :: (IsTKSmartCardATR tkSmartCardATR, IsNSData bytes) => tkSmartCardATR -> bytes -> IO (Id TKSmartCardATR)
initWithBytes tkSmartCardATR bytes =
  sendOwnedMessage tkSmartCardATR initWithBytesSelector (toNSData bytes)

-- | Parses ATR from stream.
--
-- @source@ — Provides one byte of ATR from the stream or -1 in case of an error
--
-- Returns: Parsed ATR instance, or nil when #source method failed or an invalid ATR is detected
--
-- ObjC selector: @- initWithSource:@
initWithSource :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> Ptr () -> IO (Id TKSmartCardATR)
initWithSource tkSmartCardATR source =
  sendOwnedMessage tkSmartCardATR initWithSourceSelector source

-- | Retrieves interface group with specified index.
--
-- @index@ — Index of the requested interface group.  Indexing conforms to ISO7816-3, i.e. starts from 1.
--
-- Returns: Interface group with given index, or nil of no such group was present.
--
-- ObjC selector: @- interfaceGroupAtIndex:@
interfaceGroupAtIndex :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> CLong -> IO (Id TKSmartCardATRInterfaceGroup)
interfaceGroupAtIndex tkSmartCardATR index =
  sendMessage tkSmartCardATR interfaceGroupAtIndexSelector index

-- | @protocol@ — Protocol number for which the interface group is requested.
--
-- ObjC selector: @- interfaceGroupForProtocol:@
interfaceGroupForProtocol :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> TKSmartCardProtocol -> IO (Id TKSmartCardATRInterfaceGroup)
interfaceGroupForProtocol tkSmartCardATR protocol =
  sendMessage tkSmartCardATR interfaceGroupForProtocolSelector protocol

-- | Full ATR as string of bytes
--
-- ObjC selector: @- bytes@
bytes :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> IO (Id NSData)
bytes tkSmartCardATR =
  sendMessage tkSmartCardATR bytesSelector

-- | Array of NSNumber of protocols indicated in ATR, in the correct order (i.e. the default protocol comes first), duplicates sorted out.
--
-- ObjC selector: @- protocols@
protocols :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> IO (Id NSArray)
protocols tkSmartCardATR =
  sendMessage tkSmartCardATR protocolsSelector

-- | Just historical bytes of ATR, without Tck and interface bytes.
--
-- ObjC selector: @- historicalBytes@
historicalBytes :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> IO (Id NSData)
historicalBytes tkSmartCardATR =
  sendMessage tkSmartCardATR historicalBytesSelector

-- | An array of TKCompactTLVRecord instances with TLV records parsed from historical bytes.  If historical bytes are not structured using Compact TLV encoding, nil is returned.
--
-- Note: In case that ATR historical bytes begin with 0x00, the last three bytes (status indicator) are automatically       appended into the returned records as if historical bytes would begin with 0x80 and 0x8 record is present       in historical bytes.
--
-- ObjC selector: @- historicalRecords@
historicalRecords :: IsTKSmartCardATR tkSmartCardATR => tkSmartCardATR -> IO (Id NSArray)
historicalRecords tkSmartCardATR =
  sendMessage tkSmartCardATR historicalRecordsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBytes:@
initWithBytesSelector :: Selector '[Id NSData] (Id TKSmartCardATR)
initWithBytesSelector = mkSelector "initWithBytes:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Ptr ()] (Id TKSmartCardATR)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @interfaceGroupAtIndex:@
interfaceGroupAtIndexSelector :: Selector '[CLong] (Id TKSmartCardATRInterfaceGroup)
interfaceGroupAtIndexSelector = mkSelector "interfaceGroupAtIndex:"

-- | @Selector@ for @interfaceGroupForProtocol:@
interfaceGroupForProtocolSelector :: Selector '[TKSmartCardProtocol] (Id TKSmartCardATRInterfaceGroup)
interfaceGroupForProtocolSelector = mkSelector "interfaceGroupForProtocol:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector '[] (Id NSData)
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @protocols@
protocolsSelector :: Selector '[] (Id NSArray)
protocolsSelector = mkSelector "protocols"

-- | @Selector@ for @historicalBytes@
historicalBytesSelector :: Selector '[] (Id NSData)
historicalBytesSelector = mkSelector "historicalBytes"

-- | @Selector@ for @historicalRecords@
historicalRecordsSelector :: Selector '[] (Id NSArray)
historicalRecordsSelector = mkSelector "historicalRecords"

