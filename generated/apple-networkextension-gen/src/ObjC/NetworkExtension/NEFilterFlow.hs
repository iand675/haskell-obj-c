{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterFlow
--
-- The NEFilterFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered.
--
-- NEFilterFlow is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterFlow@.
module ObjC.NetworkExtension.NEFilterFlow
  ( NEFilterFlow
  , IsNEFilterFlow(..)
  , url
  , sourceAppUniqueIdentifier
  , sourceAppIdentifier
  , sourceAppVersion
  , direction
  , sourceAppAuditToken
  , sourceProcessAuditToken
  , identifier
  , urlSelector
  , sourceAppUniqueIdentifierSelector
  , sourceAppIdentifierSelector
  , sourceAppVersionSelector
  , directionSelector
  , sourceAppAuditTokenSelector
  , sourceProcessAuditTokenSelector
  , identifierSelector

  -- * Enum types
  , NETrafficDirection(NETrafficDirection)
  , pattern NETrafficDirectionAny
  , pattern NETrafficDirectionInbound
  , pattern NETrafficDirectionOutbound

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

-- | URL
--
-- The flow's HTTP request URL. Will be nil if the flow did not originate from WebKit.
--
-- ObjC selector: @- URL@
url :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSURL)
url neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAppUniqueIdentifier
--
-- A byte string that uniquely identifies the binary for each build of the source application of the flow.
--
-- ObjC selector: @- sourceAppUniqueIdentifier@
sourceAppUniqueIdentifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSData)
sourceAppUniqueIdentifier neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "sourceAppUniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAppIdentifier
--
-- A string containing the identifier of the source application of the flow. This identifier stays the same for all versions and builds of the application. This identifier is unique among all applications.
--
-- ObjC selector: @- sourceAppIdentifier@
sourceAppIdentifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSString)
sourceAppIdentifier neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "sourceAppIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAppVersion
--
-- The short version string of the source application. Will be nil if the app info is unavailable.
--
-- ObjC selector: @- sourceAppVersion@
sourceAppVersion :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSString)
sourceAppVersion neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "sourceAppVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | direction
--
-- Initial direction of the flow (outgoing or incoming flow)
--
-- ObjC selector: @- direction@
direction :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO NETrafficDirection
direction neFilterFlow  =
    fmap (coerce :: CLong -> NETrafficDirection) $ sendMsg neFilterFlow (mkSelector "direction") retCLong []

-- | sourceAppAuditToken
--
-- Audit token of the source application of the flow.
--
-- ObjC selector: @- sourceAppAuditToken@
sourceAppAuditToken :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSData)
sourceAppAuditToken neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "sourceAppAuditToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceProcessAuditToken
--
-- The audit token of the process that created the flow. In cases where the connection was created by a system process on behalf of the source application, sourceProcessAuditToken will be different from sourceAppAuditToken and will contain the audit token of the system process. In cases where the source application directly created the connection sourceAppAuditToken and sourceProcessAuditToken will be identical.
--
-- ObjC selector: @- sourceProcessAuditToken@
sourceProcessAuditToken :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSData)
sourceProcessAuditToken neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "sourceProcessAuditToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identifier
--
-- The unique identifier of the flow.
--
-- ObjC selector: @- identifier@
identifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSUUID)
identifier neFilterFlow  =
    sendMsg neFilterFlow (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @sourceAppUniqueIdentifier@
sourceAppUniqueIdentifierSelector :: Selector
sourceAppUniqueIdentifierSelector = mkSelector "sourceAppUniqueIdentifier"

-- | @Selector@ for @sourceAppIdentifier@
sourceAppIdentifierSelector :: Selector
sourceAppIdentifierSelector = mkSelector "sourceAppIdentifier"

-- | @Selector@ for @sourceAppVersion@
sourceAppVersionSelector :: Selector
sourceAppVersionSelector = mkSelector "sourceAppVersion"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @sourceAppAuditToken@
sourceAppAuditTokenSelector :: Selector
sourceAppAuditTokenSelector = mkSelector "sourceAppAuditToken"

-- | @Selector@ for @sourceProcessAuditToken@
sourceProcessAuditTokenSelector :: Selector
sourceProcessAuditTokenSelector = mkSelector "sourceProcessAuditToken"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

