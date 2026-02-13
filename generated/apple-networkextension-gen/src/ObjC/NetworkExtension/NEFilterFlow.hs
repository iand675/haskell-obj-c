{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , directionSelector
  , identifierSelector
  , sourceAppAuditTokenSelector
  , sourceAppIdentifierSelector
  , sourceAppUniqueIdentifierSelector
  , sourceAppVersionSelector
  , sourceProcessAuditTokenSelector
  , urlSelector

  -- * Enum types
  , NETrafficDirection(NETrafficDirection)
  , pattern NETrafficDirectionAny
  , pattern NETrafficDirectionInbound
  , pattern NETrafficDirectionOutbound

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

-- | URL
--
-- The flow's HTTP request URL. Will be nil if the flow did not originate from WebKit.
--
-- ObjC selector: @- URL@
url :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSURL)
url neFilterFlow =
  sendMessage neFilterFlow urlSelector

-- | sourceAppUniqueIdentifier
--
-- A byte string that uniquely identifies the binary for each build of the source application of the flow.
--
-- ObjC selector: @- sourceAppUniqueIdentifier@
sourceAppUniqueIdentifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSData)
sourceAppUniqueIdentifier neFilterFlow =
  sendMessage neFilterFlow sourceAppUniqueIdentifierSelector

-- | sourceAppIdentifier
--
-- A string containing the identifier of the source application of the flow. This identifier stays the same for all versions and builds of the application. This identifier is unique among all applications.
--
-- ObjC selector: @- sourceAppIdentifier@
sourceAppIdentifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSString)
sourceAppIdentifier neFilterFlow =
  sendMessage neFilterFlow sourceAppIdentifierSelector

-- | sourceAppVersion
--
-- The short version string of the source application. Will be nil if the app info is unavailable.
--
-- ObjC selector: @- sourceAppVersion@
sourceAppVersion :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSString)
sourceAppVersion neFilterFlow =
  sendMessage neFilterFlow sourceAppVersionSelector

-- | direction
--
-- Initial direction of the flow (outgoing or incoming flow)
--
-- ObjC selector: @- direction@
direction :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO NETrafficDirection
direction neFilterFlow =
  sendMessage neFilterFlow directionSelector

-- | sourceAppAuditToken
--
-- Audit token of the source application of the flow.
--
-- ObjC selector: @- sourceAppAuditToken@
sourceAppAuditToken :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSData)
sourceAppAuditToken neFilterFlow =
  sendMessage neFilterFlow sourceAppAuditTokenSelector

-- | sourceProcessAuditToken
--
-- The audit token of the process that created the flow. In cases where the connection was created by a system process on behalf of the source application, sourceProcessAuditToken will be different from sourceAppAuditToken and will contain the audit token of the system process. In cases where the source application directly created the connection sourceAppAuditToken and sourceProcessAuditToken will be identical.
--
-- ObjC selector: @- sourceProcessAuditToken@
sourceProcessAuditToken :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSData)
sourceProcessAuditToken neFilterFlow =
  sendMessage neFilterFlow sourceProcessAuditTokenSelector

-- | identifier
--
-- The unique identifier of the flow.
--
-- ObjC selector: @- identifier@
identifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSUUID)
identifier neFilterFlow =
  sendMessage neFilterFlow identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @sourceAppUniqueIdentifier@
sourceAppUniqueIdentifierSelector :: Selector '[] (Id NSData)
sourceAppUniqueIdentifierSelector = mkSelector "sourceAppUniqueIdentifier"

-- | @Selector@ for @sourceAppIdentifier@
sourceAppIdentifierSelector :: Selector '[] (Id NSString)
sourceAppIdentifierSelector = mkSelector "sourceAppIdentifier"

-- | @Selector@ for @sourceAppVersion@
sourceAppVersionSelector :: Selector '[] (Id NSString)
sourceAppVersionSelector = mkSelector "sourceAppVersion"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] NETrafficDirection
directionSelector = mkSelector "direction"

-- | @Selector@ for @sourceAppAuditToken@
sourceAppAuditTokenSelector :: Selector '[] (Id NSData)
sourceAppAuditTokenSelector = mkSelector "sourceAppAuditToken"

-- | @Selector@ for @sourceProcessAuditToken@
sourceProcessAuditTokenSelector :: Selector '[] (Id NSData)
sourceProcessAuditTokenSelector = mkSelector "sourceProcessAuditToken"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

