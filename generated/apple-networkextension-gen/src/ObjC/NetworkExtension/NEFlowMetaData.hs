{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFlowMetaData
--
-- The NEFlowMetaData class declares the programmatic interface for an object that contains extra information about a flow.
--
-- Generated bindings for @NEFlowMetaData@.
module ObjC.NetworkExtension.NEFlowMetaData
  ( NEFlowMetaData
  , IsNEFlowMetaData(..)
  , sourceAppUniqueIdentifier
  , sourceAppSigningIdentifier
  , sourceAppAuditToken
  , filterFlowIdentifier
  , filterFlowIdentifierSelector
  , sourceAppAuditTokenSelector
  , sourceAppSigningIdentifierSelector
  , sourceAppUniqueIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sourceAppUniqueIdentifier
--
-- A byte string that uniquely identifies the binary for each build of the source application of the flow. The data object may be empty in cases where the flow originates from a system process.
--
-- ObjC selector: @- sourceAppUniqueIdentifier@
sourceAppUniqueIdentifier :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSData)
sourceAppUniqueIdentifier neFlowMetaData =
  sendMessage neFlowMetaData sourceAppUniqueIdentifierSelector

-- | sourceAppSigningIdentifier
--
-- A string containing the signing identifier (almost always equivalent to the bundle identifier) of the source app of the flow. The string may be empty in cases where the flow originates from a system process.
--
-- ObjC selector: @- sourceAppSigningIdentifier@
sourceAppSigningIdentifier :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSString)
sourceAppSigningIdentifier neFlowMetaData =
  sendMessage neFlowMetaData sourceAppSigningIdentifierSelector

-- | sourceAppAuditToken
--
-- Audit token of the source application of the flow.
--
-- ObjC selector: @- sourceAppAuditToken@
sourceAppAuditToken :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSData)
sourceAppAuditToken neFlowMetaData =
  sendMessage neFlowMetaData sourceAppAuditTokenSelector

-- | filterFlowIdentifier
--
-- The identifier of the content filter flow corresponding to this flow.
--
-- ObjC selector: @- filterFlowIdentifier@
filterFlowIdentifier :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSUUID)
filterFlowIdentifier neFlowMetaData =
  sendMessage neFlowMetaData filterFlowIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceAppUniqueIdentifier@
sourceAppUniqueIdentifierSelector :: Selector '[] (Id NSData)
sourceAppUniqueIdentifierSelector = mkSelector "sourceAppUniqueIdentifier"

-- | @Selector@ for @sourceAppSigningIdentifier@
sourceAppSigningIdentifierSelector :: Selector '[] (Id NSString)
sourceAppSigningIdentifierSelector = mkSelector "sourceAppSigningIdentifier"

-- | @Selector@ for @sourceAppAuditToken@
sourceAppAuditTokenSelector :: Selector '[] (Id NSData)
sourceAppAuditTokenSelector = mkSelector "sourceAppAuditToken"

-- | @Selector@ for @filterFlowIdentifier@
filterFlowIdentifierSelector :: Selector '[] (Id NSUUID)
filterFlowIdentifierSelector = mkSelector "filterFlowIdentifier"

