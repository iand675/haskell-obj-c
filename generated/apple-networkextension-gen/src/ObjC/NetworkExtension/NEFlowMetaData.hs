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
  , sourceAppUniqueIdentifierSelector
  , sourceAppSigningIdentifierSelector
  , sourceAppAuditTokenSelector
  , filterFlowIdentifierSelector


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
import ObjC.Foundation.Internal.Classes

-- | sourceAppUniqueIdentifier
--
-- A byte string that uniquely identifies the binary for each build of the source application of the flow. The data object may be empty in cases where the flow originates from a system process.
--
-- ObjC selector: @- sourceAppUniqueIdentifier@
sourceAppUniqueIdentifier :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSData)
sourceAppUniqueIdentifier neFlowMetaData  =
    sendMsg neFlowMetaData (mkSelector "sourceAppUniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAppSigningIdentifier
--
-- A string containing the signing identifier (almost always equivalent to the bundle identifier) of the source app of the flow. The string may be empty in cases where the flow originates from a system process.
--
-- ObjC selector: @- sourceAppSigningIdentifier@
sourceAppSigningIdentifier :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSString)
sourceAppSigningIdentifier neFlowMetaData  =
    sendMsg neFlowMetaData (mkSelector "sourceAppSigningIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAppAuditToken
--
-- Audit token of the source application of the flow.
--
-- ObjC selector: @- sourceAppAuditToken@
sourceAppAuditToken :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSData)
sourceAppAuditToken neFlowMetaData  =
    sendMsg neFlowMetaData (mkSelector "sourceAppAuditToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | filterFlowIdentifier
--
-- The identifier of the content filter flow corresponding to this flow.
--
-- ObjC selector: @- filterFlowIdentifier@
filterFlowIdentifier :: IsNEFlowMetaData neFlowMetaData => neFlowMetaData -> IO (Id NSUUID)
filterFlowIdentifier neFlowMetaData  =
    sendMsg neFlowMetaData (mkSelector "filterFlowIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceAppUniqueIdentifier@
sourceAppUniqueIdentifierSelector :: Selector
sourceAppUniqueIdentifierSelector = mkSelector "sourceAppUniqueIdentifier"

-- | @Selector@ for @sourceAppSigningIdentifier@
sourceAppSigningIdentifierSelector :: Selector
sourceAppSigningIdentifierSelector = mkSelector "sourceAppSigningIdentifier"

-- | @Selector@ for @sourceAppAuditToken@
sourceAppAuditTokenSelector :: Selector
sourceAppAuditTokenSelector = mkSelector "sourceAppAuditToken"

-- | @Selector@ for @filterFlowIdentifier@
filterFlowIdentifierSelector :: Selector
filterFlowIdentifierSelector = mkSelector "filterFlowIdentifier"

