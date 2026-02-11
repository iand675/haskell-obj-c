{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotHelperCommand
--
-- An NEHotspotHelperCommand object is provided to the helper's   command handler block. The HotspotHelper processes the command   instantiates an NEHotspotHelperResponse object, sets the annotated   network or networkList (Evaluate/FilterScanList only),   then delivers it.
--
-- Generated bindings for @NEHotspotHelperCommand@.
module ObjC.NetworkExtension.NEHotspotHelperCommand
  ( NEHotspotHelperCommand
  , IsNEHotspotHelperCommand(..)
  , createResponse
  , createTCPConnection
  , createUDPSession
  , commandType
  , network
  , networkList
  , interface
  , createResponseSelector
  , createTCPConnectionSelector
  , createUDPSessionSelector
  , commandTypeSelector
  , networkSelector
  , networkListSelector
  , interfaceSelector

  -- * Enum types
  , NEHotspotHelperCommandType(NEHotspotHelperCommandType)
  , pattern KNEHotspotHelperCommandTypeNone
  , pattern KNEHotspotHelperCommandTypeFilterScanList
  , pattern KNEHotspotHelperCommandTypeEvaluate
  , pattern KNEHotspotHelperCommandTypeAuthenticate
  , pattern KNEHotspotHelperCommandTypePresentUI
  , pattern KNEHotspotHelperCommandTypeMaintain
  , pattern KNEHotspotHelperCommandTypeLogoff
  , NEHotspotHelperResult(NEHotspotHelperResult)
  , pattern KNEHotspotHelperResultSuccess
  , pattern KNEHotspotHelperResultFailure
  , pattern KNEHotspotHelperResultUIRequired
  , pattern KNEHotspotHelperResultCommandNotRecognized
  , pattern KNEHotspotHelperResultAuthenticationRequired
  , pattern KNEHotspotHelperResultUnsupportedNetwork
  , pattern KNEHotspotHelperResultTemporaryFailure

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

-- | createResponse:
--
-- Create a response to the command.
--
-- Instantiate an NEHotspotHelperResponse for the command.
--
-- Returns: NEHotspotHelperResponse with the specified result.
--
-- ObjC selector: @- createResponse:@
createResponse :: IsNEHotspotHelperCommand neHotspotHelperCommand => neHotspotHelperCommand -> NEHotspotHelperResult -> IO (Id NEHotspotHelperResponse)
createResponse neHotspotHelperCommand  result =
    sendMsg neHotspotHelperCommand (mkSelector "createResponse:") (retPtr retVoid) [argCLong (coerce result)] >>= retainedObject . castPtr

-- | createTCPConnection
--
-- Create a new TCP connection over the interface associated with the command.
--
-- Instantiate an NWTCPConnection to the specified endpoint   bound to the network interface associated with the command.
--
-- Returns: non-nil NWTCPConnection object if successful, nil otherwise
--
-- ObjC selector: @- createTCPConnection:@
createTCPConnection :: (IsNEHotspotHelperCommand neHotspotHelperCommand, IsNWEndpoint endpoint) => neHotspotHelperCommand -> endpoint -> IO (Id NWTCPConnection)
createTCPConnection neHotspotHelperCommand  endpoint =
  withObjCPtr endpoint $ \raw_endpoint ->
      sendMsg neHotspotHelperCommand (mkSelector "createTCPConnection:") (retPtr retVoid) [argPtr (castPtr raw_endpoint :: Ptr ())] >>= retainedObject . castPtr

-- | createUDPSession
--
-- Create a new UDP session over the interface associated with the command.
--
-- Instantiate an NWUDPSession to the specified endpoint   bound to the network interface associated with the command.
--
-- Returns: non-nil NWUDPSession object if successful, nil otherwise
--
-- ObjC selector: @- createUDPSession:@
createUDPSession :: (IsNEHotspotHelperCommand neHotspotHelperCommand, IsNWEndpoint endpoint) => neHotspotHelperCommand -> endpoint -> IO (Id NWUDPSession)
createUDPSession neHotspotHelperCommand  endpoint =
  withObjCPtr endpoint $ \raw_endpoint ->
      sendMsg neHotspotHelperCommand (mkSelector "createUDPSession:") (retPtr retVoid) [argPtr (castPtr raw_endpoint :: Ptr ())] >>= retainedObject . castPtr

-- | commandType
--
-- The type of the command.
--
-- ObjC selector: @- commandType@
commandType :: IsNEHotspotHelperCommand neHotspotHelperCommand => neHotspotHelperCommand -> IO NEHotspotHelperCommandType
commandType neHotspotHelperCommand  =
    fmap (coerce :: CLong -> NEHotspotHelperCommandType) $ sendMsg neHotspotHelperCommand (mkSelector "commandType") retCLong []

-- | network
--
-- The network associated with the command. May be nil.
--
-- ObjC selector: @- network@
network :: IsNEHotspotHelperCommand neHotspotHelperCommand => neHotspotHelperCommand -> IO (Id NEHotspotNetwork)
network neHotspotHelperCommand  =
    sendMsg neHotspotHelperCommand (mkSelector "network") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | networkList
--
-- The list of networks associated with a command. Will be nil unless   the command type is kNEHotspotHelperCommandTypeFilterScanList.   This property returns an NSArray of NEHotspotNetwork.
--
-- ObjC selector: @- networkList@
networkList :: IsNEHotspotHelperCommand neHotspotHelperCommand => neHotspotHelperCommand -> IO (Id NSArray)
networkList neHotspotHelperCommand  =
    sendMsg neHotspotHelperCommand (mkSelector "networkList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | interface
--
-- Network interface associated with the command.
--
-- To create a connection over the hotspot, set the interface on the corresponding parameters using @nw_parameters_require_interface@.
--
-- ObjC selector: @- interface@
interface :: IsNEHotspotHelperCommand neHotspotHelperCommand => neHotspotHelperCommand -> IO (Id NSObject)
interface neHotspotHelperCommand  =
    sendMsg neHotspotHelperCommand (mkSelector "interface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createResponse:@
createResponseSelector :: Selector
createResponseSelector = mkSelector "createResponse:"

-- | @Selector@ for @createTCPConnection:@
createTCPConnectionSelector :: Selector
createTCPConnectionSelector = mkSelector "createTCPConnection:"

-- | @Selector@ for @createUDPSession:@
createUDPSessionSelector :: Selector
createUDPSessionSelector = mkSelector "createUDPSession:"

-- | @Selector@ for @commandType@
commandTypeSelector :: Selector
commandTypeSelector = mkSelector "commandType"

-- | @Selector@ for @network@
networkSelector :: Selector
networkSelector = mkSelector "network"

-- | @Selector@ for @networkList@
networkListSelector :: Selector
networkListSelector = mkSelector "networkList"

-- | @Selector@ for @interface@
interfaceSelector :: Selector
interfaceSelector = mkSelector "interface"

