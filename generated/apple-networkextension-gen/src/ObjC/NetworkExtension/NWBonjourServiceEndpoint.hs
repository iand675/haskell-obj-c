{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NWBonjourServiceEndpoint
--
-- NWBonjourServiceEndpoint is a subclass of NWEndpoint. It represents an endpoint		backed by a Bonjour service, specified with a name, type, and domain. For example, the		Bonjour service MyMusicStudio._music._tcp.local. has the name "MyMusicStudio",		the type "_music._tcp", and the domain "local".
--
-- Generated bindings for @NWBonjourServiceEndpoint@.
module ObjC.NetworkExtension.NWBonjourServiceEndpoint
  ( NWBonjourServiceEndpoint
  , IsNWBonjourServiceEndpoint(..)
  , endpointWithName_type_domain
  , name
  , type_
  , domain
  , endpointWithName_type_domainSelector
  , nameSelector
  , typeSelector
  , domainSelector


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

-- | endpointWithName:type:domain:
--
-- @name@ — The Bonjour service name.
--
-- @type@ — The Bonjour service type.
--
-- @domain@ — The Bonjour service domain.
--
-- Returns: An initialized NWBonjourServiceEndpoint object.
--
-- ObjC selector: @+ endpointWithName:type:domain:@
endpointWithName_type_domain :: (IsNSString name, IsNSString type_, IsNSString domain) => name -> type_ -> domain -> IO (Id NWBonjourServiceEndpoint)
endpointWithName_type_domain name type_ domain =
  do
    cls' <- getRequiredClass "NWBonjourServiceEndpoint"
    withObjCPtr name $ \raw_name ->
      withObjCPtr type_ $ \raw_type_ ->
        withObjCPtr domain $ \raw_domain ->
          sendClassMsg cls' (mkSelector "endpointWithName:type:domain:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_domain :: Ptr ())] >>= retainedObject . castPtr

-- | name
--
-- The endpoint's Bonjour service name.
--
-- ObjC selector: @- name@
name :: IsNWBonjourServiceEndpoint nwBonjourServiceEndpoint => nwBonjourServiceEndpoint -> IO (Id NSString)
name nwBonjourServiceEndpoint  =
    sendMsg nwBonjourServiceEndpoint (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | type
--
-- The endpoint's Bonjour service type.
--
-- ObjC selector: @- type@
type_ :: IsNWBonjourServiceEndpoint nwBonjourServiceEndpoint => nwBonjourServiceEndpoint -> IO (Id NSString)
type_ nwBonjourServiceEndpoint  =
    sendMsg nwBonjourServiceEndpoint (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | domain
--
-- The endpoint's Bonjour service domain.
--
-- ObjC selector: @- domain@
domain :: IsNWBonjourServiceEndpoint nwBonjourServiceEndpoint => nwBonjourServiceEndpoint -> IO (Id NSString)
domain nwBonjourServiceEndpoint  =
    sendMsg nwBonjourServiceEndpoint (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointWithName:type:domain:@
endpointWithName_type_domainSelector :: Selector
endpointWithName_type_domainSelector = mkSelector "endpointWithName:type:domain:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

