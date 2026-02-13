{-# LANGUAGE DataKinds #-}
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
  , domainSelector
  , endpointWithName_type_domainSelector
  , nameSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' endpointWithName_type_domainSelector (toNSString name) (toNSString type_) (toNSString domain)

-- | name
--
-- The endpoint's Bonjour service name.
--
-- ObjC selector: @- name@
name :: IsNWBonjourServiceEndpoint nwBonjourServiceEndpoint => nwBonjourServiceEndpoint -> IO (Id NSString)
name nwBonjourServiceEndpoint =
  sendMessage nwBonjourServiceEndpoint nameSelector

-- | type
--
-- The endpoint's Bonjour service type.
--
-- ObjC selector: @- type@
type_ :: IsNWBonjourServiceEndpoint nwBonjourServiceEndpoint => nwBonjourServiceEndpoint -> IO (Id NSString)
type_ nwBonjourServiceEndpoint =
  sendMessage nwBonjourServiceEndpoint typeSelector

-- | domain
--
-- The endpoint's Bonjour service domain.
--
-- ObjC selector: @- domain@
domain :: IsNWBonjourServiceEndpoint nwBonjourServiceEndpoint => nwBonjourServiceEndpoint -> IO (Id NSString)
domain nwBonjourServiceEndpoint =
  sendMessage nwBonjourServiceEndpoint domainSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointWithName:type:domain:@
endpointWithName_type_domainSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NWBonjourServiceEndpoint)
endpointWithName_type_domainSelector = mkSelector "endpointWithName:type:domain:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

