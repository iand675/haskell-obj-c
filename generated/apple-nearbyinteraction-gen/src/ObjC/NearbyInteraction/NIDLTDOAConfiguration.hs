{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A session configuration that enables UWB Down Link Time Difference of Arrival(DL-TDoA) ranging with nearby anchors.
--
-- Generated bindings for @NIDLTDOAConfiguration@.
module ObjC.NearbyInteraction.NIDLTDOAConfiguration
  ( NIDLTDOAConfiguration
  , IsNIDLTDOAConfiguration(..)
  , initWithNetworkIdentifier
  , init_
  , new
  , networkIdentifier
  , setNetworkIdentifier
  , initSelector
  , initWithNetworkIdentifierSelector
  , networkIdentifierSelector
  , newSelector
  , setNetworkIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new configuration with a network identifier
--
-- ObjC selector: @- initWithNetworkIdentifier:@
initWithNetworkIdentifier :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> CLong -> IO (Id NIDLTDOAConfiguration)
initWithNetworkIdentifier nidltdoaConfiguration networkIdentifier =
  sendOwnedMessage nidltdoaConfiguration initWithNetworkIdentifierSelector networkIdentifier

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> IO (Id NIDLTDOAConfiguration)
init_ nidltdoaConfiguration =
  sendOwnedMessage nidltdoaConfiguration initSelector

-- | @+ new@
new :: IO (Id NIDLTDOAConfiguration)
new  =
  do
    cls' <- getRequiredClass "NIDLTDOAConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | A unique identifier for a network supporting UWB Down Link Time Difference of Arrival(DL-TDoA).
--
-- ObjC selector: @- networkIdentifier@
networkIdentifier :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> IO CLong
networkIdentifier nidltdoaConfiguration =
  sendMessage nidltdoaConfiguration networkIdentifierSelector

-- | A unique identifier for a network supporting UWB Down Link Time Difference of Arrival(DL-TDoA).
--
-- ObjC selector: @- setNetworkIdentifier:@
setNetworkIdentifier :: IsNIDLTDOAConfiguration nidltdoaConfiguration => nidltdoaConfiguration -> CLong -> IO ()
setNetworkIdentifier nidltdoaConfiguration value =
  sendMessage nidltdoaConfiguration setNetworkIdentifierSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNetworkIdentifier:@
initWithNetworkIdentifierSelector :: Selector '[CLong] (Id NIDLTDOAConfiguration)
initWithNetworkIdentifierSelector = mkSelector "initWithNetworkIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NIDLTDOAConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NIDLTDOAConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @networkIdentifier@
networkIdentifierSelector :: Selector '[] CLong
networkIdentifierSelector = mkSelector "networkIdentifier"

-- | @Selector@ for @setNetworkIdentifier:@
setNetworkIdentifierSelector :: Selector '[CLong] ()
setNetworkIdentifierSelector = mkSelector "setNetworkIdentifier:"

