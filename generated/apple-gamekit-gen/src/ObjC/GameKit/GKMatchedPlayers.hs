{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKMatchedPlayers@.
module ObjC.GameKit.GKMatchedPlayers
  ( GKMatchedPlayers
  , IsGKMatchedPlayers(..)
  , properties
  , players
  , playerProperties
  , playerPropertiesSelector
  , playersSelector
  , propertiesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- properties@
properties :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO RawId
properties gkMatchedPlayers =
  sendMessage gkMatchedPlayers propertiesSelector

-- | @- players@
players :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO (Id NSArray)
players gkMatchedPlayers =
  sendMessage gkMatchedPlayers playersSelector

-- | @- playerProperties@
playerProperties :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO (Id NSDictionary)
playerProperties gkMatchedPlayers =
  sendMessage gkMatchedPlayers playerPropertiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] RawId
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @players@
playersSelector :: Selector '[] (Id NSArray)
playersSelector = mkSelector "players"

-- | @Selector@ for @playerProperties@
playerPropertiesSelector :: Selector '[] (Id NSDictionary)
playerPropertiesSelector = mkSelector "playerProperties"

