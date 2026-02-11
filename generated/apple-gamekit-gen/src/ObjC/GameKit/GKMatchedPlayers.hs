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
  , propertiesSelector
  , playersSelector
  , playerPropertiesSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- properties@
properties :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO RawId
properties gkMatchedPlayers  =
    fmap (RawId . castPtr) $ sendMsg gkMatchedPlayers (mkSelector "properties") (retPtr retVoid) []

-- | @- players@
players :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO (Id NSArray)
players gkMatchedPlayers  =
    sendMsg gkMatchedPlayers (mkSelector "players") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playerProperties@
playerProperties :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO (Id NSDictionary)
playerProperties gkMatchedPlayers  =
    sendMsg gkMatchedPlayers (mkSelector "playerProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @players@
playersSelector :: Selector
playersSelector = mkSelector "players"

-- | @Selector@ for @playerProperties@
playerPropertiesSelector :: Selector
playerPropertiesSelector = mkSelector "playerProperties"

