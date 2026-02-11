{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKMatchedPlayers@.
module ObjC.GameKit.GKMatchedPlayers
  ( GKMatchedPlayers
  , IsGKMatchedPlayers(..)
  , players
  , playersSelector


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

-- | @- players@
players :: IsGKMatchedPlayers gkMatchedPlayers => gkMatchedPlayers -> IO (Id NSArray)
players gkMatchedPlayers  =
  sendMsg gkMatchedPlayers (mkSelector "players") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @players@
playersSelector :: Selector
playersSelector = mkSelector "players"

