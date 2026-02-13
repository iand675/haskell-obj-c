{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKBasePlayer@.
module ObjC.GameKit.GKBasePlayer
  ( GKBasePlayer
  , IsGKBasePlayer(..)
  , playerID
  , displayName
  , displayNameSelector
  , playerIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- playerID@
playerID :: IsGKBasePlayer gkBasePlayer => gkBasePlayer -> IO (Id NSString)
playerID gkBasePlayer =
  sendMessage gkBasePlayer playerIDSelector

-- | This player's name representation as displayed in the Game Center in-game UI. Use this when you need to display the player's name. The display name may be very long, so be sure to use appropriate string truncation API when drawing.
--
-- ObjC selector: @- displayName@
displayName :: IsGKBasePlayer gkBasePlayer => gkBasePlayer -> IO (Id NSString)
displayName gkBasePlayer =
  sendMessage gkBasePlayer displayNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playerID@
playerIDSelector :: Selector '[] (Id NSString)
playerIDSelector = mkSelector "playerID"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

