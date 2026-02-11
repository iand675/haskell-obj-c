{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKBasePlayer@.
module ObjC.GameKit.GKBasePlayer
  ( GKBasePlayer
  , IsGKBasePlayer(..)
  , playerID
  , displayName
  , playerIDSelector
  , displayNameSelector


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

-- | @- playerID@
playerID :: IsGKBasePlayer gkBasePlayer => gkBasePlayer -> IO (Id NSString)
playerID gkBasePlayer  =
  sendMsg gkBasePlayer (mkSelector "playerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This player's name representation as displayed in the Game Center in-game UI. Use this when you need to display the player's name. The display name may be very long, so be sure to use appropriate string truncation API when drawing.
--
-- ObjC selector: @- displayName@
displayName :: IsGKBasePlayer gkBasePlayer => gkBasePlayer -> IO (Id NSString)
displayName gkBasePlayer  =
  sendMsg gkBasePlayer (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playerID@
playerIDSelector :: Selector
playerIDSelector = mkSelector "playerID"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

