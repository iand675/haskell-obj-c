{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCancelRideIntent@.
module ObjC.Intents.INCancelRideIntent
  ( INCancelRideIntent
  , IsINCancelRideIntent(..)
  , init_
  , initWithRideIdentifier
  , rideIdentifier
  , initSelector
  , initWithRideIdentifierSelector
  , rideIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCancelRideIntent inCancelRideIntent => inCancelRideIntent -> IO (Id INCancelRideIntent)
init_ inCancelRideIntent =
  sendOwnedMessage inCancelRideIntent initSelector

-- | @- initWithRideIdentifier:@
initWithRideIdentifier :: (IsINCancelRideIntent inCancelRideIntent, IsNSString rideIdentifier) => inCancelRideIntent -> rideIdentifier -> IO (Id INCancelRideIntent)
initWithRideIdentifier inCancelRideIntent rideIdentifier =
  sendOwnedMessage inCancelRideIntent initWithRideIdentifierSelector (toNSString rideIdentifier)

-- | @- rideIdentifier@
rideIdentifier :: IsINCancelRideIntent inCancelRideIntent => inCancelRideIntent -> IO (Id NSString)
rideIdentifier inCancelRideIntent =
  sendMessage inCancelRideIntent rideIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCancelRideIntent)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRideIdentifier:@
initWithRideIdentifierSelector :: Selector '[Id NSString] (Id INCancelRideIntent)
initWithRideIdentifierSelector = mkSelector "initWithRideIdentifier:"

-- | @Selector@ for @rideIdentifier@
rideIdentifierSelector :: Selector '[] (Id NSString)
rideIdentifierSelector = mkSelector "rideIdentifier"

