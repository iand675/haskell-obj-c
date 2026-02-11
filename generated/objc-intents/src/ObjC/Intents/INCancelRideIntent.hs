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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCancelRideIntent inCancelRideIntent => inCancelRideIntent -> IO (Id INCancelRideIntent)
init_ inCancelRideIntent  =
  sendMsg inCancelRideIntent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRideIdentifier:@
initWithRideIdentifier :: (IsINCancelRideIntent inCancelRideIntent, IsNSString rideIdentifier) => inCancelRideIntent -> rideIdentifier -> IO (Id INCancelRideIntent)
initWithRideIdentifier inCancelRideIntent  rideIdentifier =
withObjCPtr rideIdentifier $ \raw_rideIdentifier ->
    sendMsg inCancelRideIntent (mkSelector "initWithRideIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_rideIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- rideIdentifier@
rideIdentifier :: IsINCancelRideIntent inCancelRideIntent => inCancelRideIntent -> IO (Id NSString)
rideIdentifier inCancelRideIntent  =
  sendMsg inCancelRideIntent (mkSelector "rideIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRideIdentifier:@
initWithRideIdentifierSelector :: Selector
initWithRideIdentifierSelector = mkSelector "initWithRideIdentifier:"

-- | @Selector@ for @rideIdentifier@
rideIdentifierSelector :: Selector
rideIdentifierSelector = mkSelector "rideIdentifier"

