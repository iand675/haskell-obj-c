{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetCarLockStatusIntent@.
module ObjC.Intents.INSetCarLockStatusIntent
  ( INSetCarLockStatusIntent
  , IsINSetCarLockStatusIntent(..)
  , initWithLocked_carName
  , locked
  , carName
  , carNameSelector
  , initWithLocked_carNameSelector
  , lockedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocked:carName:@
initWithLocked_carName :: (IsINSetCarLockStatusIntent inSetCarLockStatusIntent, IsNSNumber locked, IsINSpeakableString carName) => inSetCarLockStatusIntent -> locked -> carName -> IO (Id INSetCarLockStatusIntent)
initWithLocked_carName inSetCarLockStatusIntent locked carName =
  sendOwnedMessage inSetCarLockStatusIntent initWithLocked_carNameSelector (toNSNumber locked) (toINSpeakableString carName)

-- | @- locked@
locked :: IsINSetCarLockStatusIntent inSetCarLockStatusIntent => inSetCarLockStatusIntent -> IO (Id NSNumber)
locked inSetCarLockStatusIntent =
  sendMessage inSetCarLockStatusIntent lockedSelector

-- | @- carName@
carName :: IsINSetCarLockStatusIntent inSetCarLockStatusIntent => inSetCarLockStatusIntent -> IO (Id INSpeakableString)
carName inSetCarLockStatusIntent =
  sendMessage inSetCarLockStatusIntent carNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocked:carName:@
initWithLocked_carNameSelector :: Selector '[Id NSNumber, Id INSpeakableString] (Id INSetCarLockStatusIntent)
initWithLocked_carNameSelector = mkSelector "initWithLocked:carName:"

-- | @Selector@ for @locked@
lockedSelector :: Selector '[] (Id NSNumber)
lockedSelector = mkSelector "locked"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

