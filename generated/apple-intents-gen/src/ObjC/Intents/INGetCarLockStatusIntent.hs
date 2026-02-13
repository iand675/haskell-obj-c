{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarLockStatusIntent@.
module ObjC.Intents.INGetCarLockStatusIntent
  ( INGetCarLockStatusIntent
  , IsINGetCarLockStatusIntent(..)
  , initWithCarName
  , carName
  , carNameSelector
  , initWithCarNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCarName:@
initWithCarName :: (IsINGetCarLockStatusIntent inGetCarLockStatusIntent, IsINSpeakableString carName) => inGetCarLockStatusIntent -> carName -> IO (Id INGetCarLockStatusIntent)
initWithCarName inGetCarLockStatusIntent carName =
  sendOwnedMessage inGetCarLockStatusIntent initWithCarNameSelector (toINSpeakableString carName)

-- | @- carName@
carName :: IsINGetCarLockStatusIntent inGetCarLockStatusIntent => inGetCarLockStatusIntent -> IO (Id INSpeakableString)
carName inGetCarLockStatusIntent =
  sendMessage inGetCarLockStatusIntent carNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCarName:@
initWithCarNameSelector :: Selector '[Id INSpeakableString] (Id INGetCarLockStatusIntent)
initWithCarNameSelector = mkSelector "initWithCarName:"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

