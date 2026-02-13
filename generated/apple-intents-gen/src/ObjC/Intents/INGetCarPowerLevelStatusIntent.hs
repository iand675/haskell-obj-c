{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarPowerLevelStatusIntent@.
module ObjC.Intents.INGetCarPowerLevelStatusIntent
  ( INGetCarPowerLevelStatusIntent
  , IsINGetCarPowerLevelStatusIntent(..)
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
initWithCarName :: (IsINGetCarPowerLevelStatusIntent inGetCarPowerLevelStatusIntent, IsINSpeakableString carName) => inGetCarPowerLevelStatusIntent -> carName -> IO (Id INGetCarPowerLevelStatusIntent)
initWithCarName inGetCarPowerLevelStatusIntent carName =
  sendOwnedMessage inGetCarPowerLevelStatusIntent initWithCarNameSelector (toINSpeakableString carName)

-- | @- carName@
carName :: IsINGetCarPowerLevelStatusIntent inGetCarPowerLevelStatusIntent => inGetCarPowerLevelStatusIntent -> IO (Id INSpeakableString)
carName inGetCarPowerLevelStatusIntent =
  sendMessage inGetCarPowerLevelStatusIntent carNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCarName:@
initWithCarNameSelector :: Selector '[Id INSpeakableString] (Id INGetCarPowerLevelStatusIntent)
initWithCarNameSelector = mkSelector "initWithCarName:"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

