{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INActivateCarSignalIntent@.
module ObjC.Intents.INActivateCarSignalIntent
  ( INActivateCarSignalIntent
  , IsINActivateCarSignalIntent(..)
  , initWithCarName_signals
  , carName
  , signals
  , carNameSelector
  , initWithCarName_signalsSelector
  , signalsSelector

  -- * Enum types
  , INCarSignalOptions(INCarSignalOptions)
  , pattern INCarSignalOptionAudible
  , pattern INCarSignalOptionVisible

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithCarName:signals:@
initWithCarName_signals :: (IsINActivateCarSignalIntent inActivateCarSignalIntent, IsINSpeakableString carName) => inActivateCarSignalIntent -> carName -> INCarSignalOptions -> IO (Id INActivateCarSignalIntent)
initWithCarName_signals inActivateCarSignalIntent carName signals =
  sendOwnedMessage inActivateCarSignalIntent initWithCarName_signalsSelector (toINSpeakableString carName) signals

-- | @- carName@
carName :: IsINActivateCarSignalIntent inActivateCarSignalIntent => inActivateCarSignalIntent -> IO (Id INSpeakableString)
carName inActivateCarSignalIntent =
  sendMessage inActivateCarSignalIntent carNameSelector

-- | @- signals@
signals :: IsINActivateCarSignalIntent inActivateCarSignalIntent => inActivateCarSignalIntent -> IO INCarSignalOptions
signals inActivateCarSignalIntent =
  sendMessage inActivateCarSignalIntent signalsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCarName:signals:@
initWithCarName_signalsSelector :: Selector '[Id INSpeakableString, INCarSignalOptions] (Id INActivateCarSignalIntent)
initWithCarName_signalsSelector = mkSelector "initWithCarName:signals:"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

-- | @Selector@ for @signals@
signalsSelector :: Selector '[] INCarSignalOptions
signalsSelector = mkSelector "signals"

