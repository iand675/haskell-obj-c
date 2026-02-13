{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetDefrosterSettingsInCarIntent@.
module ObjC.Intents.INSetDefrosterSettingsInCarIntent
  ( INSetDefrosterSettingsInCarIntent
  , IsINSetDefrosterSettingsInCarIntent(..)
  , initWithEnable_defroster_carName
  , initWithEnable_defroster
  , enable
  , defroster
  , carName
  , carNameSelector
  , defrosterSelector
  , enableSelector
  , initWithEnable_defrosterSelector
  , initWithEnable_defroster_carNameSelector

  -- * Enum types
  , INCarDefroster(INCarDefroster)
  , pattern INCarDefrosterUnknown
  , pattern INCarDefrosterFront
  , pattern INCarDefrosterRear
  , pattern INCarDefrosterAll

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

-- | @- initWithEnable:defroster:carName:@
initWithEnable_defroster_carName :: (IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent, IsNSNumber enable, IsINSpeakableString carName) => inSetDefrosterSettingsInCarIntent -> enable -> INCarDefroster -> carName -> IO (Id INSetDefrosterSettingsInCarIntent)
initWithEnable_defroster_carName inSetDefrosterSettingsInCarIntent enable defroster carName =
  sendOwnedMessage inSetDefrosterSettingsInCarIntent initWithEnable_defroster_carNameSelector (toNSNumber enable) defroster (toINSpeakableString carName)

-- | @- initWithEnable:defroster:@
initWithEnable_defroster :: (IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent, IsNSNumber enable) => inSetDefrosterSettingsInCarIntent -> enable -> INCarDefroster -> IO (Id INSetDefrosterSettingsInCarIntent)
initWithEnable_defroster inSetDefrosterSettingsInCarIntent enable defroster =
  sendOwnedMessage inSetDefrosterSettingsInCarIntent initWithEnable_defrosterSelector (toNSNumber enable) defroster

-- | @- enable@
enable :: IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent => inSetDefrosterSettingsInCarIntent -> IO (Id NSNumber)
enable inSetDefrosterSettingsInCarIntent =
  sendMessage inSetDefrosterSettingsInCarIntent enableSelector

-- | @- defroster@
defroster :: IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent => inSetDefrosterSettingsInCarIntent -> IO INCarDefroster
defroster inSetDefrosterSettingsInCarIntent =
  sendMessage inSetDefrosterSettingsInCarIntent defrosterSelector

-- | @- carName@
carName :: IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent => inSetDefrosterSettingsInCarIntent -> IO (Id INSpeakableString)
carName inSetDefrosterSettingsInCarIntent =
  sendMessage inSetDefrosterSettingsInCarIntent carNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnable:defroster:carName:@
initWithEnable_defroster_carNameSelector :: Selector '[Id NSNumber, INCarDefroster, Id INSpeakableString] (Id INSetDefrosterSettingsInCarIntent)
initWithEnable_defroster_carNameSelector = mkSelector "initWithEnable:defroster:carName:"

-- | @Selector@ for @initWithEnable:defroster:@
initWithEnable_defrosterSelector :: Selector '[Id NSNumber, INCarDefroster] (Id INSetDefrosterSettingsInCarIntent)
initWithEnable_defrosterSelector = mkSelector "initWithEnable:defroster:"

-- | @Selector@ for @enable@
enableSelector :: Selector '[] (Id NSNumber)
enableSelector = mkSelector "enable"

-- | @Selector@ for @defroster@
defrosterSelector :: Selector '[] INCarDefroster
defrosterSelector = mkSelector "defroster"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

