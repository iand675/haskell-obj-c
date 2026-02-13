{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetProfileInCarIntent@.
module ObjC.Intents.INSetProfileInCarIntent
  ( INSetProfileInCarIntent
  , IsINSetProfileInCarIntent(..)
  , initWithProfileNumber_profileName_defaultProfile_carName
  , initWithProfileNumber_profileLabel_defaultProfile
  , initWithProfileNumber_profileName_defaultProfile
  , profileNumber
  , profileName
  , defaultProfile
  , carName
  , profileLabel
  , carNameSelector
  , defaultProfileSelector
  , initWithProfileNumber_profileLabel_defaultProfileSelector
  , initWithProfileNumber_profileName_defaultProfileSelector
  , initWithProfileNumber_profileName_defaultProfile_carNameSelector
  , profileLabelSelector
  , profileNameSelector
  , profileNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithProfileNumber:profileName:defaultProfile:carName:@
initWithProfileNumber_profileName_defaultProfile_carName :: (IsINSetProfileInCarIntent inSetProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileName, IsNSNumber defaultProfile, IsINSpeakableString carName) => inSetProfileInCarIntent -> profileNumber -> profileName -> defaultProfile -> carName -> IO (Id INSetProfileInCarIntent)
initWithProfileNumber_profileName_defaultProfile_carName inSetProfileInCarIntent profileNumber profileName defaultProfile carName =
  sendOwnedMessage inSetProfileInCarIntent initWithProfileNumber_profileName_defaultProfile_carNameSelector (toNSNumber profileNumber) (toNSString profileName) (toNSNumber defaultProfile) (toINSpeakableString carName)

-- | @- initWithProfileNumber:profileLabel:defaultProfile:@
initWithProfileNumber_profileLabel_defaultProfile :: (IsINSetProfileInCarIntent inSetProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileLabel, IsNSNumber defaultProfile) => inSetProfileInCarIntent -> profileNumber -> profileLabel -> defaultProfile -> IO (Id INSetProfileInCarIntent)
initWithProfileNumber_profileLabel_defaultProfile inSetProfileInCarIntent profileNumber profileLabel defaultProfile =
  sendOwnedMessage inSetProfileInCarIntent initWithProfileNumber_profileLabel_defaultProfileSelector (toNSNumber profileNumber) (toNSString profileLabel) (toNSNumber defaultProfile)

-- | @- initWithProfileNumber:profileName:defaultProfile:@
initWithProfileNumber_profileName_defaultProfile :: (IsINSetProfileInCarIntent inSetProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileName, IsNSNumber defaultProfile) => inSetProfileInCarIntent -> profileNumber -> profileName -> defaultProfile -> IO (Id INSetProfileInCarIntent)
initWithProfileNumber_profileName_defaultProfile inSetProfileInCarIntent profileNumber profileName defaultProfile =
  sendOwnedMessage inSetProfileInCarIntent initWithProfileNumber_profileName_defaultProfileSelector (toNSNumber profileNumber) (toNSString profileName) (toNSNumber defaultProfile)

-- | @- profileNumber@
profileNumber :: IsINSetProfileInCarIntent inSetProfileInCarIntent => inSetProfileInCarIntent -> IO (Id NSNumber)
profileNumber inSetProfileInCarIntent =
  sendMessage inSetProfileInCarIntent profileNumberSelector

-- | @- profileName@
profileName :: IsINSetProfileInCarIntent inSetProfileInCarIntent => inSetProfileInCarIntent -> IO (Id NSString)
profileName inSetProfileInCarIntent =
  sendMessage inSetProfileInCarIntent profileNameSelector

-- | @- defaultProfile@
defaultProfile :: IsINSetProfileInCarIntent inSetProfileInCarIntent => inSetProfileInCarIntent -> IO (Id NSNumber)
defaultProfile inSetProfileInCarIntent =
  sendMessage inSetProfileInCarIntent defaultProfileSelector

-- | @- carName@
carName :: IsINSetProfileInCarIntent inSetProfileInCarIntent => inSetProfileInCarIntent -> IO (Id INSpeakableString)
carName inSetProfileInCarIntent =
  sendMessage inSetProfileInCarIntent carNameSelector

-- | @- profileLabel@
profileLabel :: IsINSetProfileInCarIntent inSetProfileInCarIntent => inSetProfileInCarIntent -> IO (Id NSString)
profileLabel inSetProfileInCarIntent =
  sendMessage inSetProfileInCarIntent profileLabelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProfileNumber:profileName:defaultProfile:carName:@
initWithProfileNumber_profileName_defaultProfile_carNameSelector :: Selector '[Id NSNumber, Id NSString, Id NSNumber, Id INSpeakableString] (Id INSetProfileInCarIntent)
initWithProfileNumber_profileName_defaultProfile_carNameSelector = mkSelector "initWithProfileNumber:profileName:defaultProfile:carName:"

-- | @Selector@ for @initWithProfileNumber:profileLabel:defaultProfile:@
initWithProfileNumber_profileLabel_defaultProfileSelector :: Selector '[Id NSNumber, Id NSString, Id NSNumber] (Id INSetProfileInCarIntent)
initWithProfileNumber_profileLabel_defaultProfileSelector = mkSelector "initWithProfileNumber:profileLabel:defaultProfile:"

-- | @Selector@ for @initWithProfileNumber:profileName:defaultProfile:@
initWithProfileNumber_profileName_defaultProfileSelector :: Selector '[Id NSNumber, Id NSString, Id NSNumber] (Id INSetProfileInCarIntent)
initWithProfileNumber_profileName_defaultProfileSelector = mkSelector "initWithProfileNumber:profileName:defaultProfile:"

-- | @Selector@ for @profileNumber@
profileNumberSelector :: Selector '[] (Id NSNumber)
profileNumberSelector = mkSelector "profileNumber"

-- | @Selector@ for @profileName@
profileNameSelector :: Selector '[] (Id NSString)
profileNameSelector = mkSelector "profileName"

-- | @Selector@ for @defaultProfile@
defaultProfileSelector :: Selector '[] (Id NSNumber)
defaultProfileSelector = mkSelector "defaultProfile"

-- | @Selector@ for @carName@
carNameSelector :: Selector '[] (Id INSpeakableString)
carNameSelector = mkSelector "carName"

-- | @Selector@ for @profileLabel@
profileLabelSelector :: Selector '[] (Id NSString)
profileLabelSelector = mkSelector "profileLabel"

