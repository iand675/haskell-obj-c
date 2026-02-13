{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSaveProfileInCarIntent@.
module ObjC.Intents.INSaveProfileInCarIntent
  ( INSaveProfileInCarIntent
  , IsINSaveProfileInCarIntent(..)
  , initWithProfileNumber_profileName
  , initWithProfileNumber_profileLabel
  , profileNumber
  , profileName
  , profileLabel
  , initWithProfileNumber_profileLabelSelector
  , initWithProfileNumber_profileNameSelector
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

-- | @- initWithProfileNumber:profileName:@
initWithProfileNumber_profileName :: (IsINSaveProfileInCarIntent inSaveProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileName) => inSaveProfileInCarIntent -> profileNumber -> profileName -> IO (Id INSaveProfileInCarIntent)
initWithProfileNumber_profileName inSaveProfileInCarIntent profileNumber profileName =
  sendOwnedMessage inSaveProfileInCarIntent initWithProfileNumber_profileNameSelector (toNSNumber profileNumber) (toNSString profileName)

-- | @- initWithProfileNumber:profileLabel:@
initWithProfileNumber_profileLabel :: (IsINSaveProfileInCarIntent inSaveProfileInCarIntent, IsNSNumber profileNumber, IsNSString profileLabel) => inSaveProfileInCarIntent -> profileNumber -> profileLabel -> IO (Id INSaveProfileInCarIntent)
initWithProfileNumber_profileLabel inSaveProfileInCarIntent profileNumber profileLabel =
  sendOwnedMessage inSaveProfileInCarIntent initWithProfileNumber_profileLabelSelector (toNSNumber profileNumber) (toNSString profileLabel)

-- | @- profileNumber@
profileNumber :: IsINSaveProfileInCarIntent inSaveProfileInCarIntent => inSaveProfileInCarIntent -> IO (Id NSNumber)
profileNumber inSaveProfileInCarIntent =
  sendMessage inSaveProfileInCarIntent profileNumberSelector

-- | @- profileName@
profileName :: IsINSaveProfileInCarIntent inSaveProfileInCarIntent => inSaveProfileInCarIntent -> IO (Id NSString)
profileName inSaveProfileInCarIntent =
  sendMessage inSaveProfileInCarIntent profileNameSelector

-- | @- profileLabel@
profileLabel :: IsINSaveProfileInCarIntent inSaveProfileInCarIntent => inSaveProfileInCarIntent -> IO (Id NSString)
profileLabel inSaveProfileInCarIntent =
  sendMessage inSaveProfileInCarIntent profileLabelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProfileNumber:profileName:@
initWithProfileNumber_profileNameSelector :: Selector '[Id NSNumber, Id NSString] (Id INSaveProfileInCarIntent)
initWithProfileNumber_profileNameSelector = mkSelector "initWithProfileNumber:profileName:"

-- | @Selector@ for @initWithProfileNumber:profileLabel:@
initWithProfileNumber_profileLabelSelector :: Selector '[Id NSNumber, Id NSString] (Id INSaveProfileInCarIntent)
initWithProfileNumber_profileLabelSelector = mkSelector "initWithProfileNumber:profileLabel:"

-- | @Selector@ for @profileNumber@
profileNumberSelector :: Selector '[] (Id NSNumber)
profileNumberSelector = mkSelector "profileNumber"

-- | @Selector@ for @profileName@
profileNameSelector :: Selector '[] (Id NSString)
profileNameSelector = mkSelector "profileName"

-- | @Selector@ for @profileLabel@
profileLabelSelector :: Selector '[] (Id NSString)
profileLabelSelector = mkSelector "profileLabel"

