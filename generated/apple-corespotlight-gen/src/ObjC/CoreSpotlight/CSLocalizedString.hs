{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSLocalizedString@.
module ObjC.CoreSpotlight.CSLocalizedString
  ( CSLocalizedString
  , IsCSLocalizedString(..)
  , initWithLocalizedStrings
  , localizedString
  , initWithLocalizedStringsSelector
  , localizedStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocalizedStrings:@
initWithLocalizedStrings :: (IsCSLocalizedString csLocalizedString, IsNSDictionary localizedStrings) => csLocalizedString -> localizedStrings -> IO (Id CSLocalizedString)
initWithLocalizedStrings csLocalizedString localizedStrings =
  sendOwnedMessage csLocalizedString initWithLocalizedStringsSelector (toNSDictionary localizedStrings)

-- | @- localizedString@
localizedString :: IsCSLocalizedString csLocalizedString => csLocalizedString -> IO (Id NSString)
localizedString csLocalizedString =
  sendMessage csLocalizedString localizedStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedStrings:@
initWithLocalizedStringsSelector :: Selector '[Id NSDictionary] (Id CSLocalizedString)
initWithLocalizedStringsSelector = mkSelector "initWithLocalizedStrings:"

-- | @Selector@ for @localizedString@
localizedStringSelector :: Selector '[] (Id NSString)
localizedStringSelector = mkSelector "localizedString"

