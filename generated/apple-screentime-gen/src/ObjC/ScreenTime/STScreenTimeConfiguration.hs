{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The configuration for this device.
--
-- Generated bindings for @STScreenTimeConfiguration@.
module ObjC.ScreenTime.STScreenTimeConfiguration
  ( STScreenTimeConfiguration
  , IsSTScreenTimeConfiguration(..)
  , init_
  , new
  , enforcesChildRestrictions
  , enforcesChildRestrictionsSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenTime.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSTScreenTimeConfiguration stScreenTimeConfiguration => stScreenTimeConfiguration -> IO (Id STScreenTimeConfiguration)
init_ stScreenTimeConfiguration =
  sendOwnedMessage stScreenTimeConfiguration initSelector

-- | @+ new@
new :: IO (Id STScreenTimeConfiguration)
new  =
  do
    cls' <- getRequiredClass "STScreenTimeConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | A Boolean that indicates whether the device is currently enforcing child restrictions.
--
-- ObjC selector: @- enforcesChildRestrictions@
enforcesChildRestrictions :: IsSTScreenTimeConfiguration stScreenTimeConfiguration => stScreenTimeConfiguration -> IO Bool
enforcesChildRestrictions stScreenTimeConfiguration =
  sendMessage stScreenTimeConfiguration enforcesChildRestrictionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id STScreenTimeConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id STScreenTimeConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @enforcesChildRestrictions@
enforcesChildRestrictionsSelector :: Selector '[] Bool
enforcesChildRestrictionsSelector = mkSelector "enforcesChildRestrictions"

