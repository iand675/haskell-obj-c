{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLocationRelevanceProvider@.
module ObjC.Intents.INLocationRelevanceProvider
  ( INLocationRelevanceProvider
  , IsINLocationRelevanceProvider(..)
  , initWithRegion
  , region
  , initWithRegionSelector
  , regionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a location relevance provider with the specified region.
--
-- ObjC selector: @- initWithRegion:@
initWithRegion :: (IsINLocationRelevanceProvider inLocationRelevanceProvider, IsCLRegion region) => inLocationRelevanceProvider -> region -> IO (Id INLocationRelevanceProvider)
initWithRegion inLocationRelevanceProvider region =
  sendOwnedMessage inLocationRelevanceProvider initWithRegionSelector (toCLRegion region)

-- | The region representing the relevant location.
--
-- CLCircularRegion
--
-- ObjC selector: @- region@
region :: IsINLocationRelevanceProvider inLocationRelevanceProvider => inLocationRelevanceProvider -> IO (Id CLRegion)
region inLocationRelevanceProvider =
  sendMessage inLocationRelevanceProvider regionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRegion:@
initWithRegionSelector :: Selector '[Id CLRegion] (Id INLocationRelevanceProvider)
initWithRegionSelector = mkSelector "initWithRegion:"

-- | @Selector@ for @region@
regionSelector :: Selector '[] (Id CLRegion)
regionSelector = mkSelector "region"

