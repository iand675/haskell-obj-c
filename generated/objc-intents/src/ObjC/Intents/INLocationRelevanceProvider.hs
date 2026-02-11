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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a location relevance provider with the specified region.
--
-- ObjC selector: @- initWithRegion:@
initWithRegion :: (IsINLocationRelevanceProvider inLocationRelevanceProvider, IsCLRegion region) => inLocationRelevanceProvider -> region -> IO (Id INLocationRelevanceProvider)
initWithRegion inLocationRelevanceProvider  region =
withObjCPtr region $ \raw_region ->
    sendMsg inLocationRelevanceProvider (mkSelector "initWithRegion:") (retPtr retVoid) [argPtr (castPtr raw_region :: Ptr ())] >>= ownedObject . castPtr

-- | The region representing the relevant location.
--
-- CLCircularRegion
--
-- ObjC selector: @- region@
region :: IsINLocationRelevanceProvider inLocationRelevanceProvider => inLocationRelevanceProvider -> IO (Id CLRegion)
region inLocationRelevanceProvider  =
  sendMsg inLocationRelevanceProvider (mkSelector "region") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRegion:@
initWithRegionSelector :: Selector
initWithRegionSelector = mkSelector "initWithRegion:"

-- | @Selector@ for @region@
regionSelector :: Selector
regionSelector = mkSelector "region"

