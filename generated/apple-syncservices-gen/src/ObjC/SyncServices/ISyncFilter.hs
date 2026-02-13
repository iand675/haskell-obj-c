{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncFilter@.
module ObjC.SyncServices.ISyncFilter
  ( ISyncFilter
  , IsISyncFilter(..)
  , filterMatchingAllFilters
  , filterMatchingAtLeastOneFilter
  , filterMatchingAllFiltersSelector
  , filterMatchingAtLeastOneFilterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ filterMatchingAllFilters:@
filterMatchingAllFilters :: IsNSArray filters => filters -> IO RawId
filterMatchingAllFilters filters =
  do
    cls' <- getRequiredClass "ISyncFilter"
    sendClassMessage cls' filterMatchingAllFiltersSelector (toNSArray filters)

-- | @+ filterMatchingAtLeastOneFilter:@
filterMatchingAtLeastOneFilter :: IsNSArray filters => filters -> IO RawId
filterMatchingAtLeastOneFilter filters =
  do
    cls' <- getRequiredClass "ISyncFilter"
    sendClassMessage cls' filterMatchingAtLeastOneFilterSelector (toNSArray filters)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterMatchingAllFilters:@
filterMatchingAllFiltersSelector :: Selector '[Id NSArray] RawId
filterMatchingAllFiltersSelector = mkSelector "filterMatchingAllFilters:"

-- | @Selector@ for @filterMatchingAtLeastOneFilter:@
filterMatchingAtLeastOneFilterSelector :: Selector '[Id NSArray] RawId
filterMatchingAtLeastOneFilterSelector = mkSelector "filterMatchingAtLeastOneFilter:"

