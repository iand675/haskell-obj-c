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

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ filterMatchingAllFilters:@
filterMatchingAllFilters :: IsNSArray filters => filters -> IO RawId
filterMatchingAllFilters filters =
  do
    cls' <- getRequiredClass "ISyncFilter"
    withObjCPtr filters $ \raw_filters ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "filterMatchingAllFilters:") (retPtr retVoid) [argPtr (castPtr raw_filters :: Ptr ())]

-- | @+ filterMatchingAtLeastOneFilter:@
filterMatchingAtLeastOneFilter :: IsNSArray filters => filters -> IO RawId
filterMatchingAtLeastOneFilter filters =
  do
    cls' <- getRequiredClass "ISyncFilter"
    withObjCPtr filters $ \raw_filters ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "filterMatchingAtLeastOneFilter:") (retPtr retVoid) [argPtr (castPtr raw_filters :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterMatchingAllFilters:@
filterMatchingAllFiltersSelector :: Selector
filterMatchingAllFiltersSelector = mkSelector "filterMatchingAllFilters:"

-- | @Selector@ for @filterMatchingAtLeastOneFilter:@
filterMatchingAtLeastOneFilterSelector :: Selector
filterMatchingAtLeastOneFilterSelector = mkSelector "filterMatchingAtLeastOneFilter:"

