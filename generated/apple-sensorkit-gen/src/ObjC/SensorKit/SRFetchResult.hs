{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFetchResult@.
module ObjC.SensorKit.SRFetchResult
  ( SRFetchResult
  , IsSRFetchResult(..)
  , init_
  , new
  , sample
  , timestamp
  , initSelector
  , newSelector
  , sampleSelector
  , timestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRFetchResult srFetchResult => srFetchResult -> IO (Id SRFetchResult)
init_ srFetchResult =
  sendOwnedMessage srFetchResult initSelector

-- | @+ new@
new :: IO (Id SRFetchResult)
new  =
  do
    cls' <- getRequiredClass "SRFetchResult"
    sendOwnedClassMessage cls' newSelector

-- | Retrieves the resultant sample
--
-- The caller is expected to know what the result type should be
--
-- Note: This may thrown an exception if the sample could not be constructed from the data in the datastore
--
-- ObjC selector: @- sample@
sample :: IsSRFetchResult srFetchResult => srFetchResult -> IO RawId
sample srFetchResult =
  sendMessage srFetchResult sampleSelector

-- | the timestamp the sample was written to the data store
--
-- ObjC selector: @- timestamp@
timestamp :: IsSRFetchResult srFetchResult => srFetchResult -> IO CDouble
timestamp srFetchResult =
  sendMessage srFetchResult timestampSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRFetchResult)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRFetchResult)
newSelector = mkSelector "new"

-- | @Selector@ for @sample@
sampleSelector :: Selector '[] RawId
sampleSelector = mkSelector "sample"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] CDouble
timestampSelector = mkSelector "timestamp"

