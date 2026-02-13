{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMetadataQueryResultGroup@.
module ObjC.Foundation.NSMetadataQueryResultGroup
  ( NSMetadataQueryResultGroup
  , IsNSMetadataQueryResultGroup(..)
  , resultAtIndex
  , attribute
  , value
  , subgroups
  , resultCount
  , results
  , attributeSelector
  , resultAtIndexSelector
  , resultCountSelector
  , resultsSelector
  , subgroupsSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- resultAtIndex:@
resultAtIndex :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> CULong -> IO RawId
resultAtIndex nsMetadataQueryResultGroup idx =
  sendMessage nsMetadataQueryResultGroup resultAtIndexSelector idx

-- | @- attribute@
attribute :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO (Id NSString)
attribute nsMetadataQueryResultGroup =
  sendMessage nsMetadataQueryResultGroup attributeSelector

-- | @- value@
value :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO RawId
value nsMetadataQueryResultGroup =
  sendMessage nsMetadataQueryResultGroup valueSelector

-- | @- subgroups@
subgroups :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO (Id NSArray)
subgroups nsMetadataQueryResultGroup =
  sendMessage nsMetadataQueryResultGroup subgroupsSelector

-- | @- resultCount@
resultCount :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO CULong
resultCount nsMetadataQueryResultGroup =
  sendMessage nsMetadataQueryResultGroup resultCountSelector

-- | @- results@
results :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO (Id NSArray)
results nsMetadataQueryResultGroup =
  sendMessage nsMetadataQueryResultGroup resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resultAtIndex:@
resultAtIndexSelector :: Selector '[CULong] RawId
resultAtIndexSelector = mkSelector "resultAtIndex:"

-- | @Selector@ for @attribute@
attributeSelector :: Selector '[] (Id NSString)
attributeSelector = mkSelector "attribute"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @subgroups@
subgroupsSelector :: Selector '[] (Id NSArray)
subgroupsSelector = mkSelector "subgroups"

-- | @Selector@ for @resultCount@
resultCountSelector :: Selector '[] CULong
resultCountSelector = mkSelector "resultCount"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

