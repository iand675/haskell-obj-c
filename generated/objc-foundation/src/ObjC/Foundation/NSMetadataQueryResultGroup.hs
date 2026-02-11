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
  , resultAtIndexSelector
  , attributeSelector
  , valueSelector
  , subgroupsSelector
  , resultCountSelector
  , resultsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- resultAtIndex:@
resultAtIndex :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> CULong -> IO RawId
resultAtIndex nsMetadataQueryResultGroup  idx =
  fmap (RawId . castPtr) $ sendMsg nsMetadataQueryResultGroup (mkSelector "resultAtIndex:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- attribute@
attribute :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO (Id NSString)
attribute nsMetadataQueryResultGroup  =
  sendMsg nsMetadataQueryResultGroup (mkSelector "attribute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO RawId
value nsMetadataQueryResultGroup  =
  fmap (RawId . castPtr) $ sendMsg nsMetadataQueryResultGroup (mkSelector "value") (retPtr retVoid) []

-- | @- subgroups@
subgroups :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO (Id NSArray)
subgroups nsMetadataQueryResultGroup  =
  sendMsg nsMetadataQueryResultGroup (mkSelector "subgroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resultCount@
resultCount :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO CULong
resultCount nsMetadataQueryResultGroup  =
  sendMsg nsMetadataQueryResultGroup (mkSelector "resultCount") retCULong []

-- | @- results@
results :: IsNSMetadataQueryResultGroup nsMetadataQueryResultGroup => nsMetadataQueryResultGroup -> IO (Id NSArray)
results nsMetadataQueryResultGroup  =
  sendMsg nsMetadataQueryResultGroup (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resultAtIndex:@
resultAtIndexSelector :: Selector
resultAtIndexSelector = mkSelector "resultAtIndex:"

-- | @Selector@ for @attribute@
attributeSelector :: Selector
attributeSelector = mkSelector "attribute"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @subgroups@
subgroupsSelector :: Selector
subgroupsSelector = mkSelector "subgroups"

-- | @Selector@ for @resultCount@
resultCountSelector :: Selector
resultCountSelector = mkSelector "resultCount"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

