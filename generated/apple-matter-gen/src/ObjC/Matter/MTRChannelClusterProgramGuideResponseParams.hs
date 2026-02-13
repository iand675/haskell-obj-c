{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramGuideResponseParams@.
module ObjC.Matter.MTRChannelClusterProgramGuideResponseParams
  ( MTRChannelClusterProgramGuideResponseParams
  , IsMTRChannelClusterProgramGuideResponseParams(..)
  , initWithResponseValue_error
  , paging
  , setPaging
  , programList
  , setProgramList
  , initWithResponseValue_errorSelector
  , pagingSelector
  , programListSelector
  , setPagingSelector
  , setProgramListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRChannelClusterProgramGuideResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrChannelClusterProgramGuideResponseParams -> responseValue -> error_ -> IO (Id MTRChannelClusterProgramGuideResponseParams)
initWithResponseValue_error mtrChannelClusterProgramGuideResponseParams responseValue error_ =
  sendOwnedMessage mtrChannelClusterProgramGuideResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- paging@
paging :: IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams => mtrChannelClusterProgramGuideResponseParams -> IO (Id MTRChannelClusterChannelPagingStruct)
paging mtrChannelClusterProgramGuideResponseParams =
  sendMessage mtrChannelClusterProgramGuideResponseParams pagingSelector

-- | @- setPaging:@
setPaging :: (IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams, IsMTRChannelClusterChannelPagingStruct value) => mtrChannelClusterProgramGuideResponseParams -> value -> IO ()
setPaging mtrChannelClusterProgramGuideResponseParams value =
  sendMessage mtrChannelClusterProgramGuideResponseParams setPagingSelector (toMTRChannelClusterChannelPagingStruct value)

-- | @- programList@
programList :: IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams => mtrChannelClusterProgramGuideResponseParams -> IO (Id NSArray)
programList mtrChannelClusterProgramGuideResponseParams =
  sendMessage mtrChannelClusterProgramGuideResponseParams programListSelector

-- | @- setProgramList:@
setProgramList :: (IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams, IsNSArray value) => mtrChannelClusterProgramGuideResponseParams -> value -> IO ()
setProgramList mtrChannelClusterProgramGuideResponseParams value =
  sendMessage mtrChannelClusterProgramGuideResponseParams setProgramListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRChannelClusterProgramGuideResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @paging@
pagingSelector :: Selector '[] (Id MTRChannelClusterChannelPagingStruct)
pagingSelector = mkSelector "paging"

-- | @Selector@ for @setPaging:@
setPagingSelector :: Selector '[Id MTRChannelClusterChannelPagingStruct] ()
setPagingSelector = mkSelector "setPaging:"

-- | @Selector@ for @programList@
programListSelector :: Selector '[] (Id NSArray)
programListSelector = mkSelector "programList"

-- | @Selector@ for @setProgramList:@
setProgramListSelector :: Selector '[Id NSArray] ()
setProgramListSelector = mkSelector "setProgramList:"

