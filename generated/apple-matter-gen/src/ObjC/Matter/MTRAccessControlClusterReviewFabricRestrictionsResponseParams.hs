{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterReviewFabricRestrictionsResponseParams@.
module ObjC.Matter.MTRAccessControlClusterReviewFabricRestrictionsResponseParams
  ( MTRAccessControlClusterReviewFabricRestrictionsResponseParams
  , IsMTRAccessControlClusterReviewFabricRestrictionsResponseParams(..)
  , initWithResponseValue_error
  , token
  , setToken
  , initWithResponseValue_errorSelector
  , setTokenSelector
  , tokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRAccessControlClusterReviewFabricRestrictionsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRAccessControlClusterReviewFabricRestrictionsResponseParams mtrAccessControlClusterReviewFabricRestrictionsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrAccessControlClusterReviewFabricRestrictionsResponseParams -> responseValue -> error_ -> IO (Id MTRAccessControlClusterReviewFabricRestrictionsResponseParams)
initWithResponseValue_error mtrAccessControlClusterReviewFabricRestrictionsResponseParams responseValue error_ =
  sendOwnedMessage mtrAccessControlClusterReviewFabricRestrictionsResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- token@
token :: IsMTRAccessControlClusterReviewFabricRestrictionsResponseParams mtrAccessControlClusterReviewFabricRestrictionsResponseParams => mtrAccessControlClusterReviewFabricRestrictionsResponseParams -> IO (Id NSNumber)
token mtrAccessControlClusterReviewFabricRestrictionsResponseParams =
  sendMessage mtrAccessControlClusterReviewFabricRestrictionsResponseParams tokenSelector

-- | @- setToken:@
setToken :: (IsMTRAccessControlClusterReviewFabricRestrictionsResponseParams mtrAccessControlClusterReviewFabricRestrictionsResponseParams, IsNSNumber value) => mtrAccessControlClusterReviewFabricRestrictionsResponseParams -> value -> IO ()
setToken mtrAccessControlClusterReviewFabricRestrictionsResponseParams value =
  sendMessage mtrAccessControlClusterReviewFabricRestrictionsResponseParams setTokenSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRAccessControlClusterReviewFabricRestrictionsResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id NSNumber)
tokenSelector = mkSelector "token"

-- | @Selector@ for @setToken:@
setTokenSelector :: Selector '[Id NSNumber] ()
setTokenSelector = mkSelector "setToken:"

