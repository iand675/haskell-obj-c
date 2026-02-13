{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAsynchronousFetchRequest@.
module ObjC.CoreData.NSAsynchronousFetchRequest
  ( NSAsynchronousFetchRequest
  , IsNSAsynchronousFetchRequest(..)
  , fetchRequest
  , completionBlock
  , estimatedResultCount
  , setEstimatedResultCount
  , completionBlockSelector
  , estimatedResultCountSelector
  , fetchRequestSelector
  , setEstimatedResultCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fetchRequest@
fetchRequest :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> IO (Id NSFetchRequest)
fetchRequest nsAsynchronousFetchRequest =
  sendMessage nsAsynchronousFetchRequest fetchRequestSelector

-- | @- completionBlock@
completionBlock :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> IO (Ptr ())
completionBlock nsAsynchronousFetchRequest =
  sendMessage nsAsynchronousFetchRequest completionBlockSelector

-- | @- estimatedResultCount@
estimatedResultCount :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> IO CLong
estimatedResultCount nsAsynchronousFetchRequest =
  sendMessage nsAsynchronousFetchRequest estimatedResultCountSelector

-- | @- setEstimatedResultCount:@
setEstimatedResultCount :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> CLong -> IO ()
setEstimatedResultCount nsAsynchronousFetchRequest value =
  sendMessage nsAsynchronousFetchRequest setEstimatedResultCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @completionBlock@
completionBlockSelector :: Selector '[] (Ptr ())
completionBlockSelector = mkSelector "completionBlock"

-- | @Selector@ for @estimatedResultCount@
estimatedResultCountSelector :: Selector '[] CLong
estimatedResultCountSelector = mkSelector "estimatedResultCount"

-- | @Selector@ for @setEstimatedResultCount:@
setEstimatedResultCountSelector :: Selector '[CLong] ()
setEstimatedResultCountSelector = mkSelector "setEstimatedResultCount:"

