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
  , fetchRequestSelector
  , completionBlockSelector
  , estimatedResultCountSelector
  , setEstimatedResultCountSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fetchRequest@
fetchRequest :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> IO (Id NSFetchRequest)
fetchRequest nsAsynchronousFetchRequest  =
  sendMsg nsAsynchronousFetchRequest (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- completionBlock@
completionBlock :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> IO (Ptr ())
completionBlock nsAsynchronousFetchRequest  =
  fmap castPtr $ sendMsg nsAsynchronousFetchRequest (mkSelector "completionBlock") (retPtr retVoid) []

-- | @- estimatedResultCount@
estimatedResultCount :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> IO CLong
estimatedResultCount nsAsynchronousFetchRequest  =
  sendMsg nsAsynchronousFetchRequest (mkSelector "estimatedResultCount") retCLong []

-- | @- setEstimatedResultCount:@
setEstimatedResultCount :: IsNSAsynchronousFetchRequest nsAsynchronousFetchRequest => nsAsynchronousFetchRequest -> CLong -> IO ()
setEstimatedResultCount nsAsynchronousFetchRequest  value =
  sendMsg nsAsynchronousFetchRequest (mkSelector "setEstimatedResultCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @completionBlock@
completionBlockSelector :: Selector
completionBlockSelector = mkSelector "completionBlock"

-- | @Selector@ for @estimatedResultCount@
estimatedResultCountSelector :: Selector
estimatedResultCountSelector = mkSelector "estimatedResultCount"

-- | @Selector@ for @setEstimatedResultCount:@
setEstimatedResultCountSelector :: Selector
setEstimatedResultCountSelector = mkSelector "setEstimatedResultCount:"

