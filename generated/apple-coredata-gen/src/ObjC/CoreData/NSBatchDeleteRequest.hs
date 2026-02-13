{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchDeleteRequest@.
module ObjC.CoreData.NSBatchDeleteRequest
  ( NSBatchDeleteRequest
  , IsNSBatchDeleteRequest(..)
  , init_
  , initWithFetchRequest
  , initWithObjectIDs
  , resultType
  , setResultType
  , fetchRequest
  , fetchRequestSelector
  , initSelector
  , initWithFetchRequestSelector
  , initWithObjectIDsSelector
  , resultTypeSelector
  , setResultTypeSelector

  -- * Enum types
  , NSBatchDeleteRequestResultType(NSBatchDeleteRequestResultType)
  , pattern NSBatchDeleteResultTypeStatusOnly
  , pattern NSBatchDeleteResultTypeObjectIDs
  , pattern NSBatchDeleteResultTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> IO (Id NSBatchDeleteRequest)
init_ nsBatchDeleteRequest =
  sendOwnedMessage nsBatchDeleteRequest initSelector

-- | @- initWithFetchRequest:@
initWithFetchRequest :: (IsNSBatchDeleteRequest nsBatchDeleteRequest, IsNSFetchRequest fetch) => nsBatchDeleteRequest -> fetch -> IO (Id NSBatchDeleteRequest)
initWithFetchRequest nsBatchDeleteRequest fetch =
  sendOwnedMessage nsBatchDeleteRequest initWithFetchRequestSelector (toNSFetchRequest fetch)

-- | @- initWithObjectIDs:@
initWithObjectIDs :: (IsNSBatchDeleteRequest nsBatchDeleteRequest, IsNSArray objects) => nsBatchDeleteRequest -> objects -> IO (Id NSBatchDeleteRequest)
initWithObjectIDs nsBatchDeleteRequest objects =
  sendOwnedMessage nsBatchDeleteRequest initWithObjectIDsSelector (toNSArray objects)

-- | @- resultType@
resultType :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> IO NSBatchDeleteRequestResultType
resultType nsBatchDeleteRequest =
  sendMessage nsBatchDeleteRequest resultTypeSelector

-- | @- setResultType:@
setResultType :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> NSBatchDeleteRequestResultType -> IO ()
setResultType nsBatchDeleteRequest value =
  sendMessage nsBatchDeleteRequest setResultTypeSelector value

-- | @- fetchRequest@
fetchRequest :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> IO (Id NSFetchRequest)
fetchRequest nsBatchDeleteRequest =
  sendMessage nsBatchDeleteRequest fetchRequestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSBatchDeleteRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFetchRequest:@
initWithFetchRequestSelector :: Selector '[Id NSFetchRequest] (Id NSBatchDeleteRequest)
initWithFetchRequestSelector = mkSelector "initWithFetchRequest:"

-- | @Selector@ for @initWithObjectIDs:@
initWithObjectIDsSelector :: Selector '[Id NSArray] (Id NSBatchDeleteRequest)
initWithObjectIDsSelector = mkSelector "initWithObjectIDs:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSBatchDeleteRequestResultType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector '[NSBatchDeleteRequestResultType] ()
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

