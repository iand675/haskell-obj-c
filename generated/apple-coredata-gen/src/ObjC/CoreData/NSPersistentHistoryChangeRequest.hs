{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentHistoryChangeRequest@.
module ObjC.CoreData.NSPersistentHistoryChangeRequest
  ( NSPersistentHistoryChangeRequest
  , IsNSPersistentHistoryChangeRequest(..)
  , fetchHistoryAfterDate
  , fetchHistoryAfterToken
  , fetchHistoryAfterTransaction
  , fetchHistoryWithFetchRequest
  , deleteHistoryBeforeDate
  , deleteHistoryBeforeToken
  , deleteHistoryBeforeTransaction
  , resultType
  , setResultType
  , token
  , fetchRequest
  , setFetchRequest
  , deleteHistoryBeforeDateSelector
  , deleteHistoryBeforeTokenSelector
  , deleteHistoryBeforeTransactionSelector
  , fetchHistoryAfterDateSelector
  , fetchHistoryAfterTokenSelector
  , fetchHistoryAfterTransactionSelector
  , fetchHistoryWithFetchRequestSelector
  , fetchRequestSelector
  , resultTypeSelector
  , setFetchRequestSelector
  , setResultTypeSelector
  , tokenSelector

  -- * Enum types
  , NSPersistentHistoryResultType(NSPersistentHistoryResultType)
  , pattern NSPersistentHistoryResultTypeStatusOnly
  , pattern NSPersistentHistoryResultTypeObjectIDs
  , pattern NSPersistentHistoryResultTypeCount
  , pattern NSPersistentHistoryResultTypeTransactionsOnly
  , pattern NSPersistentHistoryResultTypeChangesOnly
  , pattern NSPersistentHistoryResultTypeTransactionsAndChanges

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

-- | @+ fetchHistoryAfterDate:@
fetchHistoryAfterDate :: IsNSDate date => date -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterDate date =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' fetchHistoryAfterDateSelector (toNSDate date)

-- | @+ fetchHistoryAfterToken:@
fetchHistoryAfterToken :: IsNSPersistentHistoryToken token => token -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterToken token =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' fetchHistoryAfterTokenSelector (toNSPersistentHistoryToken token)

-- | @+ fetchHistoryAfterTransaction:@
fetchHistoryAfterTransaction :: IsNSPersistentHistoryTransaction transaction => transaction -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterTransaction transaction =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' fetchHistoryAfterTransactionSelector (toNSPersistentHistoryTransaction transaction)

-- | @+ fetchHistoryWithFetchRequest:@
fetchHistoryWithFetchRequest :: IsNSFetchRequest fetchRequest => fetchRequest -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryWithFetchRequest fetchRequest =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' fetchHistoryWithFetchRequestSelector (toNSFetchRequest fetchRequest)

-- | @+ deleteHistoryBeforeDate:@
deleteHistoryBeforeDate :: IsNSDate date => date -> IO (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeDate date =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' deleteHistoryBeforeDateSelector (toNSDate date)

-- | @+ deleteHistoryBeforeToken:@
deleteHistoryBeforeToken :: IsNSPersistentHistoryToken token => token -> IO (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeToken token =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' deleteHistoryBeforeTokenSelector (toNSPersistentHistoryToken token)

-- | @+ deleteHistoryBeforeTransaction:@
deleteHistoryBeforeTransaction :: IsNSPersistentHistoryTransaction transaction => transaction -> IO (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeTransaction transaction =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    sendClassMessage cls' deleteHistoryBeforeTransactionSelector (toNSPersistentHistoryTransaction transaction)

-- | @- resultType@
resultType :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> IO NSPersistentHistoryResultType
resultType nsPersistentHistoryChangeRequest =
  sendMessage nsPersistentHistoryChangeRequest resultTypeSelector

-- | @- setResultType:@
setResultType :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> NSPersistentHistoryResultType -> IO ()
setResultType nsPersistentHistoryChangeRequest value =
  sendMessage nsPersistentHistoryChangeRequest setResultTypeSelector value

-- | @- token@
token :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> IO (Id NSPersistentHistoryToken)
token nsPersistentHistoryChangeRequest =
  sendMessage nsPersistentHistoryChangeRequest tokenSelector

-- | @- fetchRequest@
fetchRequest :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> IO (Id NSFetchRequest)
fetchRequest nsPersistentHistoryChangeRequest =
  sendMessage nsPersistentHistoryChangeRequest fetchRequestSelector

-- | @- setFetchRequest:@
setFetchRequest :: (IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest, IsNSFetchRequest value) => nsPersistentHistoryChangeRequest -> value -> IO ()
setFetchRequest nsPersistentHistoryChangeRequest value =
  sendMessage nsPersistentHistoryChangeRequest setFetchRequestSelector (toNSFetchRequest value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchHistoryAfterDate:@
fetchHistoryAfterDateSelector :: Selector '[Id NSDate] (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterDateSelector = mkSelector "fetchHistoryAfterDate:"

-- | @Selector@ for @fetchHistoryAfterToken:@
fetchHistoryAfterTokenSelector :: Selector '[Id NSPersistentHistoryToken] (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterTokenSelector = mkSelector "fetchHistoryAfterToken:"

-- | @Selector@ for @fetchHistoryAfterTransaction:@
fetchHistoryAfterTransactionSelector :: Selector '[Id NSPersistentHistoryTransaction] (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterTransactionSelector = mkSelector "fetchHistoryAfterTransaction:"

-- | @Selector@ for @fetchHistoryWithFetchRequest:@
fetchHistoryWithFetchRequestSelector :: Selector '[Id NSFetchRequest] (Id NSPersistentHistoryChangeRequest)
fetchHistoryWithFetchRequestSelector = mkSelector "fetchHistoryWithFetchRequest:"

-- | @Selector@ for @deleteHistoryBeforeDate:@
deleteHistoryBeforeDateSelector :: Selector '[Id NSDate] (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeDateSelector = mkSelector "deleteHistoryBeforeDate:"

-- | @Selector@ for @deleteHistoryBeforeToken:@
deleteHistoryBeforeTokenSelector :: Selector '[Id NSPersistentHistoryToken] (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeTokenSelector = mkSelector "deleteHistoryBeforeToken:"

-- | @Selector@ for @deleteHistoryBeforeTransaction:@
deleteHistoryBeforeTransactionSelector :: Selector '[Id NSPersistentHistoryTransaction] (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeTransactionSelector = mkSelector "deleteHistoryBeforeTransaction:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSPersistentHistoryResultType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector '[NSPersistentHistoryResultType] ()
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id NSPersistentHistoryToken)
tokenSelector = mkSelector "token"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @setFetchRequest:@
setFetchRequestSelector :: Selector '[Id NSFetchRequest] ()
setFetchRequestSelector = mkSelector "setFetchRequest:"

