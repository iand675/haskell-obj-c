{-# LANGUAGE PatternSynonyms #-}
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
  , fetchHistoryAfterDateSelector
  , fetchHistoryAfterTokenSelector
  , fetchHistoryAfterTransactionSelector
  , fetchHistoryWithFetchRequestSelector
  , deleteHistoryBeforeDateSelector
  , deleteHistoryBeforeTokenSelector
  , deleteHistoryBeforeTransactionSelector
  , resultTypeSelector
  , setResultTypeSelector
  , tokenSelector
  , fetchRequestSelector
  , setFetchRequestSelector

  -- * Enum types
  , NSPersistentHistoryResultType(NSPersistentHistoryResultType)
  , pattern NSPersistentHistoryResultTypeStatusOnly
  , pattern NSPersistentHistoryResultTypeObjectIDs
  , pattern NSPersistentHistoryResultTypeCount
  , pattern NSPersistentHistoryResultTypeTransactionsOnly
  , pattern NSPersistentHistoryResultTypeChangesOnly
  , pattern NSPersistentHistoryResultTypeTransactionsAndChanges

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
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ fetchHistoryAfterDate:@
fetchHistoryAfterDate :: IsNSDate date => date -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterDate date =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "fetchHistoryAfterDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchHistoryAfterToken:@
fetchHistoryAfterToken :: IsNSPersistentHistoryToken token => token -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterToken token =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr token $ \raw_token ->
      sendClassMsg cls' (mkSelector "fetchHistoryAfterToken:") (retPtr retVoid) [argPtr (castPtr raw_token :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchHistoryAfterTransaction:@
fetchHistoryAfterTransaction :: IsNSPersistentHistoryTransaction transaction => transaction -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryAfterTransaction transaction =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr transaction $ \raw_transaction ->
      sendClassMsg cls' (mkSelector "fetchHistoryAfterTransaction:") (retPtr retVoid) [argPtr (castPtr raw_transaction :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchHistoryWithFetchRequest:@
fetchHistoryWithFetchRequest :: IsNSFetchRequest fetchRequest => fetchRequest -> IO (Id NSPersistentHistoryChangeRequest)
fetchHistoryWithFetchRequest fetchRequest =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr fetchRequest $ \raw_fetchRequest ->
      sendClassMsg cls' (mkSelector "fetchHistoryWithFetchRequest:") (retPtr retVoid) [argPtr (castPtr raw_fetchRequest :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteHistoryBeforeDate:@
deleteHistoryBeforeDate :: IsNSDate date => date -> IO (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeDate date =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "deleteHistoryBeforeDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteHistoryBeforeToken:@
deleteHistoryBeforeToken :: IsNSPersistentHistoryToken token => token -> IO (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeToken token =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr token $ \raw_token ->
      sendClassMsg cls' (mkSelector "deleteHistoryBeforeToken:") (retPtr retVoid) [argPtr (castPtr raw_token :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteHistoryBeforeTransaction:@
deleteHistoryBeforeTransaction :: IsNSPersistentHistoryTransaction transaction => transaction -> IO (Id NSPersistentHistoryChangeRequest)
deleteHistoryBeforeTransaction transaction =
  do
    cls' <- getRequiredClass "NSPersistentHistoryChangeRequest"
    withObjCPtr transaction $ \raw_transaction ->
      sendClassMsg cls' (mkSelector "deleteHistoryBeforeTransaction:") (retPtr retVoid) [argPtr (castPtr raw_transaction :: Ptr ())] >>= retainedObject . castPtr

-- | @- resultType@
resultType :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> IO NSPersistentHistoryResultType
resultType nsPersistentHistoryChangeRequest  =
  fmap (coerce :: CLong -> NSPersistentHistoryResultType) $ sendMsg nsPersistentHistoryChangeRequest (mkSelector "resultType") retCLong []

-- | @- setResultType:@
setResultType :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> NSPersistentHistoryResultType -> IO ()
setResultType nsPersistentHistoryChangeRequest  value =
  sendMsg nsPersistentHistoryChangeRequest (mkSelector "setResultType:") retVoid [argCLong (coerce value)]

-- | @- token@
token :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> IO (Id NSPersistentHistoryToken)
token nsPersistentHistoryChangeRequest  =
  sendMsg nsPersistentHistoryChangeRequest (mkSelector "token") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchRequest@
fetchRequest :: IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest => nsPersistentHistoryChangeRequest -> IO (Id NSFetchRequest)
fetchRequest nsPersistentHistoryChangeRequest  =
  sendMsg nsPersistentHistoryChangeRequest (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFetchRequest:@
setFetchRequest :: (IsNSPersistentHistoryChangeRequest nsPersistentHistoryChangeRequest, IsNSFetchRequest value) => nsPersistentHistoryChangeRequest -> value -> IO ()
setFetchRequest nsPersistentHistoryChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentHistoryChangeRequest (mkSelector "setFetchRequest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchHistoryAfterDate:@
fetchHistoryAfterDateSelector :: Selector
fetchHistoryAfterDateSelector = mkSelector "fetchHistoryAfterDate:"

-- | @Selector@ for @fetchHistoryAfterToken:@
fetchHistoryAfterTokenSelector :: Selector
fetchHistoryAfterTokenSelector = mkSelector "fetchHistoryAfterToken:"

-- | @Selector@ for @fetchHistoryAfterTransaction:@
fetchHistoryAfterTransactionSelector :: Selector
fetchHistoryAfterTransactionSelector = mkSelector "fetchHistoryAfterTransaction:"

-- | @Selector@ for @fetchHistoryWithFetchRequest:@
fetchHistoryWithFetchRequestSelector :: Selector
fetchHistoryWithFetchRequestSelector = mkSelector "fetchHistoryWithFetchRequest:"

-- | @Selector@ for @deleteHistoryBeforeDate:@
deleteHistoryBeforeDateSelector :: Selector
deleteHistoryBeforeDateSelector = mkSelector "deleteHistoryBeforeDate:"

-- | @Selector@ for @deleteHistoryBeforeToken:@
deleteHistoryBeforeTokenSelector :: Selector
deleteHistoryBeforeTokenSelector = mkSelector "deleteHistoryBeforeToken:"

-- | @Selector@ for @deleteHistoryBeforeTransaction:@
deleteHistoryBeforeTransactionSelector :: Selector
deleteHistoryBeforeTransactionSelector = mkSelector "deleteHistoryBeforeTransaction:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @token@
tokenSelector :: Selector
tokenSelector = mkSelector "token"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @setFetchRequest:@
setFetchRequestSelector :: Selector
setFetchRequestSelector = mkSelector "setFetchRequest:"

