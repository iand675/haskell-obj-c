{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentHistoryTransaction@.
module ObjC.CoreData.NSPersistentHistoryTransaction
  ( NSPersistentHistoryTransaction
  , IsNSPersistentHistoryTransaction(..)
  , entityDescriptionWithContext
  , objectIDNotification
  , entityDescription
  , fetchRequest
  , timestamp
  , changes
  , transactionNumber
  , storeID
  , bundleID
  , processID
  , contextName
  , author
  , token
  , authorSelector
  , bundleIDSelector
  , changesSelector
  , contextNameSelector
  , entityDescriptionSelector
  , entityDescriptionWithContextSelector
  , fetchRequestSelector
  , objectIDNotificationSelector
  , processIDSelector
  , storeIDSelector
  , timestampSelector
  , tokenSelector
  , transactionNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ entityDescriptionWithContext:@
entityDescriptionWithContext :: IsNSManagedObjectContext context => context -> IO (Id NSEntityDescription)
entityDescriptionWithContext context =
  do
    cls' <- getRequiredClass "NSPersistentHistoryTransaction"
    sendClassMessage cls' entityDescriptionWithContextSelector (toNSManagedObjectContext context)

-- | @- objectIDNotification@
objectIDNotification :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSNotification)
objectIDNotification nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction objectIDNotificationSelector

-- | @+ entityDescription@
entityDescription :: IO (Id NSEntityDescription)
entityDescription  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryTransaction"
    sendClassMessage cls' entityDescriptionSelector

-- | @+ fetchRequest@
fetchRequest :: IO (Id NSFetchRequest)
fetchRequest  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryTransaction"
    sendClassMessage cls' fetchRequestSelector

-- | @- timestamp@
timestamp :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSDate)
timestamp nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction timestampSelector

-- | @- changes@
changes :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSArray)
changes nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction changesSelector

-- | @- transactionNumber@
transactionNumber :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO CLong
transactionNumber nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction transactionNumberSelector

-- | @- storeID@
storeID :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
storeID nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction storeIDSelector

-- | @- bundleID@
bundleID :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
bundleID nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction bundleIDSelector

-- | @- processID@
processID :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
processID nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction processIDSelector

-- | @- contextName@
contextName :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
contextName nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction contextNameSelector

-- | @- author@
author :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
author nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction authorSelector

-- | @- token@
token :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSPersistentHistoryToken)
token nsPersistentHistoryTransaction =
  sendMessage nsPersistentHistoryTransaction tokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entityDescriptionWithContext:@
entityDescriptionWithContextSelector :: Selector '[Id NSManagedObjectContext] (Id NSEntityDescription)
entityDescriptionWithContextSelector = mkSelector "entityDescriptionWithContext:"

-- | @Selector@ for @objectIDNotification@
objectIDNotificationSelector :: Selector '[] (Id NSNotification)
objectIDNotificationSelector = mkSelector "objectIDNotification"

-- | @Selector@ for @entityDescription@
entityDescriptionSelector :: Selector '[] (Id NSEntityDescription)
entityDescriptionSelector = mkSelector "entityDescription"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSDate)
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @changes@
changesSelector :: Selector '[] (Id NSArray)
changesSelector = mkSelector "changes"

-- | @Selector@ for @transactionNumber@
transactionNumberSelector :: Selector '[] CLong
transactionNumberSelector = mkSelector "transactionNumber"

-- | @Selector@ for @storeID@
storeIDSelector :: Selector '[] (Id NSString)
storeIDSelector = mkSelector "storeID"

-- | @Selector@ for @bundleID@
bundleIDSelector :: Selector '[] (Id NSString)
bundleIDSelector = mkSelector "bundleID"

-- | @Selector@ for @processID@
processIDSelector :: Selector '[] (Id NSString)
processIDSelector = mkSelector "processID"

-- | @Selector@ for @contextName@
contextNameSelector :: Selector '[] (Id NSString)
contextNameSelector = mkSelector "contextName"

-- | @Selector@ for @author@
authorSelector :: Selector '[] (Id NSString)
authorSelector = mkSelector "author"

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id NSPersistentHistoryToken)
tokenSelector = mkSelector "token"

