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
  , entityDescriptionWithContextSelector
  , objectIDNotificationSelector
  , entityDescriptionSelector
  , fetchRequestSelector
  , timestampSelector
  , changesSelector
  , transactionNumberSelector
  , storeIDSelector
  , bundleIDSelector
  , processIDSelector
  , contextNameSelector
  , authorSelector
  , tokenSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ entityDescriptionWithContext:@
entityDescriptionWithContext :: IsNSManagedObjectContext context => context -> IO (Id NSEntityDescription)
entityDescriptionWithContext context =
  do
    cls' <- getRequiredClass "NSPersistentHistoryTransaction"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "entityDescriptionWithContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectIDNotification@
objectIDNotification :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSNotification)
objectIDNotification nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "objectIDNotification") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ entityDescription@
entityDescription :: IO (Id NSEntityDescription)
entityDescription  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryTransaction"
    sendClassMsg cls' (mkSelector "entityDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fetchRequest@
fetchRequest :: IO (Id NSFetchRequest)
fetchRequest  =
  do
    cls' <- getRequiredClass "NSPersistentHistoryTransaction"
    sendClassMsg cls' (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timestamp@
timestamp :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSDate)
timestamp nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changes@
changes :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSArray)
changes nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "changes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionNumber@
transactionNumber :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO CLong
transactionNumber nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "transactionNumber") retCLong []

-- | @- storeID@
storeID :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
storeID nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "storeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bundleID@
bundleID :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
bundleID nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "bundleID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- processID@
processID :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
processID nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "processID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contextName@
contextName :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
contextName nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "contextName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- author@
author :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSString)
author nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "author") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- token@
token :: IsNSPersistentHistoryTransaction nsPersistentHistoryTransaction => nsPersistentHistoryTransaction -> IO (Id NSPersistentHistoryToken)
token nsPersistentHistoryTransaction  =
  sendMsg nsPersistentHistoryTransaction (mkSelector "token") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entityDescriptionWithContext:@
entityDescriptionWithContextSelector :: Selector
entityDescriptionWithContextSelector = mkSelector "entityDescriptionWithContext:"

-- | @Selector@ for @objectIDNotification@
objectIDNotificationSelector :: Selector
objectIDNotificationSelector = mkSelector "objectIDNotification"

-- | @Selector@ for @entityDescription@
entityDescriptionSelector :: Selector
entityDescriptionSelector = mkSelector "entityDescription"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @changes@
changesSelector :: Selector
changesSelector = mkSelector "changes"

-- | @Selector@ for @transactionNumber@
transactionNumberSelector :: Selector
transactionNumberSelector = mkSelector "transactionNumber"

-- | @Selector@ for @storeID@
storeIDSelector :: Selector
storeIDSelector = mkSelector "storeID"

-- | @Selector@ for @bundleID@
bundleIDSelector :: Selector
bundleIDSelector = mkSelector "bundleID"

-- | @Selector@ for @processID@
processIDSelector :: Selector
processIDSelector = mkSelector "processID"

-- | @Selector@ for @contextName@
contextNameSelector :: Selector
contextNameSelector = mkSelector "contextName"

-- | @Selector@ for @author@
authorSelector :: Selector
authorSelector = mkSelector "author"

-- | @Selector@ for @token@
tokenSelector :: Selector
tokenSelector = mkSelector "token"

