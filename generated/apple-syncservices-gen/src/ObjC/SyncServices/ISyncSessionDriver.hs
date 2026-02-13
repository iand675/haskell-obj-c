{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncSessionDriver@.
module ObjC.SyncServices.ISyncSessionDriver
  ( ISyncSessionDriver
  , IsISyncSessionDriver(..)
  , sessionDriverWithDataSource
  , sync
  , startAsynchronousSync
  , lastError
  , dataSource
  , setDelegate
  , delegate
  , setHandlesSyncAlerts
  , handlesSyncAlerts
  , client
  , session
  , finishSyncing
  , clientSelector
  , dataSourceSelector
  , delegateSelector
  , finishSyncingSelector
  , handlesSyncAlertsSelector
  , lastErrorSelector
  , sessionDriverWithDataSourceSelector
  , sessionSelector
  , setDelegateSelector
  , setHandlesSyncAlertsSelector
  , startAsynchronousSyncSelector
  , syncSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sessionDriverWithDataSource:@
sessionDriverWithDataSource :: RawId -> IO (Id ISyncSessionDriver)
sessionDriverWithDataSource dataSource =
  do
    cls' <- getRequiredClass "ISyncSessionDriver"
    sendClassMessage cls' sessionDriverWithDataSourceSelector dataSource

-- | @- sync@
sync :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO Bool
sync iSyncSessionDriver =
  sendMessage iSyncSessionDriver syncSelector

-- | @- startAsynchronousSync:@
startAsynchronousSync :: (IsISyncSessionDriver iSyncSessionDriver, IsNSError outError) => iSyncSessionDriver -> outError -> IO Bool
startAsynchronousSync iSyncSessionDriver outError =
  sendMessage iSyncSessionDriver startAsynchronousSyncSelector (toNSError outError)

-- | @- lastError@
lastError :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO (Id NSError)
lastError iSyncSessionDriver =
  sendMessage iSyncSessionDriver lastErrorSelector

-- | @- dataSource@
dataSource :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO RawId
dataSource iSyncSessionDriver =
  sendMessage iSyncSessionDriver dataSourceSelector

-- | @- setDelegate:@
setDelegate :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> RawId -> IO ()
setDelegate iSyncSessionDriver delegate =
  sendMessage iSyncSessionDriver setDelegateSelector delegate

-- | @- delegate@
delegate :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO RawId
delegate iSyncSessionDriver =
  sendMessage iSyncSessionDriver delegateSelector

-- | @- setHandlesSyncAlerts:@
setHandlesSyncAlerts :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> Bool -> IO ()
setHandlesSyncAlerts iSyncSessionDriver yesOrNo =
  sendMessage iSyncSessionDriver setHandlesSyncAlertsSelector yesOrNo

-- | @- handlesSyncAlerts@
handlesSyncAlerts :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO Bool
handlesSyncAlerts iSyncSessionDriver =
  sendMessage iSyncSessionDriver handlesSyncAlertsSelector

-- | @- client@
client :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO (Id ISyncClient)
client iSyncSessionDriver =
  sendMessage iSyncSessionDriver clientSelector

-- | @- session@
session :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO (Id ISyncSession)
session iSyncSessionDriver =
  sendMessage iSyncSessionDriver sessionSelector

-- | @- finishSyncing@
finishSyncing :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO ()
finishSyncing iSyncSessionDriver =
  sendMessage iSyncSessionDriver finishSyncingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionDriverWithDataSource:@
sessionDriverWithDataSourceSelector :: Selector '[RawId] (Id ISyncSessionDriver)
sessionDriverWithDataSourceSelector = mkSelector "sessionDriverWithDataSource:"

-- | @Selector@ for @sync@
syncSelector :: Selector '[] Bool
syncSelector = mkSelector "sync"

-- | @Selector@ for @startAsynchronousSync:@
startAsynchronousSyncSelector :: Selector '[Id NSError] Bool
startAsynchronousSyncSelector = mkSelector "startAsynchronousSync:"

-- | @Selector@ for @lastError@
lastErrorSelector :: Selector '[] (Id NSError)
lastErrorSelector = mkSelector "lastError"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setHandlesSyncAlerts:@
setHandlesSyncAlertsSelector :: Selector '[Bool] ()
setHandlesSyncAlertsSelector = mkSelector "setHandlesSyncAlerts:"

-- | @Selector@ for @handlesSyncAlerts@
handlesSyncAlertsSelector :: Selector '[] Bool
handlesSyncAlertsSelector = mkSelector "handlesSyncAlerts"

-- | @Selector@ for @client@
clientSelector :: Selector '[] (Id ISyncClient)
clientSelector = mkSelector "client"

-- | @Selector@ for @session@
sessionSelector :: Selector '[] (Id ISyncSession)
sessionSelector = mkSelector "session"

-- | @Selector@ for @finishSyncing@
finishSyncingSelector :: Selector '[] ()
finishSyncingSelector = mkSelector "finishSyncing"

