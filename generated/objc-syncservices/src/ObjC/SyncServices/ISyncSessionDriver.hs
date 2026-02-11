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
  , sessionDriverWithDataSourceSelector
  , syncSelector
  , startAsynchronousSyncSelector
  , lastErrorSelector
  , dataSourceSelector
  , setDelegateSelector
  , delegateSelector
  , setHandlesSyncAlertsSelector
  , handlesSyncAlertsSelector
  , clientSelector
  , sessionSelector
  , finishSyncingSelector


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

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sessionDriverWithDataSource:@
sessionDriverWithDataSource :: RawId -> IO (Id ISyncSessionDriver)
sessionDriverWithDataSource dataSource =
  do
    cls' <- getRequiredClass "ISyncSessionDriver"
    sendClassMsg cls' (mkSelector "sessionDriverWithDataSource:") (retPtr retVoid) [argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= retainedObject . castPtr

-- | @- sync@
sync :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO Bool
sync iSyncSessionDriver  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSessionDriver (mkSelector "sync") retCULong []

-- | @- startAsynchronousSync:@
startAsynchronousSync :: (IsISyncSessionDriver iSyncSessionDriver, IsNSError outError) => iSyncSessionDriver -> outError -> IO Bool
startAsynchronousSync iSyncSessionDriver  outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSessionDriver (mkSelector "startAsynchronousSync:") retCULong [argPtr (castPtr raw_outError :: Ptr ())]

-- | @- lastError@
lastError :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO (Id NSError)
lastError iSyncSessionDriver  =
  sendMsg iSyncSessionDriver (mkSelector "lastError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dataSource@
dataSource :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO RawId
dataSource iSyncSessionDriver  =
  fmap (RawId . castPtr) $ sendMsg iSyncSessionDriver (mkSelector "dataSource") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> RawId -> IO ()
setDelegate iSyncSessionDriver  delegate =
  sendMsg iSyncSessionDriver (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | @- delegate@
delegate :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO RawId
delegate iSyncSessionDriver  =
  fmap (RawId . castPtr) $ sendMsg iSyncSessionDriver (mkSelector "delegate") (retPtr retVoid) []

-- | @- setHandlesSyncAlerts:@
setHandlesSyncAlerts :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> Bool -> IO ()
setHandlesSyncAlerts iSyncSessionDriver  yesOrNo =
  sendMsg iSyncSessionDriver (mkSelector "setHandlesSyncAlerts:") retVoid [argCULong (if yesOrNo then 1 else 0)]

-- | @- handlesSyncAlerts@
handlesSyncAlerts :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO Bool
handlesSyncAlerts iSyncSessionDriver  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSessionDriver (mkSelector "handlesSyncAlerts") retCULong []

-- | @- client@
client :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO (Id ISyncClient)
client iSyncSessionDriver  =
  sendMsg iSyncSessionDriver (mkSelector "client") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- session@
session :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO (Id ISyncSession)
session iSyncSessionDriver  =
  sendMsg iSyncSessionDriver (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- finishSyncing@
finishSyncing :: IsISyncSessionDriver iSyncSessionDriver => iSyncSessionDriver -> IO ()
finishSyncing iSyncSessionDriver  =
  sendMsg iSyncSessionDriver (mkSelector "finishSyncing") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionDriverWithDataSource:@
sessionDriverWithDataSourceSelector :: Selector
sessionDriverWithDataSourceSelector = mkSelector "sessionDriverWithDataSource:"

-- | @Selector@ for @sync@
syncSelector :: Selector
syncSelector = mkSelector "sync"

-- | @Selector@ for @startAsynchronousSync:@
startAsynchronousSyncSelector :: Selector
startAsynchronousSyncSelector = mkSelector "startAsynchronousSync:"

-- | @Selector@ for @lastError@
lastErrorSelector :: Selector
lastErrorSelector = mkSelector "lastError"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setHandlesSyncAlerts:@
setHandlesSyncAlertsSelector :: Selector
setHandlesSyncAlertsSelector = mkSelector "setHandlesSyncAlerts:"

-- | @Selector@ for @handlesSyncAlerts@
handlesSyncAlertsSelector :: Selector
handlesSyncAlertsSelector = mkSelector "handlesSyncAlerts"

-- | @Selector@ for @client@
clientSelector :: Selector
clientSelector = mkSelector "client"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @finishSyncing@
finishSyncingSelector :: Selector
finishSyncingSelector = mkSelector "finishSyncing"

