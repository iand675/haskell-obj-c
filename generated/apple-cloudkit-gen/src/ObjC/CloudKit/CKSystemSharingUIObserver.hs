{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSystemSharingUIObserver@.
module ObjC.CloudKit.CKSystemSharingUIObserver
  ( CKSystemSharingUIObserver
  , IsCKSystemSharingUIObserver(..)
  , init_
  , new
  , initWithContainer
  , systemSharingUIDidSaveShareBlock
  , setSystemSharingUIDidSaveShareBlock
  , systemSharingUIDidStopSharingBlock
  , setSystemSharingUIDidStopSharingBlock
  , initSelector
  , initWithContainerSelector
  , newSelector
  , setSystemSharingUIDidSaveShareBlockSelector
  , setSystemSharingUIDidStopSharingBlockSelector
  , systemSharingUIDidSaveShareBlockSelector
  , systemSharingUIDidStopSharingBlockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> IO (Id CKSystemSharingUIObserver)
init_ ckSystemSharingUIObserver =
  sendOwnedMessage ckSystemSharingUIObserver initSelector

-- | @+ new@
new :: IO (Id CKSystemSharingUIObserver)
new  =
  do
    cls' <- getRequiredClass "CKSystemSharingUIObserver"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithContainer:@
initWithContainer :: (IsCKSystemSharingUIObserver ckSystemSharingUIObserver, IsCKContainer container) => ckSystemSharingUIObserver -> container -> IO (Id CKSystemSharingUIObserver)
initWithContainer ckSystemSharingUIObserver container =
  sendOwnedMessage ckSystemSharingUIObserver initWithContainerSelector (toCKContainer container)

-- | Called on success or failure of a @CKShare@ save after user modifications via the system sharing UI
--
-- Following a successful share save by the system sharing UI in the provided @CKContainer,@ this callback will be invoked with a nonnull @recordID,@ a nonnull @share,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nonnull @recordID,@ a nil @share,@ and a nonnull @error@  Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- systemSharingUIDidSaveShareBlock@
systemSharingUIDidSaveShareBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> IO (Ptr ())
systemSharingUIDidSaveShareBlock ckSystemSharingUIObserver =
  sendMessage ckSystemSharingUIObserver systemSharingUIDidSaveShareBlockSelector

-- | Called on success or failure of a @CKShare@ save after user modifications via the system sharing UI
--
-- Following a successful share save by the system sharing UI in the provided @CKContainer,@ this callback will be invoked with a nonnull @recordID,@ a nonnull @share,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nonnull @recordID,@ a nil @share,@ and a nonnull @error@  Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- setSystemSharingUIDidSaveShareBlock:@
setSystemSharingUIDidSaveShareBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> Ptr () -> IO ()
setSystemSharingUIDidSaveShareBlock ckSystemSharingUIObserver value =
  sendMessage ckSystemSharingUIObserver setSystemSharingUIDidSaveShareBlockSelector value

-- | Called on success or failure of a @CKShare@ delete when the user decides to stop sharing via the system sharing UI
--
-- Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- systemSharingUIDidStopSharingBlock@
systemSharingUIDidStopSharingBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> IO (Ptr ())
systemSharingUIDidStopSharingBlock ckSystemSharingUIObserver =
  sendMessage ckSystemSharingUIObserver systemSharingUIDidStopSharingBlockSelector

-- | Called on success or failure of a @CKShare@ delete when the user decides to stop sharing via the system sharing UI
--
-- Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- setSystemSharingUIDidStopSharingBlock:@
setSystemSharingUIDidStopSharingBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> Ptr () -> IO ()
setSystemSharingUIDidStopSharingBlock ckSystemSharingUIObserver value =
  sendMessage ckSystemSharingUIObserver setSystemSharingUIDidStopSharingBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSystemSharingUIObserver)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSystemSharingUIObserver)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithContainer:@
initWithContainerSelector :: Selector '[Id CKContainer] (Id CKSystemSharingUIObserver)
initWithContainerSelector = mkSelector "initWithContainer:"

-- | @Selector@ for @systemSharingUIDidSaveShareBlock@
systemSharingUIDidSaveShareBlockSelector :: Selector '[] (Ptr ())
systemSharingUIDidSaveShareBlockSelector = mkSelector "systemSharingUIDidSaveShareBlock"

-- | @Selector@ for @setSystemSharingUIDidSaveShareBlock:@
setSystemSharingUIDidSaveShareBlockSelector :: Selector '[Ptr ()] ()
setSystemSharingUIDidSaveShareBlockSelector = mkSelector "setSystemSharingUIDidSaveShareBlock:"

-- | @Selector@ for @systemSharingUIDidStopSharingBlock@
systemSharingUIDidStopSharingBlockSelector :: Selector '[] (Ptr ())
systemSharingUIDidStopSharingBlockSelector = mkSelector "systemSharingUIDidStopSharingBlock"

-- | @Selector@ for @setSystemSharingUIDidStopSharingBlock:@
setSystemSharingUIDidStopSharingBlockSelector :: Selector '[Ptr ()] ()
setSystemSharingUIDidStopSharingBlockSelector = mkSelector "setSystemSharingUIDidStopSharingBlock:"

