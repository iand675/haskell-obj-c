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
  , newSelector
  , initWithContainerSelector
  , systemSharingUIDidSaveShareBlockSelector
  , setSystemSharingUIDidSaveShareBlockSelector
  , systemSharingUIDidStopSharingBlockSelector
  , setSystemSharingUIDidStopSharingBlockSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> IO (Id CKSystemSharingUIObserver)
init_ ckSystemSharingUIObserver  =
  sendMsg ckSystemSharingUIObserver (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSystemSharingUIObserver)
new  =
  do
    cls' <- getRequiredClass "CKSystemSharingUIObserver"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithContainer:@
initWithContainer :: (IsCKSystemSharingUIObserver ckSystemSharingUIObserver, IsCKContainer container) => ckSystemSharingUIObserver -> container -> IO (Id CKSystemSharingUIObserver)
initWithContainer ckSystemSharingUIObserver  container =
withObjCPtr container $ \raw_container ->
    sendMsg ckSystemSharingUIObserver (mkSelector "initWithContainer:") (retPtr retVoid) [argPtr (castPtr raw_container :: Ptr ())] >>= ownedObject . castPtr

-- | Called on success or failure of a @CKShare@ save after user modifications via the system sharing UI
--
-- Following a successful share save by the system sharing UI in the provided @CKContainer,@ this callback will be invoked with a nonnull @recordID,@ a nonnull @share,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nonnull @recordID,@ a nil @share,@ and a nonnull @error@  Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- systemSharingUIDidSaveShareBlock@
systemSharingUIDidSaveShareBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> IO (Ptr ())
systemSharingUIDidSaveShareBlock ckSystemSharingUIObserver  =
  fmap castPtr $ sendMsg ckSystemSharingUIObserver (mkSelector "systemSharingUIDidSaveShareBlock") (retPtr retVoid) []

-- | Called on success or failure of a @CKShare@ save after user modifications via the system sharing UI
--
-- Following a successful share save by the system sharing UI in the provided @CKContainer,@ this callback will be invoked with a nonnull @recordID,@ a nonnull @share,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nonnull @recordID,@ a nil @share,@ and a nonnull @error@  Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- setSystemSharingUIDidSaveShareBlock:@
setSystemSharingUIDidSaveShareBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> Ptr () -> IO ()
setSystemSharingUIDidSaveShareBlock ckSystemSharingUIObserver  value =
  sendMsg ckSystemSharingUIObserver (mkSelector "setSystemSharingUIDidSaveShareBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure of a @CKShare@ delete when the user decides to stop sharing via the system sharing UI
--
-- Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- systemSharingUIDidStopSharingBlock@
systemSharingUIDidStopSharingBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> IO (Ptr ())
systemSharingUIDidStopSharingBlock ckSystemSharingUIObserver  =
  fmap castPtr $ sendMsg ckSystemSharingUIObserver (mkSelector "systemSharingUIDidStopSharingBlock") (retPtr retVoid) []

-- | Called on success or failure of a @CKShare@ delete when the user decides to stop sharing via the system sharing UI
--
-- Each @CKSystemSharingUIObserver@ instance has a private serial queue. This queue is used for all callback block invocations.
--
-- ObjC selector: @- setSystemSharingUIDidStopSharingBlock:@
setSystemSharingUIDidStopSharingBlock :: IsCKSystemSharingUIObserver ckSystemSharingUIObserver => ckSystemSharingUIObserver -> Ptr () -> IO ()
setSystemSharingUIDidStopSharingBlock ckSystemSharingUIObserver  value =
  sendMsg ckSystemSharingUIObserver (mkSelector "setSystemSharingUIDidStopSharingBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithContainer:@
initWithContainerSelector :: Selector
initWithContainerSelector = mkSelector "initWithContainer:"

-- | @Selector@ for @systemSharingUIDidSaveShareBlock@
systemSharingUIDidSaveShareBlockSelector :: Selector
systemSharingUIDidSaveShareBlockSelector = mkSelector "systemSharingUIDidSaveShareBlock"

-- | @Selector@ for @setSystemSharingUIDidSaveShareBlock:@
setSystemSharingUIDidSaveShareBlockSelector :: Selector
setSystemSharingUIDidSaveShareBlockSelector = mkSelector "setSystemSharingUIDidSaveShareBlock:"

-- | @Selector@ for @systemSharingUIDidStopSharingBlock@
systemSharingUIDidStopSharingBlockSelector :: Selector
systemSharingUIDidStopSharingBlockSelector = mkSelector "systemSharingUIDidStopSharingBlock"

-- | @Selector@ for @setSystemSharingUIDidStopSharingBlock:@
setSystemSharingUIDidStopSharingBlockSelector :: Selector
setSystemSharingUIDidStopSharingBlockSelector = mkSelector "setSystemSharingUIDidStopSharingBlock:"

