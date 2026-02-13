{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDistributedLock@.
module ObjC.Foundation.NSDistributedLock
  ( NSDistributedLock
  , IsNSDistributedLock(..)
  , lockWithPath
  , init_
  , initWithPath
  , tryLock
  , unlock
  , breakLock
  , lockDate
  , breakLockSelector
  , initSelector
  , initWithPathSelector
  , lockDateSelector
  , lockWithPathSelector
  , tryLockSelector
  , unlockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ lockWithPath:@
lockWithPath :: IsNSString path => path -> IO (Id NSDistributedLock)
lockWithPath path =
  do
    cls' <- getRequiredClass "NSDistributedLock"
    sendClassMessage cls' lockWithPathSelector (toNSString path)

-- | @- init@
init_ :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO (Id NSDistributedLock)
init_ nsDistributedLock =
  sendOwnedMessage nsDistributedLock initSelector

-- | @- initWithPath:@
initWithPath :: (IsNSDistributedLock nsDistributedLock, IsNSString path) => nsDistributedLock -> path -> IO (Id NSDistributedLock)
initWithPath nsDistributedLock path =
  sendOwnedMessage nsDistributedLock initWithPathSelector (toNSString path)

-- | @- tryLock@
tryLock :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO Bool
tryLock nsDistributedLock =
  sendMessage nsDistributedLock tryLockSelector

-- | @- unlock@
unlock :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO ()
unlock nsDistributedLock =
  sendMessage nsDistributedLock unlockSelector

-- | @- breakLock@
breakLock :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO ()
breakLock nsDistributedLock =
  sendMessage nsDistributedLock breakLockSelector

-- | @- lockDate@
lockDate :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO (Id NSDate)
lockDate nsDistributedLock =
  sendMessage nsDistributedLock lockDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockWithPath:@
lockWithPathSelector :: Selector '[Id NSString] (Id NSDistributedLock)
lockWithPathSelector = mkSelector "lockWithPath:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDistributedLock)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector '[Id NSString] (Id NSDistributedLock)
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector '[] Bool
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector '[] ()
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @breakLock@
breakLockSelector :: Selector '[] ()
breakLockSelector = mkSelector "breakLock"

-- | @Selector@ for @lockDate@
lockDateSelector :: Selector '[] (Id NSDate)
lockDateSelector = mkSelector "lockDate"

