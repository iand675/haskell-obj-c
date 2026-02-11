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
  , lockWithPathSelector
  , initSelector
  , initWithPathSelector
  , tryLockSelector
  , unlockSelector
  , breakLockSelector
  , lockDateSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ lockWithPath:@
lockWithPath :: IsNSString path => path -> IO (Id NSDistributedLock)
lockWithPath path =
  do
    cls' <- getRequiredClass "NSDistributedLock"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "lockWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO (Id NSDistributedLock)
init_ nsDistributedLock  =
  sendMsg nsDistributedLock (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPath:@
initWithPath :: (IsNSDistributedLock nsDistributedLock, IsNSString path) => nsDistributedLock -> path -> IO (Id NSDistributedLock)
initWithPath nsDistributedLock  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsDistributedLock (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- tryLock@
tryLock :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO Bool
tryLock nsDistributedLock  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDistributedLock (mkSelector "tryLock") retCULong []

-- | @- unlock@
unlock :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO ()
unlock nsDistributedLock  =
  sendMsg nsDistributedLock (mkSelector "unlock") retVoid []

-- | @- breakLock@
breakLock :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO ()
breakLock nsDistributedLock  =
  sendMsg nsDistributedLock (mkSelector "breakLock") retVoid []

-- | @- lockDate@
lockDate :: IsNSDistributedLock nsDistributedLock => nsDistributedLock -> IO (Id NSDate)
lockDate nsDistributedLock  =
  sendMsg nsDistributedLock (mkSelector "lockDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockWithPath:@
lockWithPathSelector :: Selector
lockWithPathSelector = mkSelector "lockWithPath:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @breakLock@
breakLockSelector :: Selector
breakLockSelector = mkSelector "breakLock"

-- | @Selector@ for @lockDate@
lockDateSelector :: Selector
lockDateSelector = mkSelector "lockDate"

