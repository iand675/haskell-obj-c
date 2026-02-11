{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EKObject@.
module ObjC.EventKit.EKObject
  ( EKObject
  , IsEKObject(..)
  , reset
  , rollback
  , refresh
  , hasChanges
  , new
  , resetSelector
  , rollbackSelector
  , refreshSelector
  , hasChangesSelector
  , newSelector


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

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reset@
reset :: IsEKObject ekObject => ekObject -> IO ()
reset ekObject  =
  sendMsg ekObject (mkSelector "reset") retVoid []

-- | @- rollback@
rollback :: IsEKObject ekObject => ekObject -> IO ()
rollback ekObject  =
  sendMsg ekObject (mkSelector "rollback") retVoid []

-- | @- refresh@
refresh :: IsEKObject ekObject => ekObject -> IO Bool
refresh ekObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekObject (mkSelector "refresh") retCULong []

-- | @- hasChanges@
hasChanges :: IsEKObject ekObject => ekObject -> IO Bool
hasChanges ekObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekObject (mkSelector "hasChanges") retCULong []

-- | @- new@
new :: IsEKObject ekObject => ekObject -> IO Bool
new ekObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekObject (mkSelector "new") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @rollback@
rollbackSelector :: Selector
rollbackSelector = mkSelector "rollback"

-- | @Selector@ for @refresh@
refreshSelector :: Selector
refreshSelector = mkSelector "refresh"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector
hasChangesSelector = mkSelector "hasChanges"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

