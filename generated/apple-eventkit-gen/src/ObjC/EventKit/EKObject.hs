{-# LANGUAGE DataKinds #-}
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
  , hasChangesSelector
  , newSelector
  , refreshSelector
  , resetSelector
  , rollbackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reset@
reset :: IsEKObject ekObject => ekObject -> IO ()
reset ekObject =
  sendMessage ekObject resetSelector

-- | @- rollback@
rollback :: IsEKObject ekObject => ekObject -> IO ()
rollback ekObject =
  sendMessage ekObject rollbackSelector

-- | @- refresh@
refresh :: IsEKObject ekObject => ekObject -> IO Bool
refresh ekObject =
  sendMessage ekObject refreshSelector

-- | @- hasChanges@
hasChanges :: IsEKObject ekObject => ekObject -> IO Bool
hasChanges ekObject =
  sendMessage ekObject hasChangesSelector

-- | @- new@
new :: IsEKObject ekObject => ekObject -> IO Bool
new ekObject =
  sendOwnedMessage ekObject newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @rollback@
rollbackSelector :: Selector '[] ()
rollbackSelector = mkSelector "rollback"

-- | @Selector@ for @refresh@
refreshSelector :: Selector '[] Bool
refreshSelector = mkSelector "refresh"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector '[] Bool
hasChangesSelector = mkSelector "hasChanges"

-- | @Selector@ for @new@
newSelector :: Selector '[] Bool
newSelector = mkSelector "new"

