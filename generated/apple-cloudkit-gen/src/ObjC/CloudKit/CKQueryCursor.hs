{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKQueryCursor@.
module ObjC.CloudKit.CKQueryCursor
  ( CKQueryCursor
  , IsCKQueryCursor(..)
  , init_
  , new
  , initSelector
  , newSelector


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
init_ :: IsCKQueryCursor ckQueryCursor => ckQueryCursor -> IO (Id CKQueryCursor)
init_ ckQueryCursor =
  sendOwnedMessage ckQueryCursor initSelector

-- | @+ new@
new :: IO (Id CKQueryCursor)
new  =
  do
    cls' <- getRequiredClass "CKQueryCursor"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKQueryCursor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKQueryCursor)
newSelector = mkSelector "new"

