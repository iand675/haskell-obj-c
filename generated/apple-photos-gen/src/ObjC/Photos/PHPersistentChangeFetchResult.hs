{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPersistentChangeFetchResult@.
module ObjC.Photos.PHPersistentChangeFetchResult
  ( PHPersistentChangeFetchResult
  , IsPHPersistentChangeFetchResult(..)
  , new
  , init_
  , enumerateChangesWithBlock
  , enumerateChangesWithBlockSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPersistentChangeFetchResult)
new  =
  do
    cls' <- getRequiredClass "PHPersistentChangeFetchResult"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPHPersistentChangeFetchResult phPersistentChangeFetchResult => phPersistentChangeFetchResult -> IO (Id PHPersistentChangeFetchResult)
init_ phPersistentChangeFetchResult =
  sendOwnedMessage phPersistentChangeFetchResult initSelector

-- | @- enumerateChangesWithBlock:@
enumerateChangesWithBlock :: IsPHPersistentChangeFetchResult phPersistentChangeFetchResult => phPersistentChangeFetchResult -> Ptr () -> IO ()
enumerateChangesWithBlock phPersistentChangeFetchResult block =
  sendMessage phPersistentChangeFetchResult enumerateChangesWithBlockSelector block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHPersistentChangeFetchResult)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPersistentChangeFetchResult)
initSelector = mkSelector "init"

-- | @Selector@ for @enumerateChangesWithBlock:@
enumerateChangesWithBlockSelector :: Selector '[Ptr ()] ()
enumerateChangesWithBlockSelector = mkSelector "enumerateChangesWithBlock:"

