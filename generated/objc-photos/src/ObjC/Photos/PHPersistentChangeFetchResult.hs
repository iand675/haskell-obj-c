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
  , newSelector
  , initSelector
  , enumerateChangesWithBlockSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPersistentChangeFetchResult)
new  =
  do
    cls' <- getRequiredClass "PHPersistentChangeFetchResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHPersistentChangeFetchResult phPersistentChangeFetchResult => phPersistentChangeFetchResult -> IO (Id PHPersistentChangeFetchResult)
init_ phPersistentChangeFetchResult  =
  sendMsg phPersistentChangeFetchResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- enumerateChangesWithBlock:@
enumerateChangesWithBlock :: IsPHPersistentChangeFetchResult phPersistentChangeFetchResult => phPersistentChangeFetchResult -> Ptr () -> IO ()
enumerateChangesWithBlock phPersistentChangeFetchResult  block =
  sendMsg phPersistentChangeFetchResult (mkSelector "enumerateChangesWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @enumerateChangesWithBlock:@
enumerateChangesWithBlockSelector :: Selector
enumerateChangesWithBlockSelector = mkSelector "enumerateChangesWithBlock:"

