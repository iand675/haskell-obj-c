{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An instance of @GCPhysicalInputElementCollection@ contains the collection of input elements found in a device's physical input profile.
--
-- Generated bindings for @GCPhysicalInputElementCollection@.
module ObjC.GameController.GCPhysicalInputElementCollection
  ( GCPhysicalInputElementCollection
  , IsGCPhysicalInputElementCollection(..)
  , new
  , init_
  , elementEnumerator
  , count
  , newSelector
  , initSelector
  , elementEnumeratorSelector
  , countSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id GCPhysicalInputElementCollection)
new  =
  do
    cls' <- getRequiredClass "GCPhysicalInputElementCollection"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsGCPhysicalInputElementCollection gcPhysicalInputElementCollection => gcPhysicalInputElementCollection -> IO (Id GCPhysicalInputElementCollection)
init_ gcPhysicalInputElementCollection  =
  sendMsg gcPhysicalInputElementCollection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- elementEnumerator@
elementEnumerator :: IsGCPhysicalInputElementCollection gcPhysicalInputElementCollection => gcPhysicalInputElementCollection -> IO (Id NSEnumerator)
elementEnumerator gcPhysicalInputElementCollection  =
  sendMsg gcPhysicalInputElementCollection (mkSelector "elementEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The number of elements in the collection.
--
-- ObjC selector: @- count@
count :: IsGCPhysicalInputElementCollection gcPhysicalInputElementCollection => gcPhysicalInputElementCollection -> IO CULong
count gcPhysicalInputElementCollection  =
  sendMsg gcPhysicalInputElementCollection (mkSelector "count") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @elementEnumerator@
elementEnumeratorSelector :: Selector
elementEnumeratorSelector = mkSelector "elementEnumerator"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

