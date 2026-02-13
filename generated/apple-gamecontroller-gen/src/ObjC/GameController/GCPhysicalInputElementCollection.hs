{-# LANGUAGE DataKinds #-}
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
  , countSelector
  , elementEnumeratorSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id GCPhysicalInputElementCollection)
new  =
  do
    cls' <- getRequiredClass "GCPhysicalInputElementCollection"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsGCPhysicalInputElementCollection gcPhysicalInputElementCollection => gcPhysicalInputElementCollection -> IO (Id GCPhysicalInputElementCollection)
init_ gcPhysicalInputElementCollection =
  sendOwnedMessage gcPhysicalInputElementCollection initSelector

-- | @- elementEnumerator@
elementEnumerator :: IsGCPhysicalInputElementCollection gcPhysicalInputElementCollection => gcPhysicalInputElementCollection -> IO (Id NSEnumerator)
elementEnumerator gcPhysicalInputElementCollection =
  sendMessage gcPhysicalInputElementCollection elementEnumeratorSelector

-- | The number of elements in the collection.
--
-- ObjC selector: @- count@
count :: IsGCPhysicalInputElementCollection gcPhysicalInputElementCollection => gcPhysicalInputElementCollection -> IO CULong
count gcPhysicalInputElementCollection =
  sendMessage gcPhysicalInputElementCollection countSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id GCPhysicalInputElementCollection)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCPhysicalInputElementCollection)
initSelector = mkSelector "init"

-- | @Selector@ for @elementEnumerator@
elementEnumeratorSelector :: Selector '[] (Id NSEnumerator)
elementEnumeratorSelector = mkSelector "elementEnumerator"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

