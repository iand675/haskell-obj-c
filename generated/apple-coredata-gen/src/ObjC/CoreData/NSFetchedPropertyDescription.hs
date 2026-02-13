{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFetchedPropertyDescription@.
module ObjC.CoreData.NSFetchedPropertyDescription
  ( NSFetchedPropertyDescription
  , IsNSFetchedPropertyDescription(..)
  , fetchRequest
  , setFetchRequest
  , fetchRequestSelector
  , setFetchRequestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fetchRequest@
fetchRequest :: IsNSFetchedPropertyDescription nsFetchedPropertyDescription => nsFetchedPropertyDescription -> IO (Id NSFetchRequest)
fetchRequest nsFetchedPropertyDescription =
  sendMessage nsFetchedPropertyDescription fetchRequestSelector

-- | @- setFetchRequest:@
setFetchRequest :: (IsNSFetchedPropertyDescription nsFetchedPropertyDescription, IsNSFetchRequest value) => nsFetchedPropertyDescription -> value -> IO ()
setFetchRequest nsFetchedPropertyDescription value =
  sendMessage nsFetchedPropertyDescription setFetchRequestSelector (toNSFetchRequest value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @setFetchRequest:@
setFetchRequestSelector :: Selector '[Id NSFetchRequest] ()
setFetchRequestSelector = mkSelector "setFetchRequest:"

