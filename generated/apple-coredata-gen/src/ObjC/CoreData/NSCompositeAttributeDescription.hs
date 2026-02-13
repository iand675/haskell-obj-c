{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCompositeAttributeDescription@.
module ObjC.CoreData.NSCompositeAttributeDescription
  ( NSCompositeAttributeDescription
  , IsNSCompositeAttributeDescription(..)
  , elements
  , setElements
  , elementsSelector
  , setElementsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- elements@
elements :: IsNSCompositeAttributeDescription nsCompositeAttributeDescription => nsCompositeAttributeDescription -> IO (Id NSArray)
elements nsCompositeAttributeDescription =
  sendMessage nsCompositeAttributeDescription elementsSelector

-- | @- setElements:@
setElements :: (IsNSCompositeAttributeDescription nsCompositeAttributeDescription, IsNSArray value) => nsCompositeAttributeDescription -> value -> IO ()
setElements nsCompositeAttributeDescription value =
  sendMessage nsCompositeAttributeDescription setElementsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id NSArray)
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector '[Id NSArray] ()
setElementsSelector = mkSelector "setElements:"

