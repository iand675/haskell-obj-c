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

import ObjC.CoreData.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- elements@
elements :: IsNSCompositeAttributeDescription nsCompositeAttributeDescription => nsCompositeAttributeDescription -> IO (Id NSArray)
elements nsCompositeAttributeDescription  =
  sendMsg nsCompositeAttributeDescription (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElements:@
setElements :: (IsNSCompositeAttributeDescription nsCompositeAttributeDescription, IsNSArray value) => nsCompositeAttributeDescription -> value -> IO ()
setElements nsCompositeAttributeDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCompositeAttributeDescription (mkSelector "setElements:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector
setElementsSelector = mkSelector "setElements:"

