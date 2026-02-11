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
import ObjC.Foundation.Internal.Classes

-- | @- fetchRequest@
fetchRequest :: IsNSFetchedPropertyDescription nsFetchedPropertyDescription => nsFetchedPropertyDescription -> IO (Id NSFetchRequest)
fetchRequest nsFetchedPropertyDescription  =
  sendMsg nsFetchedPropertyDescription (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFetchRequest:@
setFetchRequest :: (IsNSFetchedPropertyDescription nsFetchedPropertyDescription, IsNSFetchRequest value) => nsFetchedPropertyDescription -> value -> IO ()
setFetchRequest nsFetchedPropertyDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchedPropertyDescription (mkSelector "setFetchRequest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @setFetchRequest:@
setFetchRequestSelector :: Selector
setFetchRequestSelector = mkSelector "setFetchRequest:"

