{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Used to request information from multiple identity documents.
--
-- Generated bindings for @PKIdentityAnyOfDescriptor@.
module ObjC.PassKit.PKIdentityAnyOfDescriptor
  ( PKIdentityAnyOfDescriptor
  , IsPKIdentityAnyOfDescriptor(..)
  , initWithDescriptors
  , init_
  , new
  , descriptors
  , initWithDescriptorsSelector
  , initSelector
  , newSelector
  , descriptorsSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a composite document descriptor with specified descriptors.
--
-- ObjC selector: @- initWithDescriptors:@
initWithDescriptors :: (IsPKIdentityAnyOfDescriptor pkIdentityAnyOfDescriptor, IsNSArray descriptors) => pkIdentityAnyOfDescriptor -> descriptors -> IO (Id PKIdentityAnyOfDescriptor)
initWithDescriptors pkIdentityAnyOfDescriptor  descriptors =
  withObjCPtr descriptors $ \raw_descriptors ->
      sendMsg pkIdentityAnyOfDescriptor (mkSelector "initWithDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_descriptors :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPKIdentityAnyOfDescriptor pkIdentityAnyOfDescriptor => pkIdentityAnyOfDescriptor -> IO (Id PKIdentityAnyOfDescriptor)
init_ pkIdentityAnyOfDescriptor  =
    sendMsg pkIdentityAnyOfDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKIdentityAnyOfDescriptor)
new  =
  do
    cls' <- getRequiredClass "PKIdentityAnyOfDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Set of requested descriptors for the composite document descriptor.
--
-- ObjC selector: @- descriptors@
descriptors :: IsPKIdentityAnyOfDescriptor pkIdentityAnyOfDescriptor => pkIdentityAnyOfDescriptor -> IO (Id NSArray)
descriptors pkIdentityAnyOfDescriptor  =
    sendMsg pkIdentityAnyOfDescriptor (mkSelector "descriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDescriptors:@
initWithDescriptorsSelector :: Selector
initWithDescriptorsSelector = mkSelector "initWithDescriptors:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector
descriptorsSelector = mkSelector "descriptors"

