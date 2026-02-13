{-# LANGUAGE DataKinds #-}
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
  , descriptorsSelector
  , initSelector
  , initWithDescriptorsSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a composite document descriptor with specified descriptors.
--
-- ObjC selector: @- initWithDescriptors:@
initWithDescriptors :: (IsPKIdentityAnyOfDescriptor pkIdentityAnyOfDescriptor, IsNSArray descriptors) => pkIdentityAnyOfDescriptor -> descriptors -> IO (Id PKIdentityAnyOfDescriptor)
initWithDescriptors pkIdentityAnyOfDescriptor descriptors =
  sendOwnedMessage pkIdentityAnyOfDescriptor initWithDescriptorsSelector (toNSArray descriptors)

-- | @- init@
init_ :: IsPKIdentityAnyOfDescriptor pkIdentityAnyOfDescriptor => pkIdentityAnyOfDescriptor -> IO (Id PKIdentityAnyOfDescriptor)
init_ pkIdentityAnyOfDescriptor =
  sendOwnedMessage pkIdentityAnyOfDescriptor initSelector

-- | @+ new@
new :: IO (Id PKIdentityAnyOfDescriptor)
new  =
  do
    cls' <- getRequiredClass "PKIdentityAnyOfDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | Set of requested descriptors for the composite document descriptor.
--
-- ObjC selector: @- descriptors@
descriptors :: IsPKIdentityAnyOfDescriptor pkIdentityAnyOfDescriptor => pkIdentityAnyOfDescriptor -> IO (Id NSArray)
descriptors pkIdentityAnyOfDescriptor =
  sendMessage pkIdentityAnyOfDescriptor descriptorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDescriptors:@
initWithDescriptorsSelector :: Selector '[Id NSArray] (Id PKIdentityAnyOfDescriptor)
initWithDescriptorsSelector = mkSelector "initWithDescriptors:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIdentityAnyOfDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKIdentityAnyOfDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector '[] (Id NSArray)
descriptorsSelector = mkSelector "descriptors"

