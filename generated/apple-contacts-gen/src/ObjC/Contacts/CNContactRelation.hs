{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a related contact.
--
-- CNContactRelation is thread safe.
--
-- Generated bindings for @CNContactRelation@.
module ObjC.Contacts.CNContactRelation
  ( CNContactRelation
  , IsCNContactRelation(..)
  , contactRelationWithName
  , initWithName
  , name
  , contactRelationWithNameSelector
  , initWithNameSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ contactRelationWithName:@
contactRelationWithName :: IsNSString name => name -> IO (Id CNContactRelation)
contactRelationWithName name =
  do
    cls' <- getRequiredClass "CNContactRelation"
    sendClassMessage cls' contactRelationWithNameSelector (toNSString name)

-- | @- initWithName:@
initWithName :: (IsCNContactRelation cnContactRelation, IsNSString name) => cnContactRelation -> name -> IO (Id CNContactRelation)
initWithName cnContactRelation name =
  sendOwnedMessage cnContactRelation initWithNameSelector (toNSString name)

-- | @- name@
name :: IsCNContactRelation cnContactRelation => cnContactRelation -> IO (Id NSString)
name cnContactRelation =
  sendMessage cnContactRelation nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contactRelationWithName:@
contactRelationWithNameSelector :: Selector '[Id NSString] (Id CNContactRelation)
contactRelationWithNameSelector = mkSelector "contactRelationWithName:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id CNContactRelation)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

