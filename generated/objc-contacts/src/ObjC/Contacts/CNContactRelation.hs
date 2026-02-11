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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ contactRelationWithName:@
contactRelationWithName :: IsNSString name => name -> IO (Id CNContactRelation)
contactRelationWithName name =
  do
    cls' <- getRequiredClass "CNContactRelation"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "contactRelationWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsCNContactRelation cnContactRelation, IsNSString name) => cnContactRelation -> name -> IO (Id CNContactRelation)
initWithName cnContactRelation  name =
withObjCPtr name $ \raw_name ->
    sendMsg cnContactRelation (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsCNContactRelation cnContactRelation => cnContactRelation -> IO (Id NSString)
name cnContactRelation  =
  sendMsg cnContactRelation (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contactRelationWithName:@
contactRelationWithNameSelector :: Selector
contactRelationWithNameSelector = mkSelector "contactRelationWithName:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

