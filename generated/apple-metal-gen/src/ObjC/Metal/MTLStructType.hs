{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLStructType@.
module ObjC.Metal.MTLStructType
  ( MTLStructType
  , IsMTLStructType(..)
  , memberByName
  , members
  , memberByNameSelector
  , membersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- memberByName:@
memberByName :: (IsMTLStructType mtlStructType, IsNSString name) => mtlStructType -> name -> IO (Id MTLStructMember)
memberByName mtlStructType name =
  sendMessage mtlStructType memberByNameSelector (toNSString name)

-- | @- members@
members :: IsMTLStructType mtlStructType => mtlStructType -> IO (Id NSArray)
members mtlStructType =
  sendMessage mtlStructType membersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @memberByName:@
memberByNameSelector :: Selector '[Id NSString] (Id MTLStructMember)
memberByNameSelector = mkSelector "memberByName:"

-- | @Selector@ for @members@
membersSelector :: Selector '[] (Id NSArray)
membersSelector = mkSelector "members"

