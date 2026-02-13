{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramCastStruct@.
module ObjC.Matter.MTRChannelClusterProgramCastStruct
  ( MTRChannelClusterProgramCastStruct
  , IsMTRChannelClusterProgramCastStruct(..)
  , name
  , setName
  , role_
  , setRole
  , nameSelector
  , roleSelector
  , setNameSelector
  , setRoleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct => mtrChannelClusterProgramCastStruct -> IO (Id NSString)
name mtrChannelClusterProgramCastStruct =
  sendMessage mtrChannelClusterProgramCastStruct nameSelector

-- | @- setName:@
setName :: (IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct, IsNSString value) => mtrChannelClusterProgramCastStruct -> value -> IO ()
setName mtrChannelClusterProgramCastStruct value =
  sendMessage mtrChannelClusterProgramCastStruct setNameSelector (toNSString value)

-- | @- role@
role_ :: IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct => mtrChannelClusterProgramCastStruct -> IO (Id NSString)
role_ mtrChannelClusterProgramCastStruct =
  sendMessage mtrChannelClusterProgramCastStruct roleSelector

-- | @- setRole:@
setRole :: (IsMTRChannelClusterProgramCastStruct mtrChannelClusterProgramCastStruct, IsNSString value) => mtrChannelClusterProgramCastStruct -> value -> IO ()
setRole mtrChannelClusterProgramCastStruct value =
  sendMessage mtrChannelClusterProgramCastStruct setRoleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @role@
roleSelector :: Selector '[] (Id NSString)
roleSelector = mkSelector "role"

-- | @Selector@ for @setRole:@
setRoleSelector :: Selector '[Id NSString] ()
setRoleSelector = mkSelector "setRole:"

