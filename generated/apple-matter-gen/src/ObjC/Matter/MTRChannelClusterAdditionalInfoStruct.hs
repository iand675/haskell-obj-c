{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterAdditionalInfoStruct@.
module ObjC.Matter.MTRChannelClusterAdditionalInfoStruct
  ( MTRChannelClusterAdditionalInfoStruct
  , IsMTRChannelClusterAdditionalInfoStruct(..)
  , name
  , setName
  , value
  , setValue
  , nameSelector
  , setNameSelector
  , setValueSelector
  , valueSelector


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
name :: IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct => mtrChannelClusterAdditionalInfoStruct -> IO (Id NSString)
name mtrChannelClusterAdditionalInfoStruct =
  sendMessage mtrChannelClusterAdditionalInfoStruct nameSelector

-- | @- setName:@
setName :: (IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct, IsNSString value) => mtrChannelClusterAdditionalInfoStruct -> value -> IO ()
setName mtrChannelClusterAdditionalInfoStruct value =
  sendMessage mtrChannelClusterAdditionalInfoStruct setNameSelector (toNSString value)

-- | @- value@
value :: IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct => mtrChannelClusterAdditionalInfoStruct -> IO (Id NSString)
value mtrChannelClusterAdditionalInfoStruct =
  sendMessage mtrChannelClusterAdditionalInfoStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRChannelClusterAdditionalInfoStruct mtrChannelClusterAdditionalInfoStruct, IsNSString value) => mtrChannelClusterAdditionalInfoStruct -> value -> IO ()
setValue mtrChannelClusterAdditionalInfoStruct value =
  sendMessage mtrChannelClusterAdditionalInfoStruct setValueSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

