{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterMapStruct@.
module ObjC.Matter.MTRServiceAreaClusterMapStruct
  ( MTRServiceAreaClusterMapStruct
  , IsMTRServiceAreaClusterMapStruct(..)
  , mapID
  , setMapID
  , name
  , setName
  , mapIDSelector
  , nameSelector
  , setMapIDSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mapID@
mapID :: IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct => mtrServiceAreaClusterMapStruct -> IO (Id NSNumber)
mapID mtrServiceAreaClusterMapStruct =
  sendMessage mtrServiceAreaClusterMapStruct mapIDSelector

-- | @- setMapID:@
setMapID :: (IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct, IsNSNumber value) => mtrServiceAreaClusterMapStruct -> value -> IO ()
setMapID mtrServiceAreaClusterMapStruct value =
  sendMessage mtrServiceAreaClusterMapStruct setMapIDSelector (toNSNumber value)

-- | @- name@
name :: IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct => mtrServiceAreaClusterMapStruct -> IO (Id NSString)
name mtrServiceAreaClusterMapStruct =
  sendMessage mtrServiceAreaClusterMapStruct nameSelector

-- | @- setName:@
setName :: (IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct, IsNSString value) => mtrServiceAreaClusterMapStruct -> value -> IO ()
setName mtrServiceAreaClusterMapStruct value =
  sendMessage mtrServiceAreaClusterMapStruct setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapID@
mapIDSelector :: Selector '[] (Id NSNumber)
mapIDSelector = mkSelector "mapID"

-- | @Selector@ for @setMapID:@
setMapIDSelector :: Selector '[Id NSNumber] ()
setMapIDSelector = mkSelector "setMapID:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

