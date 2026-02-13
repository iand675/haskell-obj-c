{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterTwoDCartesianZoneStruct@.
module ObjC.Matter.MTRZoneManagementClusterTwoDCartesianZoneStruct
  ( MTRZoneManagementClusterTwoDCartesianZoneStruct
  , IsMTRZoneManagementClusterTwoDCartesianZoneStruct(..)
  , name
  , setName
  , use
  , setUse
  , vertices
  , setVertices
  , color
  , setColor
  , colorSelector
  , nameSelector
  , setColorSelector
  , setNameSelector
  , setUseSelector
  , setVerticesSelector
  , useSelector
  , verticesSelector


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
name :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSString)
name mtrZoneManagementClusterTwoDCartesianZoneStruct =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct nameSelector

-- | @- setName:@
setName :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSString value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setName mtrZoneManagementClusterTwoDCartesianZoneStruct value =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct setNameSelector (toNSString value)

-- | @- use@
use :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSNumber)
use mtrZoneManagementClusterTwoDCartesianZoneStruct =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct useSelector

-- | @- setUse:@
setUse :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSNumber value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setUse mtrZoneManagementClusterTwoDCartesianZoneStruct value =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct setUseSelector (toNSNumber value)

-- | @- vertices@
vertices :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSArray)
vertices mtrZoneManagementClusterTwoDCartesianZoneStruct =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct verticesSelector

-- | @- setVertices:@
setVertices :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSArray value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setVertices mtrZoneManagementClusterTwoDCartesianZoneStruct value =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct setVerticesSelector (toNSArray value)

-- | @- color@
color :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSString)
color mtrZoneManagementClusterTwoDCartesianZoneStruct =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct colorSelector

-- | @- setColor:@
setColor :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSString value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setColor mtrZoneManagementClusterTwoDCartesianZoneStruct value =
  sendMessage mtrZoneManagementClusterTwoDCartesianZoneStruct setColorSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @use@
useSelector :: Selector '[] (Id NSNumber)
useSelector = mkSelector "use"

-- | @Selector@ for @setUse:@
setUseSelector :: Selector '[Id NSNumber] ()
setUseSelector = mkSelector "setUse:"

-- | @Selector@ for @vertices@
verticesSelector :: Selector '[] (Id NSArray)
verticesSelector = mkSelector "vertices"

-- | @Selector@ for @setVertices:@
setVerticesSelector :: Selector '[Id NSArray] ()
setVerticesSelector = mkSelector "setVertices:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSString)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSString] ()
setColorSelector = mkSelector "setColor:"

