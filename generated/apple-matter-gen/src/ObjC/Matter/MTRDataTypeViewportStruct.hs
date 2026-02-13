{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeViewportStruct@.
module ObjC.Matter.MTRDataTypeViewportStruct
  ( MTRDataTypeViewportStruct
  , IsMTRDataTypeViewportStruct(..)
  , x1
  , setX1
  , y1
  , setY1
  , x2
  , setX2
  , y2
  , setY2
  , setX1Selector
  , setX2Selector
  , setY1Selector
  , setY2Selector
  , x1Selector
  , x2Selector
  , y1Selector
  , y2Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- x1@
x1 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
x1 mtrDataTypeViewportStruct =
  sendMessage mtrDataTypeViewportStruct x1Selector

-- | @- setX1:@
setX1 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setX1 mtrDataTypeViewportStruct value =
  sendMessage mtrDataTypeViewportStruct setX1Selector (toNSNumber value)

-- | @- y1@
y1 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
y1 mtrDataTypeViewportStruct =
  sendMessage mtrDataTypeViewportStruct y1Selector

-- | @- setY1:@
setY1 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setY1 mtrDataTypeViewportStruct value =
  sendMessage mtrDataTypeViewportStruct setY1Selector (toNSNumber value)

-- | @- x2@
x2 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
x2 mtrDataTypeViewportStruct =
  sendMessage mtrDataTypeViewportStruct x2Selector

-- | @- setX2:@
setX2 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setX2 mtrDataTypeViewportStruct value =
  sendMessage mtrDataTypeViewportStruct setX2Selector (toNSNumber value)

-- | @- y2@
y2 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
y2 mtrDataTypeViewportStruct =
  sendMessage mtrDataTypeViewportStruct y2Selector

-- | @- setY2:@
setY2 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setY2 mtrDataTypeViewportStruct value =
  sendMessage mtrDataTypeViewportStruct setY2Selector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @x1@
x1Selector :: Selector '[] (Id NSNumber)
x1Selector = mkSelector "x1"

-- | @Selector@ for @setX1:@
setX1Selector :: Selector '[Id NSNumber] ()
setX1Selector = mkSelector "setX1:"

-- | @Selector@ for @y1@
y1Selector :: Selector '[] (Id NSNumber)
y1Selector = mkSelector "y1"

-- | @Selector@ for @setY1:@
setY1Selector :: Selector '[Id NSNumber] ()
setY1Selector = mkSelector "setY1:"

-- | @Selector@ for @x2@
x2Selector :: Selector '[] (Id NSNumber)
x2Selector = mkSelector "x2"

-- | @Selector@ for @setX2:@
setX2Selector :: Selector '[Id NSNumber] ()
setX2Selector = mkSelector "setX2:"

-- | @Selector@ for @y2@
y2Selector :: Selector '[] (Id NSNumber)
y2Selector = mkSelector "y2"

-- | @Selector@ for @setY2:@
setY2Selector :: Selector '[Id NSNumber] ()
setY2Selector = mkSelector "setY2:"

