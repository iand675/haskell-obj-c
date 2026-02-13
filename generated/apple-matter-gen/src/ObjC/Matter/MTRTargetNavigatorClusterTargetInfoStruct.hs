{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTargetNavigatorClusterTargetInfoStruct@.
module ObjC.Matter.MTRTargetNavigatorClusterTargetInfoStruct
  ( MTRTargetNavigatorClusterTargetInfoStruct
  , IsMTRTargetNavigatorClusterTargetInfoStruct(..)
  , identifier
  , setIdentifier
  , name
  , setName
  , identifierSelector
  , nameSelector
  , setIdentifierSelector
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

-- | @- identifier@
identifier :: IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct => mtrTargetNavigatorClusterTargetInfoStruct -> IO (Id NSNumber)
identifier mtrTargetNavigatorClusterTargetInfoStruct =
  sendMessage mtrTargetNavigatorClusterTargetInfoStruct identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct, IsNSNumber value) => mtrTargetNavigatorClusterTargetInfoStruct -> value -> IO ()
setIdentifier mtrTargetNavigatorClusterTargetInfoStruct value =
  sendMessage mtrTargetNavigatorClusterTargetInfoStruct setIdentifierSelector (toNSNumber value)

-- | @- name@
name :: IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct => mtrTargetNavigatorClusterTargetInfoStruct -> IO (Id NSString)
name mtrTargetNavigatorClusterTargetInfoStruct =
  sendMessage mtrTargetNavigatorClusterTargetInfoStruct nameSelector

-- | @- setName:@
setName :: (IsMTRTargetNavigatorClusterTargetInfoStruct mtrTargetNavigatorClusterTargetInfoStruct, IsNSString value) => mtrTargetNavigatorClusterTargetInfoStruct -> value -> IO ()
setName mtrTargetNavigatorClusterTargetInfoStruct value =
  sendMessage mtrTargetNavigatorClusterTargetInfoStruct setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSNumber)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSNumber] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

