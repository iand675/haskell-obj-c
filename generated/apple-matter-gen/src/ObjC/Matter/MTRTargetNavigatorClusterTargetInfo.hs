{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTargetNavigatorClusterTargetInfo@.
module ObjC.Matter.MTRTargetNavigatorClusterTargetInfo
  ( MTRTargetNavigatorClusterTargetInfo
  , IsMTRTargetNavigatorClusterTargetInfo(..)
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
identifier :: IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo => mtrTargetNavigatorClusterTargetInfo -> IO (Id NSNumber)
identifier mtrTargetNavigatorClusterTargetInfo =
  sendMessage mtrTargetNavigatorClusterTargetInfo identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo, IsNSNumber value) => mtrTargetNavigatorClusterTargetInfo -> value -> IO ()
setIdentifier mtrTargetNavigatorClusterTargetInfo value =
  sendMessage mtrTargetNavigatorClusterTargetInfo setIdentifierSelector (toNSNumber value)

-- | @- name@
name :: IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo => mtrTargetNavigatorClusterTargetInfo -> IO (Id NSString)
name mtrTargetNavigatorClusterTargetInfo =
  sendMessage mtrTargetNavigatorClusterTargetInfo nameSelector

-- | @- setName:@
setName :: (IsMTRTargetNavigatorClusterTargetInfo mtrTargetNavigatorClusterTargetInfo, IsNSString value) => mtrTargetNavigatorClusterTargetInfo -> value -> IO ()
setName mtrTargetNavigatorClusterTargetInfo value =
  sendMessage mtrTargetNavigatorClusterTargetInfo setNameSelector (toNSString value)

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

