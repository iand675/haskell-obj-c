{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLLinkedFunctions
--
-- A class to set functions to be linked.
--
-- All functions set on this object must have unique names.
--
-- Generated bindings for @MTLLinkedFunctions@.
module ObjC.Metal.MTLLinkedFunctions
  ( MTLLinkedFunctions
  , IsMTLLinkedFunctions(..)
  , linkedFunctions
  , functions
  , setFunctions
  , binaryFunctions
  , setBinaryFunctions
  , groups
  , setGroups
  , privateFunctions
  , setPrivateFunctions
  , binaryFunctionsSelector
  , functionsSelector
  , groupsSelector
  , linkedFunctionsSelector
  , privateFunctionsSelector
  , setBinaryFunctionsSelector
  , setFunctionsSelector
  , setGroupsSelector
  , setPrivateFunctionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | linkedFunctions
--
-- Create an autoreleased MTLLinkedFunctions object.
--
-- ObjC selector: @+ linkedFunctions@
linkedFunctions :: IO (Id MTLLinkedFunctions)
linkedFunctions  =
  do
    cls' <- getRequiredClass "MTLLinkedFunctions"
    sendClassMessage cls' linkedFunctionsSelector

-- | functions
--
-- The array of functions to be AIR linked.
--
-- ObjC selector: @- functions@
functions :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSArray)
functions mtlLinkedFunctions =
  sendMessage mtlLinkedFunctions functionsSelector

-- | functions
--
-- The array of functions to be AIR linked.
--
-- ObjC selector: @- setFunctions:@
setFunctions :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSArray value) => mtlLinkedFunctions -> value -> IO ()
setFunctions mtlLinkedFunctions value =
  sendMessage mtlLinkedFunctions setFunctionsSelector (toNSArray value)

-- | binaryFunctions
--
-- The array of functions compiled to binary to be linked.
--
-- ObjC selector: @- binaryFunctions@
binaryFunctions :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSArray)
binaryFunctions mtlLinkedFunctions =
  sendMessage mtlLinkedFunctions binaryFunctionsSelector

-- | binaryFunctions
--
-- The array of functions compiled to binary to be linked.
--
-- ObjC selector: @- setBinaryFunctions:@
setBinaryFunctions :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSArray value) => mtlLinkedFunctions -> value -> IO ()
setBinaryFunctions mtlLinkedFunctions value =
  sendMessage mtlLinkedFunctions setBinaryFunctionsSelector (toNSArray value)

-- | groups
--
-- Groups of functions, grouped to match callsites in the shader code.
--
-- ObjC selector: @- groups@
groups :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSDictionary)
groups mtlLinkedFunctions =
  sendMessage mtlLinkedFunctions groupsSelector

-- | groups
--
-- Groups of functions, grouped to match callsites in the shader code.
--
-- ObjC selector: @- setGroups:@
setGroups :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSDictionary value) => mtlLinkedFunctions -> value -> IO ()
setGroups mtlLinkedFunctions value =
  sendMessage mtlLinkedFunctions setGroupsSelector (toNSDictionary value)

-- | privateFunctions
--
-- The array of functions to be AIR linked.
--
-- These functions are not exported by the pipeline state as MTLFunctionHandle objects. Function pointer support is not required to link private functions.
--
-- ObjC selector: @- privateFunctions@
privateFunctions :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSArray)
privateFunctions mtlLinkedFunctions =
  sendMessage mtlLinkedFunctions privateFunctionsSelector

-- | privateFunctions
--
-- The array of functions to be AIR linked.
--
-- These functions are not exported by the pipeline state as MTLFunctionHandle objects. Function pointer support is not required to link private functions.
--
-- ObjC selector: @- setPrivateFunctions:@
setPrivateFunctions :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSArray value) => mtlLinkedFunctions -> value -> IO ()
setPrivateFunctions mtlLinkedFunctions value =
  sendMessage mtlLinkedFunctions setPrivateFunctionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @linkedFunctions@
linkedFunctionsSelector :: Selector '[] (Id MTLLinkedFunctions)
linkedFunctionsSelector = mkSelector "linkedFunctions"

-- | @Selector@ for @functions@
functionsSelector :: Selector '[] (Id NSArray)
functionsSelector = mkSelector "functions"

-- | @Selector@ for @setFunctions:@
setFunctionsSelector :: Selector '[Id NSArray] ()
setFunctionsSelector = mkSelector "setFunctions:"

-- | @Selector@ for @binaryFunctions@
binaryFunctionsSelector :: Selector '[] (Id NSArray)
binaryFunctionsSelector = mkSelector "binaryFunctions"

-- | @Selector@ for @setBinaryFunctions:@
setBinaryFunctionsSelector :: Selector '[Id NSArray] ()
setBinaryFunctionsSelector = mkSelector "setBinaryFunctions:"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] (Id NSDictionary)
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector '[Id NSDictionary] ()
setGroupsSelector = mkSelector "setGroups:"

-- | @Selector@ for @privateFunctions@
privateFunctionsSelector :: Selector '[] (Id NSArray)
privateFunctionsSelector = mkSelector "privateFunctions"

-- | @Selector@ for @setPrivateFunctions:@
setPrivateFunctionsSelector :: Selector '[Id NSArray] ()
setPrivateFunctionsSelector = mkSelector "setPrivateFunctions:"

