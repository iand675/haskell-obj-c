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
  , linkedFunctionsSelector
  , functionsSelector
  , setFunctionsSelector
  , binaryFunctionsSelector
  , setBinaryFunctionsSelector
  , groupsSelector
  , setGroupsSelector
  , privateFunctionsSelector
  , setPrivateFunctionsSelector


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
    sendClassMsg cls' (mkSelector "linkedFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | functions
--
-- The array of functions to be AIR linked.
--
-- ObjC selector: @- functions@
functions :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSArray)
functions mtlLinkedFunctions  =
    sendMsg mtlLinkedFunctions (mkSelector "functions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | functions
--
-- The array of functions to be AIR linked.
--
-- ObjC selector: @- setFunctions:@
setFunctions :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSArray value) => mtlLinkedFunctions -> value -> IO ()
setFunctions mtlLinkedFunctions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlLinkedFunctions (mkSelector "setFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | binaryFunctions
--
-- The array of functions compiled to binary to be linked.
--
-- ObjC selector: @- binaryFunctions@
binaryFunctions :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSArray)
binaryFunctions mtlLinkedFunctions  =
    sendMsg mtlLinkedFunctions (mkSelector "binaryFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | binaryFunctions
--
-- The array of functions compiled to binary to be linked.
--
-- ObjC selector: @- setBinaryFunctions:@
setBinaryFunctions :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSArray value) => mtlLinkedFunctions -> value -> IO ()
setBinaryFunctions mtlLinkedFunctions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlLinkedFunctions (mkSelector "setBinaryFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | groups
--
-- Groups of functions, grouped to match callsites in the shader code.
--
-- ObjC selector: @- groups@
groups :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSDictionary)
groups mtlLinkedFunctions  =
    sendMsg mtlLinkedFunctions (mkSelector "groups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | groups
--
-- Groups of functions, grouped to match callsites in the shader code.
--
-- ObjC selector: @- setGroups:@
setGroups :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSDictionary value) => mtlLinkedFunctions -> value -> IO ()
setGroups mtlLinkedFunctions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlLinkedFunctions (mkSelector "setGroups:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | privateFunctions
--
-- The array of functions to be AIR linked.
--
-- These functions are not exported by the pipeline state as MTLFunctionHandle objects. Function pointer support is not required to link private functions.
--
-- ObjC selector: @- privateFunctions@
privateFunctions :: IsMTLLinkedFunctions mtlLinkedFunctions => mtlLinkedFunctions -> IO (Id NSArray)
privateFunctions mtlLinkedFunctions  =
    sendMsg mtlLinkedFunctions (mkSelector "privateFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | privateFunctions
--
-- The array of functions to be AIR linked.
--
-- These functions are not exported by the pipeline state as MTLFunctionHandle objects. Function pointer support is not required to link private functions.
--
-- ObjC selector: @- setPrivateFunctions:@
setPrivateFunctions :: (IsMTLLinkedFunctions mtlLinkedFunctions, IsNSArray value) => mtlLinkedFunctions -> value -> IO ()
setPrivateFunctions mtlLinkedFunctions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlLinkedFunctions (mkSelector "setPrivateFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @linkedFunctions@
linkedFunctionsSelector :: Selector
linkedFunctionsSelector = mkSelector "linkedFunctions"

-- | @Selector@ for @functions@
functionsSelector :: Selector
functionsSelector = mkSelector "functions"

-- | @Selector@ for @setFunctions:@
setFunctionsSelector :: Selector
setFunctionsSelector = mkSelector "setFunctions:"

-- | @Selector@ for @binaryFunctions@
binaryFunctionsSelector :: Selector
binaryFunctionsSelector = mkSelector "binaryFunctions"

-- | @Selector@ for @setBinaryFunctions:@
setBinaryFunctionsSelector :: Selector
setBinaryFunctionsSelector = mkSelector "setBinaryFunctions:"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector
setGroupsSelector = mkSelector "setGroups:"

-- | @Selector@ for @privateFunctions@
privateFunctionsSelector :: Selector
privateFunctionsSelector = mkSelector "privateFunctions"

-- | @Selector@ for @setPrivateFunctions:@
setPrivateFunctionsSelector :: Selector
setPrivateFunctionsSelector = mkSelector "setPrivateFunctions:"

