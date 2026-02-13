{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLFunctionStitchingFunctionNode
--
-- A function node that calls the specified function with arguments and ordering determined by data and control dependencies.
--
-- Generated bindings for @MTLFunctionStitchingFunctionNode@.
module ObjC.Metal.MTLFunctionStitchingFunctionNode
  ( MTLFunctionStitchingFunctionNode
  , IsMTLFunctionStitchingFunctionNode(..)
  , initWithName_arguments_controlDependencies
  , name
  , setName
  , arguments
  , setArguments
  , controlDependencies
  , setControlDependencies
  , argumentsSelector
  , controlDependenciesSelector
  , initWithName_arguments_controlDependenciesSelector
  , nameSelector
  , setArgumentsSelector
  , setControlDependenciesSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:arguments:controlDependencies:@
initWithName_arguments_controlDependencies :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSString name, IsNSArray arguments, IsNSArray controlDependencies) => mtlFunctionStitchingFunctionNode -> name -> arguments -> controlDependencies -> IO (Id MTLFunctionStitchingFunctionNode)
initWithName_arguments_controlDependencies mtlFunctionStitchingFunctionNode name arguments controlDependencies =
  sendOwnedMessage mtlFunctionStitchingFunctionNode initWithName_arguments_controlDependenciesSelector (toNSString name) (toNSArray arguments) (toNSArray controlDependencies)

-- | @- name@
name :: IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode => mtlFunctionStitchingFunctionNode -> IO (Id NSString)
name mtlFunctionStitchingFunctionNode =
  sendMessage mtlFunctionStitchingFunctionNode nameSelector

-- | @- setName:@
setName :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSString value) => mtlFunctionStitchingFunctionNode -> value -> IO ()
setName mtlFunctionStitchingFunctionNode value =
  sendMessage mtlFunctionStitchingFunctionNode setNameSelector (toNSString value)

-- | @- arguments@
arguments :: IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode => mtlFunctionStitchingFunctionNode -> IO (Id NSArray)
arguments mtlFunctionStitchingFunctionNode =
  sendMessage mtlFunctionStitchingFunctionNode argumentsSelector

-- | @- setArguments:@
setArguments :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSArray value) => mtlFunctionStitchingFunctionNode -> value -> IO ()
setArguments mtlFunctionStitchingFunctionNode value =
  sendMessage mtlFunctionStitchingFunctionNode setArgumentsSelector (toNSArray value)

-- | @- controlDependencies@
controlDependencies :: IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode => mtlFunctionStitchingFunctionNode -> IO (Id NSArray)
controlDependencies mtlFunctionStitchingFunctionNode =
  sendMessage mtlFunctionStitchingFunctionNode controlDependenciesSelector

-- | @- setControlDependencies:@
setControlDependencies :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSArray value) => mtlFunctionStitchingFunctionNode -> value -> IO ()
setControlDependencies mtlFunctionStitchingFunctionNode value =
  sendMessage mtlFunctionStitchingFunctionNode setControlDependenciesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:arguments:controlDependencies:@
initWithName_arguments_controlDependenciesSelector :: Selector '[Id NSString, Id NSArray, Id NSArray] (Id MTLFunctionStitchingFunctionNode)
initWithName_arguments_controlDependenciesSelector = mkSelector "initWithName:arguments:controlDependencies:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector '[Id NSArray] ()
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @controlDependencies@
controlDependenciesSelector :: Selector '[] (Id NSArray)
controlDependenciesSelector = mkSelector "controlDependencies"

-- | @Selector@ for @setControlDependencies:@
setControlDependenciesSelector :: Selector '[Id NSArray] ()
setControlDependenciesSelector = mkSelector "setControlDependencies:"

