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
  , controlDependencies
  , setControlDependencies
  , initWithName_arguments_controlDependenciesSelector
  , nameSelector
  , setNameSelector
  , controlDependenciesSelector
  , setControlDependenciesSelector


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

-- | @- initWithName:arguments:controlDependencies:@
initWithName_arguments_controlDependencies :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSString name, IsNSArray arguments, IsNSArray controlDependencies) => mtlFunctionStitchingFunctionNode -> name -> arguments -> controlDependencies -> IO (Id MTLFunctionStitchingFunctionNode)
initWithName_arguments_controlDependencies mtlFunctionStitchingFunctionNode  name arguments controlDependencies =
withObjCPtr name $ \raw_name ->
  withObjCPtr arguments $ \raw_arguments ->
    withObjCPtr controlDependencies $ \raw_controlDependencies ->
        sendMsg mtlFunctionStitchingFunctionNode (mkSelector "initWithName:arguments:controlDependencies:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_controlDependencies :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode => mtlFunctionStitchingFunctionNode -> IO (Id NSString)
name mtlFunctionStitchingFunctionNode  =
  sendMsg mtlFunctionStitchingFunctionNode (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSString value) => mtlFunctionStitchingFunctionNode -> value -> IO ()
setName mtlFunctionStitchingFunctionNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlFunctionStitchingFunctionNode (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- controlDependencies@
controlDependencies :: IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode => mtlFunctionStitchingFunctionNode -> IO (Id NSArray)
controlDependencies mtlFunctionStitchingFunctionNode  =
  sendMsg mtlFunctionStitchingFunctionNode (mkSelector "controlDependencies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setControlDependencies:@
setControlDependencies :: (IsMTLFunctionStitchingFunctionNode mtlFunctionStitchingFunctionNode, IsNSArray value) => mtlFunctionStitchingFunctionNode -> value -> IO ()
setControlDependencies mtlFunctionStitchingFunctionNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlFunctionStitchingFunctionNode (mkSelector "setControlDependencies:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:arguments:controlDependencies:@
initWithName_arguments_controlDependenciesSelector :: Selector
initWithName_arguments_controlDependenciesSelector = mkSelector "initWithName:arguments:controlDependencies:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @controlDependencies@
controlDependenciesSelector :: Selector
controlDependenciesSelector = mkSelector "controlDependencies"

-- | @Selector@ for @setControlDependencies:@
setControlDependenciesSelector :: Selector
setControlDependenciesSelector = mkSelector "setControlDependencies:"

