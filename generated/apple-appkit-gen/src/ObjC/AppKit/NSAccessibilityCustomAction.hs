{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAccessibilityCustomAction@.
module ObjC.AppKit.NSAccessibilityCustomAction
  ( NSAccessibilityCustomAction
  , IsNSAccessibilityCustomAction(..)
  , initWithName_handler
  , initWithName_target_selector
  , name
  , setName
  , handler
  , setHandler
  , target
  , setTarget
  , selector
  , setSelector
  , handlerSelector
  , initWithName_handlerSelector
  , initWithName_target_selectorSelector
  , nameSelector
  , selectorSelector
  , setHandlerSelector
  , setNameSelector
  , setSelectorSelector
  , setTargetSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:handler:@
initWithName_handler :: (IsNSAccessibilityCustomAction nsAccessibilityCustomAction, IsNSString name) => nsAccessibilityCustomAction -> name -> Ptr () -> IO (Id NSAccessibilityCustomAction)
initWithName_handler nsAccessibilityCustomAction name handler =
  sendOwnedMessage nsAccessibilityCustomAction initWithName_handlerSelector (toNSString name) handler

-- | @- initWithName:target:selector:@
initWithName_target_selector :: (IsNSAccessibilityCustomAction nsAccessibilityCustomAction, IsNSString name) => nsAccessibilityCustomAction -> name -> RawId -> Sel -> IO (Id NSAccessibilityCustomAction)
initWithName_target_selector nsAccessibilityCustomAction name target selector =
  sendOwnedMessage nsAccessibilityCustomAction initWithName_target_selectorSelector (toNSString name) target selector

-- | @- name@
name :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO (Id NSString)
name nsAccessibilityCustomAction =
  sendMessage nsAccessibilityCustomAction nameSelector

-- | @- setName:@
setName :: (IsNSAccessibilityCustomAction nsAccessibilityCustomAction, IsNSString value) => nsAccessibilityCustomAction -> value -> IO ()
setName nsAccessibilityCustomAction value =
  sendMessage nsAccessibilityCustomAction setNameSelector (toNSString value)

-- | @- handler@
handler :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO (Ptr ())
handler nsAccessibilityCustomAction =
  sendMessage nsAccessibilityCustomAction handlerSelector

-- | @- setHandler:@
setHandler :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> Ptr () -> IO ()
setHandler nsAccessibilityCustomAction value =
  sendMessage nsAccessibilityCustomAction setHandlerSelector value

-- | @- target@
target :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO RawId
target nsAccessibilityCustomAction =
  sendMessage nsAccessibilityCustomAction targetSelector

-- | @- setTarget:@
setTarget :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> RawId -> IO ()
setTarget nsAccessibilityCustomAction value =
  sendMessage nsAccessibilityCustomAction setTargetSelector value

-- | @- selector@
selector :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO Sel
selector nsAccessibilityCustomAction =
  sendMessage nsAccessibilityCustomAction selectorSelector

-- | @- setSelector:@
setSelector :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> Sel -> IO ()
setSelector nsAccessibilityCustomAction value =
  sendMessage nsAccessibilityCustomAction setSelectorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:handler:@
initWithName_handlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSAccessibilityCustomAction)
initWithName_handlerSelector = mkSelector "initWithName:handler:"

-- | @Selector@ for @initWithName:target:selector:@
initWithName_target_selectorSelector :: Selector '[Id NSString, RawId, Sel] (Id NSAccessibilityCustomAction)
initWithName_target_selectorSelector = mkSelector "initWithName:target:selector:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @handler@
handlerSelector :: Selector '[] (Ptr ())
handlerSelector = mkSelector "handler"

-- | @Selector@ for @setHandler:@
setHandlerSelector :: Selector '[Ptr ()] ()
setHandlerSelector = mkSelector "setHandler:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @selector@
selectorSelector :: Selector '[] Sel
selectorSelector = mkSelector "selector"

-- | @Selector@ for @setSelector:@
setSelectorSelector :: Selector '[Sel] ()
setSelectorSelector = mkSelector "setSelector:"

