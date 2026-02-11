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
  , initWithName_handlerSelector
  , initWithName_target_selectorSelector
  , nameSelector
  , setNameSelector
  , handlerSelector
  , setHandlerSelector
  , targetSelector
  , setTargetSelector
  , selectorSelector
  , setSelectorSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:handler:@
initWithName_handler :: (IsNSAccessibilityCustomAction nsAccessibilityCustomAction, IsNSString name) => nsAccessibilityCustomAction -> name -> Ptr () -> IO (Id NSAccessibilityCustomAction)
initWithName_handler nsAccessibilityCustomAction  name handler =
  withObjCPtr name $ \raw_name ->
      sendMsg nsAccessibilityCustomAction (mkSelector "initWithName:handler:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:target:selector:@
initWithName_target_selector :: (IsNSAccessibilityCustomAction nsAccessibilityCustomAction, IsNSString name) => nsAccessibilityCustomAction -> name -> RawId -> Selector -> IO (Id NSAccessibilityCustomAction)
initWithName_target_selector nsAccessibilityCustomAction  name target selector =
  withObjCPtr name $ \raw_name ->
      sendMsg nsAccessibilityCustomAction (mkSelector "initWithName:target:selector:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector)] >>= ownedObject . castPtr

-- | @- name@
name :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO (Id NSString)
name nsAccessibilityCustomAction  =
    sendMsg nsAccessibilityCustomAction (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSAccessibilityCustomAction nsAccessibilityCustomAction, IsNSString value) => nsAccessibilityCustomAction -> value -> IO ()
setName nsAccessibilityCustomAction  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsAccessibilityCustomAction (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- handler@
handler :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO (Ptr ())
handler nsAccessibilityCustomAction  =
    fmap castPtr $ sendMsg nsAccessibilityCustomAction (mkSelector "handler") (retPtr retVoid) []

-- | @- setHandler:@
setHandler :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> Ptr () -> IO ()
setHandler nsAccessibilityCustomAction  value =
    sendMsg nsAccessibilityCustomAction (mkSelector "setHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- target@
target :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO RawId
target nsAccessibilityCustomAction  =
    fmap (RawId . castPtr) $ sendMsg nsAccessibilityCustomAction (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> RawId -> IO ()
setTarget nsAccessibilityCustomAction  value =
    sendMsg nsAccessibilityCustomAction (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- selector@
selector :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> IO Selector
selector nsAccessibilityCustomAction  =
    fmap (Selector . castPtr) $ sendMsg nsAccessibilityCustomAction (mkSelector "selector") (retPtr retVoid) []

-- | @- setSelector:@
setSelector :: IsNSAccessibilityCustomAction nsAccessibilityCustomAction => nsAccessibilityCustomAction -> Selector -> IO ()
setSelector nsAccessibilityCustomAction  value =
    sendMsg nsAccessibilityCustomAction (mkSelector "setSelector:") retVoid [argPtr (unSelector value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:handler:@
initWithName_handlerSelector :: Selector
initWithName_handlerSelector = mkSelector "initWithName:handler:"

-- | @Selector@ for @initWithName:target:selector:@
initWithName_target_selectorSelector :: Selector
initWithName_target_selectorSelector = mkSelector "initWithName:target:selector:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @handler@
handlerSelector :: Selector
handlerSelector = mkSelector "handler"

-- | @Selector@ for @setHandler:@
setHandlerSelector :: Selector
setHandlerSelector = mkSelector "setHandler:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @selector@
selectorSelector :: Selector
selectorSelector = mkSelector "selector"

-- | @Selector@ for @setSelector:@
setSelectorSelector :: Selector
setSelectorSelector = mkSelector "setSelector:"

