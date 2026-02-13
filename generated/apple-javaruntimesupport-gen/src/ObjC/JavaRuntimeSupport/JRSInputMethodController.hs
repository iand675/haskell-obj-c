{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSInputMethodController@.
module ObjC.JavaRuntimeSupport.JRSInputMethodController
  ( JRSInputMethodController
  , IsJRSInputMethodController(..)
  , controller
  , availableInputMethodLocales
  , currentInputMethodName
  , currentInputMethodLocale
  , setCurrentInputMethodForLocale
  , availableInputMethodLocalesSelector
  , controllerSelector
  , currentInputMethodLocaleSelector
  , currentInputMethodNameSelector
  , setCurrentInputMethodForLocaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ controller@
controller :: IO (Id JRSInputMethodController)
controller  =
  do
    cls' <- getRequiredClass "JRSInputMethodController"
    sendClassMessage cls' controllerSelector

-- | @- availableInputMethodLocales@
availableInputMethodLocales :: IsJRSInputMethodController jrsInputMethodController => jrsInputMethodController -> IO (Id NSArray)
availableInputMethodLocales jrsInputMethodController =
  sendMessage jrsInputMethodController availableInputMethodLocalesSelector

-- | @- currentInputMethodName@
currentInputMethodName :: IsJRSInputMethodController jrsInputMethodController => jrsInputMethodController -> IO (Id NSString)
currentInputMethodName jrsInputMethodController =
  sendMessage jrsInputMethodController currentInputMethodNameSelector

-- | @- currentInputMethodLocale@
currentInputMethodLocale :: IsJRSInputMethodController jrsInputMethodController => jrsInputMethodController -> IO (Id NSString)
currentInputMethodLocale jrsInputMethodController =
  sendMessage jrsInputMethodController currentInputMethodLocaleSelector

-- | @- setCurrentInputMethodForLocale:@
setCurrentInputMethodForLocale :: (IsJRSInputMethodController jrsInputMethodController, IsNSString theLocale) => jrsInputMethodController -> theLocale -> IO ()
setCurrentInputMethodForLocale jrsInputMethodController theLocale =
  sendMessage jrsInputMethodController setCurrentInputMethodForLocaleSelector (toNSString theLocale)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controller@
controllerSelector :: Selector '[] (Id JRSInputMethodController)
controllerSelector = mkSelector "controller"

-- | @Selector@ for @availableInputMethodLocales@
availableInputMethodLocalesSelector :: Selector '[] (Id NSArray)
availableInputMethodLocalesSelector = mkSelector "availableInputMethodLocales"

-- | @Selector@ for @currentInputMethodName@
currentInputMethodNameSelector :: Selector '[] (Id NSString)
currentInputMethodNameSelector = mkSelector "currentInputMethodName"

-- | @Selector@ for @currentInputMethodLocale@
currentInputMethodLocaleSelector :: Selector '[] (Id NSString)
currentInputMethodLocaleSelector = mkSelector "currentInputMethodLocale"

-- | @Selector@ for @setCurrentInputMethodForLocale:@
setCurrentInputMethodForLocaleSelector :: Selector '[Id NSString] ()
setCurrentInputMethodForLocaleSelector = mkSelector "setCurrentInputMethodForLocale:"

