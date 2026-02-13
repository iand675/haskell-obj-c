{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptCoercionHandler@.
module ObjC.Foundation.NSScriptCoercionHandler
  ( NSScriptCoercionHandler
  , IsNSScriptCoercionHandler(..)
  , sharedCoercionHandler
  , coerceValue_toClass
  , registerCoercer_selector_toConvertFromClass_toClass
  , coerceValue_toClassSelector
  , registerCoercer_selector_toConvertFromClass_toClassSelector
  , sharedCoercionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sharedCoercionHandler@
sharedCoercionHandler :: IO (Id NSScriptCoercionHandler)
sharedCoercionHandler  =
  do
    cls' <- getRequiredClass "NSScriptCoercionHandler"
    sendClassMessage cls' sharedCoercionHandlerSelector

-- | @- coerceValue:toClass:@
coerceValue_toClass :: IsNSScriptCoercionHandler nsScriptCoercionHandler => nsScriptCoercionHandler -> RawId -> Class -> IO RawId
coerceValue_toClass nsScriptCoercionHandler value toClass =
  sendMessage nsScriptCoercionHandler coerceValue_toClassSelector value toClass

-- | @- registerCoercer:selector:toConvertFromClass:toClass:@
registerCoercer_selector_toConvertFromClass_toClass :: IsNSScriptCoercionHandler nsScriptCoercionHandler => nsScriptCoercionHandler -> RawId -> Sel -> Class -> Class -> IO ()
registerCoercer_selector_toConvertFromClass_toClass nsScriptCoercionHandler coercer selector fromClass toClass =
  sendMessage nsScriptCoercionHandler registerCoercer_selector_toConvertFromClass_toClassSelector coercer selector fromClass toClass

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCoercionHandler@
sharedCoercionHandlerSelector :: Selector '[] (Id NSScriptCoercionHandler)
sharedCoercionHandlerSelector = mkSelector "sharedCoercionHandler"

-- | @Selector@ for @coerceValue:toClass:@
coerceValue_toClassSelector :: Selector '[RawId, Class] RawId
coerceValue_toClassSelector = mkSelector "coerceValue:toClass:"

-- | @Selector@ for @registerCoercer:selector:toConvertFromClass:toClass:@
registerCoercer_selector_toConvertFromClass_toClassSelector :: Selector '[RawId, Sel, Class, Class] ()
registerCoercer_selector_toConvertFromClass_toClassSelector = mkSelector "registerCoercer:selector:toConvertFromClass:toClass:"

