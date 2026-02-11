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
  , sharedCoercionHandlerSelector
  , coerceValue_toClassSelector
  , registerCoercer_selector_toConvertFromClass_toClassSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ sharedCoercionHandler@
sharedCoercionHandler :: IO (Id NSScriptCoercionHandler)
sharedCoercionHandler  =
  do
    cls' <- getRequiredClass "NSScriptCoercionHandler"
    sendClassMsg cls' (mkSelector "sharedCoercionHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- coerceValue:toClass:@
coerceValue_toClass :: IsNSScriptCoercionHandler nsScriptCoercionHandler => nsScriptCoercionHandler -> RawId -> Class -> IO RawId
coerceValue_toClass nsScriptCoercionHandler  value toClass =
  fmap (RawId . castPtr) $ sendMsg nsScriptCoercionHandler (mkSelector "coerceValue:toClass:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (unClass toClass)]

-- | @- registerCoercer:selector:toConvertFromClass:toClass:@
registerCoercer_selector_toConvertFromClass_toClass :: IsNSScriptCoercionHandler nsScriptCoercionHandler => nsScriptCoercionHandler -> RawId -> Selector -> Class -> Class -> IO ()
registerCoercer_selector_toConvertFromClass_toClass nsScriptCoercionHandler  coercer selector fromClass toClass =
  sendMsg nsScriptCoercionHandler (mkSelector "registerCoercer:selector:toConvertFromClass:toClass:") retVoid [argPtr (castPtr (unRawId coercer) :: Ptr ()), argPtr (unSelector selector), argPtr (unClass fromClass), argPtr (unClass toClass)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCoercionHandler@
sharedCoercionHandlerSelector :: Selector
sharedCoercionHandlerSelector = mkSelector "sharedCoercionHandler"

-- | @Selector@ for @coerceValue:toClass:@
coerceValue_toClassSelector :: Selector
coerceValue_toClassSelector = mkSelector "coerceValue:toClass:"

-- | @Selector@ for @registerCoercer:selector:toConvertFromClass:toClass:@
registerCoercer_selector_toConvertFromClass_toClassSelector :: Selector
registerCoercer_selector_toConvertFromClass_toClassSelector = mkSelector "registerCoercer:selector:toConvertFromClass:toClass:"

