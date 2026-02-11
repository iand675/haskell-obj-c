{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a reflection object containing information about a function in a Metal library.
--
-- Generated bindings for @MTLFunctionReflection@.
module ObjC.Metal.MTLFunctionReflection
  ( MTLFunctionReflection
  , IsMTLFunctionReflection(..)
  , bindings
  , userAnnotation
  , bindingsSelector
  , userAnnotationSelector


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

-- | Provides a list of inputs and outputs of the function.
--
-- ObjC selector: @- bindings@
bindings :: IsMTLFunctionReflection mtlFunctionReflection => mtlFunctionReflection -> IO (Id NSArray)
bindings mtlFunctionReflection  =
    sendMsg mtlFunctionReflection (mkSelector "bindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The string passed to the user annotation attribute for this function. Null if no user annotation is present for this function.
--
-- ObjC selector: @- userAnnotation@
userAnnotation :: IsMTLFunctionReflection mtlFunctionReflection => mtlFunctionReflection -> IO (Id NSString)
userAnnotation mtlFunctionReflection  =
    sendMsg mtlFunctionReflection (mkSelector "userAnnotation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindings@
bindingsSelector :: Selector
bindingsSelector = mkSelector "bindings"

-- | @Selector@ for @userAnnotation@
userAnnotationSelector :: Selector
userAnnotationSelector = mkSelector "userAnnotation"

