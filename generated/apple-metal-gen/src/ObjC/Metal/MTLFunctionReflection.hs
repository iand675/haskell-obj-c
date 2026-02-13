{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provides a list of inputs and outputs of the function.
--
-- ObjC selector: @- bindings@
bindings :: IsMTLFunctionReflection mtlFunctionReflection => mtlFunctionReflection -> IO (Id NSArray)
bindings mtlFunctionReflection =
  sendMessage mtlFunctionReflection bindingsSelector

-- | The string passed to the user annotation attribute for this function. Null if no user annotation is present for this function.
--
-- ObjC selector: @- userAnnotation@
userAnnotation :: IsMTLFunctionReflection mtlFunctionReflection => mtlFunctionReflection -> IO (Id NSString)
userAnnotation mtlFunctionReflection =
  sendMessage mtlFunctionReflection userAnnotationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindings@
bindingsSelector :: Selector '[] (Id NSArray)
bindingsSelector = mkSelector "bindings"

-- | @Selector@ for @userAnnotation@
userAnnotationSelector :: Selector '[] (Id NSString)
userAnnotationSelector = mkSelector "userAnnotation"

