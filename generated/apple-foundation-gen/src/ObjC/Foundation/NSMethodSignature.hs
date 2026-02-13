{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMethodSignature@.
module ObjC.Foundation.NSMethodSignature
  ( NSMethodSignature
  , IsNSMethodSignature(..)
  , signatureWithObjCTypes
  , getArgumentTypeAtIndex
  , isOneway
  , numberOfArguments
  , frameLength
  , methodReturnType
  , methodReturnLength
  , frameLengthSelector
  , getArgumentTypeAtIndexSelector
  , isOnewaySelector
  , methodReturnLengthSelector
  , methodReturnTypeSelector
  , numberOfArgumentsSelector
  , signatureWithObjCTypesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ signatureWithObjCTypes:@
signatureWithObjCTypes :: Const (Ptr CChar) -> IO (Id NSMethodSignature)
signatureWithObjCTypes types =
  do
    cls' <- getRequiredClass "NSMethodSignature"
    sendClassMessage cls' signatureWithObjCTypesSelector types

-- | @- getArgumentTypeAtIndex:@
getArgumentTypeAtIndex :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> CULong -> IO (Const (Ptr CChar))
getArgumentTypeAtIndex nsMethodSignature idx =
  sendMessage nsMethodSignature getArgumentTypeAtIndexSelector idx

-- | @- isOneway@
isOneway :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO Bool
isOneway nsMethodSignature =
  sendMessage nsMethodSignature isOnewaySelector

-- | @- numberOfArguments@
numberOfArguments :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO CULong
numberOfArguments nsMethodSignature =
  sendMessage nsMethodSignature numberOfArgumentsSelector

-- | @- frameLength@
frameLength :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO CULong
frameLength nsMethodSignature =
  sendMessage nsMethodSignature frameLengthSelector

-- | @- methodReturnType@
methodReturnType :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO (Ptr CChar)
methodReturnType nsMethodSignature =
  sendMessage nsMethodSignature methodReturnTypeSelector

-- | @- methodReturnLength@
methodReturnLength :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO CULong
methodReturnLength nsMethodSignature =
  sendMessage nsMethodSignature methodReturnLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signatureWithObjCTypes:@
signatureWithObjCTypesSelector :: Selector '[Const (Ptr CChar)] (Id NSMethodSignature)
signatureWithObjCTypesSelector = mkSelector "signatureWithObjCTypes:"

-- | @Selector@ for @getArgumentTypeAtIndex:@
getArgumentTypeAtIndexSelector :: Selector '[CULong] (Const (Ptr CChar))
getArgumentTypeAtIndexSelector = mkSelector "getArgumentTypeAtIndex:"

-- | @Selector@ for @isOneway@
isOnewaySelector :: Selector '[] Bool
isOnewaySelector = mkSelector "isOneway"

-- | @Selector@ for @numberOfArguments@
numberOfArgumentsSelector :: Selector '[] CULong
numberOfArgumentsSelector = mkSelector "numberOfArguments"

-- | @Selector@ for @frameLength@
frameLengthSelector :: Selector '[] CULong
frameLengthSelector = mkSelector "frameLength"

-- | @Selector@ for @methodReturnType@
methodReturnTypeSelector :: Selector '[] (Ptr CChar)
methodReturnTypeSelector = mkSelector "methodReturnType"

-- | @Selector@ for @methodReturnLength@
methodReturnLengthSelector :: Selector '[] CULong
methodReturnLengthSelector = mkSelector "methodReturnLength"

