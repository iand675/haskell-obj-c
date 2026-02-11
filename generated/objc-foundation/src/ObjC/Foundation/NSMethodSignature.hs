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
  , methodReturnLength
  , signatureWithObjCTypesSelector
  , getArgumentTypeAtIndexSelector
  , isOnewaySelector
  , numberOfArgumentsSelector
  , frameLengthSelector
  , methodReturnLengthSelector


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

-- | @+ signatureWithObjCTypes:@
signatureWithObjCTypes :: Const (Ptr CChar) -> IO (Id NSMethodSignature)
signatureWithObjCTypes types =
  do
    cls' <- getRequiredClass "NSMethodSignature"
    sendClassMsg cls' (mkSelector "signatureWithObjCTypes:") (retPtr retVoid) [argPtr (unConst types)] >>= retainedObject . castPtr

-- | @- getArgumentTypeAtIndex:@
getArgumentTypeAtIndex :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> CULong -> IO (Const (Ptr CChar))
getArgumentTypeAtIndex nsMethodSignature  idx =
  fmap Const $ fmap castPtr $ sendMsg nsMethodSignature (mkSelector "getArgumentTypeAtIndex:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- isOneway@
isOneway :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO Bool
isOneway nsMethodSignature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMethodSignature (mkSelector "isOneway") retCULong []

-- | @- numberOfArguments@
numberOfArguments :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO CULong
numberOfArguments nsMethodSignature  =
  sendMsg nsMethodSignature (mkSelector "numberOfArguments") retCULong []

-- | @- frameLength@
frameLength :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO CULong
frameLength nsMethodSignature  =
  sendMsg nsMethodSignature (mkSelector "frameLength") retCULong []

-- | @- methodReturnLength@
methodReturnLength :: IsNSMethodSignature nsMethodSignature => nsMethodSignature -> IO CULong
methodReturnLength nsMethodSignature  =
  sendMsg nsMethodSignature (mkSelector "methodReturnLength") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signatureWithObjCTypes:@
signatureWithObjCTypesSelector :: Selector
signatureWithObjCTypesSelector = mkSelector "signatureWithObjCTypes:"

-- | @Selector@ for @getArgumentTypeAtIndex:@
getArgumentTypeAtIndexSelector :: Selector
getArgumentTypeAtIndexSelector = mkSelector "getArgumentTypeAtIndex:"

-- | @Selector@ for @isOneway@
isOnewaySelector :: Selector
isOnewaySelector = mkSelector "isOneway"

-- | @Selector@ for @numberOfArguments@
numberOfArgumentsSelector :: Selector
numberOfArgumentsSelector = mkSelector "numberOfArguments"

-- | @Selector@ for @frameLength@
frameLengthSelector :: Selector
frameLengthSelector = mkSelector "frameLength"

-- | @Selector@ for @methodReturnLength@
methodReturnLengthSelector :: Selector
methodReturnLengthSelector = mkSelector "methodReturnLength"

