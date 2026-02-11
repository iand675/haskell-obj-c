{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INParameter@.
module ObjC.Intents.INParameter
  ( INParameter
  , IsINParameter(..)
  , parameterForClass_keyPath
  , isEqualToParameter
  , setIndex_forSubKeyPath
  , indexForSubKeyPath
  , parameterClass
  , parameterKeyPath
  , parameterForClass_keyPathSelector
  , isEqualToParameterSelector
  , setIndex_forSubKeyPathSelector
  , indexForSubKeyPathSelector
  , parameterClassSelector
  , parameterKeyPathSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ parameterForClass:keyPath:@
parameterForClass_keyPath :: IsNSString keyPath => Class -> keyPath -> IO (Id INParameter)
parameterForClass_keyPath aClass keyPath =
  do
    cls' <- getRequiredClass "INParameter"
    withObjCPtr keyPath $ \raw_keyPath ->
      sendClassMsg cls' (mkSelector "parameterForClass:keyPath:") (retPtr retVoid) [argPtr (unClass aClass), argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- isEqualToParameter:@
isEqualToParameter :: (IsINParameter inParameter, IsINParameter parameter) => inParameter -> parameter -> IO Bool
isEqualToParameter inParameter  parameter =
withObjCPtr parameter $ \raw_parameter ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg inParameter (mkSelector "isEqualToParameter:") retCULong [argPtr (castPtr raw_parameter :: Ptr ())]

-- | @- setIndex:forSubKeyPath:@
setIndex_forSubKeyPath :: (IsINParameter inParameter, IsNSString subKeyPath) => inParameter -> CULong -> subKeyPath -> IO ()
setIndex_forSubKeyPath inParameter  index subKeyPath =
withObjCPtr subKeyPath $ \raw_subKeyPath ->
    sendMsg inParameter (mkSelector "setIndex:forSubKeyPath:") retVoid [argCULong (fromIntegral index), argPtr (castPtr raw_subKeyPath :: Ptr ())]

-- | @- indexForSubKeyPath:@
indexForSubKeyPath :: (IsINParameter inParameter, IsNSString subKeyPath) => inParameter -> subKeyPath -> IO CULong
indexForSubKeyPath inParameter  subKeyPath =
withObjCPtr subKeyPath $ \raw_subKeyPath ->
    sendMsg inParameter (mkSelector "indexForSubKeyPath:") retCULong [argPtr (castPtr raw_subKeyPath :: Ptr ())]

-- | @- parameterClass@
parameterClass :: IsINParameter inParameter => inParameter -> IO Class
parameterClass inParameter  =
  fmap (Class . castPtr) $ sendMsg inParameter (mkSelector "parameterClass") (retPtr retVoid) []

-- | @- parameterKeyPath@
parameterKeyPath :: IsINParameter inParameter => inParameter -> IO (Id NSString)
parameterKeyPath inParameter  =
  sendMsg inParameter (mkSelector "parameterKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterForClass:keyPath:@
parameterForClass_keyPathSelector :: Selector
parameterForClass_keyPathSelector = mkSelector "parameterForClass:keyPath:"

-- | @Selector@ for @isEqualToParameter:@
isEqualToParameterSelector :: Selector
isEqualToParameterSelector = mkSelector "isEqualToParameter:"

-- | @Selector@ for @setIndex:forSubKeyPath:@
setIndex_forSubKeyPathSelector :: Selector
setIndex_forSubKeyPathSelector = mkSelector "setIndex:forSubKeyPath:"

-- | @Selector@ for @indexForSubKeyPath:@
indexForSubKeyPathSelector :: Selector
indexForSubKeyPathSelector = mkSelector "indexForSubKeyPath:"

-- | @Selector@ for @parameterClass@
parameterClassSelector :: Selector
parameterClassSelector = mkSelector "parameterClass"

-- | @Selector@ for @parameterKeyPath@
parameterKeyPathSelector :: Selector
parameterKeyPathSelector = mkSelector "parameterKeyPath"

