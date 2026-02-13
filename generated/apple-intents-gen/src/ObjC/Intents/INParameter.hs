{-# LANGUAGE DataKinds #-}
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
  , indexForSubKeyPathSelector
  , isEqualToParameterSelector
  , parameterClassSelector
  , parameterForClass_keyPathSelector
  , parameterKeyPathSelector
  , setIndex_forSubKeyPathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ parameterForClass:keyPath:@
parameterForClass_keyPath :: IsNSString keyPath => Class -> keyPath -> IO (Id INParameter)
parameterForClass_keyPath aClass keyPath =
  do
    cls' <- getRequiredClass "INParameter"
    sendClassMessage cls' parameterForClass_keyPathSelector aClass (toNSString keyPath)

-- | @- isEqualToParameter:@
isEqualToParameter :: (IsINParameter inParameter, IsINParameter parameter) => inParameter -> parameter -> IO Bool
isEqualToParameter inParameter parameter =
  sendMessage inParameter isEqualToParameterSelector (toINParameter parameter)

-- | @- setIndex:forSubKeyPath:@
setIndex_forSubKeyPath :: (IsINParameter inParameter, IsNSString subKeyPath) => inParameter -> CULong -> subKeyPath -> IO ()
setIndex_forSubKeyPath inParameter index subKeyPath =
  sendMessage inParameter setIndex_forSubKeyPathSelector index (toNSString subKeyPath)

-- | @- indexForSubKeyPath:@
indexForSubKeyPath :: (IsINParameter inParameter, IsNSString subKeyPath) => inParameter -> subKeyPath -> IO CULong
indexForSubKeyPath inParameter subKeyPath =
  sendMessage inParameter indexForSubKeyPathSelector (toNSString subKeyPath)

-- | @- parameterClass@
parameterClass :: IsINParameter inParameter => inParameter -> IO Class
parameterClass inParameter =
  sendMessage inParameter parameterClassSelector

-- | @- parameterKeyPath@
parameterKeyPath :: IsINParameter inParameter => inParameter -> IO (Id NSString)
parameterKeyPath inParameter =
  sendMessage inParameter parameterKeyPathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterForClass:keyPath:@
parameterForClass_keyPathSelector :: Selector '[Class, Id NSString] (Id INParameter)
parameterForClass_keyPathSelector = mkSelector "parameterForClass:keyPath:"

-- | @Selector@ for @isEqualToParameter:@
isEqualToParameterSelector :: Selector '[Id INParameter] Bool
isEqualToParameterSelector = mkSelector "isEqualToParameter:"

-- | @Selector@ for @setIndex:forSubKeyPath:@
setIndex_forSubKeyPathSelector :: Selector '[CULong, Id NSString] ()
setIndex_forSubKeyPathSelector = mkSelector "setIndex:forSubKeyPath:"

-- | @Selector@ for @indexForSubKeyPath:@
indexForSubKeyPathSelector :: Selector '[Id NSString] CULong
indexForSubKeyPathSelector = mkSelector "indexForSubKeyPath:"

-- | @Selector@ for @parameterClass@
parameterClassSelector :: Selector '[] Class
parameterClassSelector = mkSelector "parameterClass"

-- | @Selector@ for @parameterKeyPath@
parameterKeyPathSelector :: Selector '[] (Id NSString)
parameterKeyPathSelector = mkSelector "parameterKeyPath"

