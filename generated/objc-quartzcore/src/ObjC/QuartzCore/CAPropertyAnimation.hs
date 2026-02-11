{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclass for property-based animations. *
--
-- Generated bindings for @CAPropertyAnimation@.
module ObjC.QuartzCore.CAPropertyAnimation
  ( CAPropertyAnimation
  , IsCAPropertyAnimation(..)
  , animationWithKeyPath
  , keyPath
  , setKeyPath
  , additive
  , setAdditive
  , cumulative
  , setCumulative
  , valueFunction
  , setValueFunction
  , animationWithKeyPathSelector
  , keyPathSelector
  , setKeyPathSelector
  , additiveSelector
  , setAdditiveSelector
  , cumulativeSelector
  , setCumulativeSelector
  , valueFunctionSelector
  , setValueFunctionSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ animationWithKeyPath:@
animationWithKeyPath :: IsNSString path => path -> IO (Id CAPropertyAnimation)
animationWithKeyPath path =
  do
    cls' <- getRequiredClass "CAPropertyAnimation"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "animationWithKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- keyPath@
keyPath :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO (Id NSString)
keyPath caPropertyAnimation  =
  sendMsg caPropertyAnimation (mkSelector "keyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyPath:@
setKeyPath :: (IsCAPropertyAnimation caPropertyAnimation, IsNSString value) => caPropertyAnimation -> value -> IO ()
setKeyPath caPropertyAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caPropertyAnimation (mkSelector "setKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- additive@
additive :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO Bool
additive caPropertyAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caPropertyAnimation (mkSelector "additive") retCULong []

-- | @- setAdditive:@
setAdditive :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> Bool -> IO ()
setAdditive caPropertyAnimation  value =
  sendMsg caPropertyAnimation (mkSelector "setAdditive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cumulative@
cumulative :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO Bool
cumulative caPropertyAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caPropertyAnimation (mkSelector "cumulative") retCULong []

-- | @- setCumulative:@
setCumulative :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> Bool -> IO ()
setCumulative caPropertyAnimation  value =
  sendMsg caPropertyAnimation (mkSelector "setCumulative:") retVoid [argCULong (if value then 1 else 0)]

-- | @- valueFunction@
valueFunction :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO (Id CAValueFunction)
valueFunction caPropertyAnimation  =
  sendMsg caPropertyAnimation (mkSelector "valueFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueFunction:@
setValueFunction :: (IsCAPropertyAnimation caPropertyAnimation, IsCAValueFunction value) => caPropertyAnimation -> value -> IO ()
setValueFunction caPropertyAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caPropertyAnimation (mkSelector "setValueFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationWithKeyPath:@
animationWithKeyPathSelector :: Selector
animationWithKeyPathSelector = mkSelector "animationWithKeyPath:"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @setKeyPath:@
setKeyPathSelector :: Selector
setKeyPathSelector = mkSelector "setKeyPath:"

-- | @Selector@ for @additive@
additiveSelector :: Selector
additiveSelector = mkSelector "additive"

-- | @Selector@ for @setAdditive:@
setAdditiveSelector :: Selector
setAdditiveSelector = mkSelector "setAdditive:"

-- | @Selector@ for @cumulative@
cumulativeSelector :: Selector
cumulativeSelector = mkSelector "cumulative"

-- | @Selector@ for @setCumulative:@
setCumulativeSelector :: Selector
setCumulativeSelector = mkSelector "setCumulative:"

-- | @Selector@ for @valueFunction@
valueFunctionSelector :: Selector
valueFunctionSelector = mkSelector "valueFunction"

-- | @Selector@ for @setValueFunction:@
setValueFunctionSelector :: Selector
setValueFunctionSelector = mkSelector "setValueFunction:"

