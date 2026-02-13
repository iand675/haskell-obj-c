{-# LANGUAGE DataKinds #-}
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
  , additiveSelector
  , animationWithKeyPathSelector
  , cumulativeSelector
  , keyPathSelector
  , setAdditiveSelector
  , setCumulativeSelector
  , setKeyPathSelector
  , setValueFunctionSelector
  , valueFunctionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ animationWithKeyPath:@
animationWithKeyPath :: IsNSString path => path -> IO (Id CAPropertyAnimation)
animationWithKeyPath path =
  do
    cls' <- getRequiredClass "CAPropertyAnimation"
    sendClassMessage cls' animationWithKeyPathSelector (toNSString path)

-- | @- keyPath@
keyPath :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO (Id NSString)
keyPath caPropertyAnimation =
  sendMessage caPropertyAnimation keyPathSelector

-- | @- setKeyPath:@
setKeyPath :: (IsCAPropertyAnimation caPropertyAnimation, IsNSString value) => caPropertyAnimation -> value -> IO ()
setKeyPath caPropertyAnimation value =
  sendMessage caPropertyAnimation setKeyPathSelector (toNSString value)

-- | @- additive@
additive :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO Bool
additive caPropertyAnimation =
  sendMessage caPropertyAnimation additiveSelector

-- | @- setAdditive:@
setAdditive :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> Bool -> IO ()
setAdditive caPropertyAnimation value =
  sendMessage caPropertyAnimation setAdditiveSelector value

-- | @- cumulative@
cumulative :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO Bool
cumulative caPropertyAnimation =
  sendMessage caPropertyAnimation cumulativeSelector

-- | @- setCumulative:@
setCumulative :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> Bool -> IO ()
setCumulative caPropertyAnimation value =
  sendMessage caPropertyAnimation setCumulativeSelector value

-- | @- valueFunction@
valueFunction :: IsCAPropertyAnimation caPropertyAnimation => caPropertyAnimation -> IO (Id CAValueFunction)
valueFunction caPropertyAnimation =
  sendMessage caPropertyAnimation valueFunctionSelector

-- | @- setValueFunction:@
setValueFunction :: (IsCAPropertyAnimation caPropertyAnimation, IsCAValueFunction value) => caPropertyAnimation -> value -> IO ()
setValueFunction caPropertyAnimation value =
  sendMessage caPropertyAnimation setValueFunctionSelector (toCAValueFunction value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationWithKeyPath:@
animationWithKeyPathSelector :: Selector '[Id NSString] (Id CAPropertyAnimation)
animationWithKeyPathSelector = mkSelector "animationWithKeyPath:"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector '[] (Id NSString)
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @setKeyPath:@
setKeyPathSelector :: Selector '[Id NSString] ()
setKeyPathSelector = mkSelector "setKeyPath:"

-- | @Selector@ for @additive@
additiveSelector :: Selector '[] Bool
additiveSelector = mkSelector "additive"

-- | @Selector@ for @setAdditive:@
setAdditiveSelector :: Selector '[Bool] ()
setAdditiveSelector = mkSelector "setAdditive:"

-- | @Selector@ for @cumulative@
cumulativeSelector :: Selector '[] Bool
cumulativeSelector = mkSelector "cumulative"

-- | @Selector@ for @setCumulative:@
setCumulativeSelector :: Selector '[Bool] ()
setCumulativeSelector = mkSelector "setCumulative:"

-- | @Selector@ for @valueFunction@
valueFunctionSelector :: Selector '[] (Id CAValueFunction)
valueFunctionSelector = mkSelector "valueFunction"

-- | @Selector@ for @setValueFunction:@
setValueFunctionSelector :: Selector '[Id CAValueFunction] ()
setValueFunctionSelector = mkSelector "setValueFunction:"

