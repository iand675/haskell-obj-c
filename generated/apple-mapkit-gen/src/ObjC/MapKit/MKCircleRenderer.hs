{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKCircleRenderer@.
module ObjC.MapKit.MKCircleRenderer
  ( MKCircleRenderer
  , IsMKCircleRenderer(..)
  , initWithCircle
  , circle
  , strokeStart
  , setStrokeStart
  , strokeEnd
  , setStrokeEnd
  , circleSelector
  , initWithCircleSelector
  , setStrokeEndSelector
  , setStrokeStartSelector
  , strokeEndSelector
  , strokeStartSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCircle:@
initWithCircle :: (IsMKCircleRenderer mkCircleRenderer, IsMKCircle circle) => mkCircleRenderer -> circle -> IO (Id MKCircleRenderer)
initWithCircle mkCircleRenderer circle =
  sendOwnedMessage mkCircleRenderer initWithCircleSelector (toMKCircle circle)

-- | @- circle@
circle :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> IO (Id MKCircle)
circle mkCircleRenderer =
  sendMessage mkCircleRenderer circleSelector

-- | @- strokeStart@
strokeStart :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> IO CDouble
strokeStart mkCircleRenderer =
  sendMessage mkCircleRenderer strokeStartSelector

-- | @- setStrokeStart:@
setStrokeStart :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> CDouble -> IO ()
setStrokeStart mkCircleRenderer value =
  sendMessage mkCircleRenderer setStrokeStartSelector value

-- | @- strokeEnd@
strokeEnd :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> IO CDouble
strokeEnd mkCircleRenderer =
  sendMessage mkCircleRenderer strokeEndSelector

-- | @- setStrokeEnd:@
setStrokeEnd :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> CDouble -> IO ()
setStrokeEnd mkCircleRenderer value =
  sendMessage mkCircleRenderer setStrokeEndSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCircle:@
initWithCircleSelector :: Selector '[Id MKCircle] (Id MKCircleRenderer)
initWithCircleSelector = mkSelector "initWithCircle:"

-- | @Selector@ for @circle@
circleSelector :: Selector '[] (Id MKCircle)
circleSelector = mkSelector "circle"

-- | @Selector@ for @strokeStart@
strokeStartSelector :: Selector '[] CDouble
strokeStartSelector = mkSelector "strokeStart"

-- | @Selector@ for @setStrokeStart:@
setStrokeStartSelector :: Selector '[CDouble] ()
setStrokeStartSelector = mkSelector "setStrokeStart:"

-- | @Selector@ for @strokeEnd@
strokeEndSelector :: Selector '[] CDouble
strokeEndSelector = mkSelector "strokeEnd"

-- | @Selector@ for @setStrokeEnd:@
setStrokeEndSelector :: Selector '[CDouble] ()
setStrokeEndSelector = mkSelector "setStrokeEnd:"

