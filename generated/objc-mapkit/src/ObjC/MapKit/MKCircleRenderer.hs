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
  , initWithCircleSelector
  , circleSelector
  , strokeStartSelector
  , setStrokeStartSelector
  , strokeEndSelector
  , setStrokeEndSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCircle:@
initWithCircle :: (IsMKCircleRenderer mkCircleRenderer, IsMKCircle circle) => mkCircleRenderer -> circle -> IO (Id MKCircleRenderer)
initWithCircle mkCircleRenderer  circle =
withObjCPtr circle $ \raw_circle ->
    sendMsg mkCircleRenderer (mkSelector "initWithCircle:") (retPtr retVoid) [argPtr (castPtr raw_circle :: Ptr ())] >>= ownedObject . castPtr

-- | @- circle@
circle :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> IO (Id MKCircle)
circle mkCircleRenderer  =
  sendMsg mkCircleRenderer (mkSelector "circle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- strokeStart@
strokeStart :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> IO CDouble
strokeStart mkCircleRenderer  =
  sendMsg mkCircleRenderer (mkSelector "strokeStart") retCDouble []

-- | @- setStrokeStart:@
setStrokeStart :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> CDouble -> IO ()
setStrokeStart mkCircleRenderer  value =
  sendMsg mkCircleRenderer (mkSelector "setStrokeStart:") retVoid [argCDouble (fromIntegral value)]

-- | @- strokeEnd@
strokeEnd :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> IO CDouble
strokeEnd mkCircleRenderer  =
  sendMsg mkCircleRenderer (mkSelector "strokeEnd") retCDouble []

-- | @- setStrokeEnd:@
setStrokeEnd :: IsMKCircleRenderer mkCircleRenderer => mkCircleRenderer -> CDouble -> IO ()
setStrokeEnd mkCircleRenderer  value =
  sendMsg mkCircleRenderer (mkSelector "setStrokeEnd:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCircle:@
initWithCircleSelector :: Selector
initWithCircleSelector = mkSelector "initWithCircle:"

-- | @Selector@ for @circle@
circleSelector :: Selector
circleSelector = mkSelector "circle"

-- | @Selector@ for @strokeStart@
strokeStartSelector :: Selector
strokeStartSelector = mkSelector "strokeStart"

-- | @Selector@ for @setStrokeStart:@
setStrokeStartSelector :: Selector
setStrokeStartSelector = mkSelector "setStrokeStart:"

-- | @Selector@ for @strokeEnd@
strokeEndSelector :: Selector
strokeEndSelector = mkSelector "strokeEnd"

-- | @Selector@ for @setStrokeEnd:@
setStrokeEndSelector :: Selector
setStrokeEndSelector = mkSelector "setStrokeEnd:"

