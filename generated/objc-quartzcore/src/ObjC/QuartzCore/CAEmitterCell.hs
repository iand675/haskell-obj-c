{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAEmitterCell@.
module ObjC.QuartzCore.CAEmitterCell
  ( CAEmitterCell
  , IsCAEmitterCell(..)
  , emitterCell
  , defaultValueForKey
  , shouldArchiveValueForKey
  , name
  , setName
  , enabled
  , setEnabled
  , birthRate
  , setBirthRate
  , lifetime
  , setLifetime
  , lifetimeRange
  , setLifetimeRange
  , emissionLatitude
  , setEmissionLatitude
  , emissionLongitude
  , setEmissionLongitude
  , emissionRange
  , setEmissionRange
  , velocity
  , setVelocity
  , velocityRange
  , setVelocityRange
  , xAcceleration
  , setXAcceleration
  , yAcceleration
  , setYAcceleration
  , zAcceleration
  , setZAcceleration
  , scale
  , setScale
  , scaleRange
  , setScaleRange
  , scaleSpeed
  , setScaleSpeed
  , spin
  , setSpin
  , spinRange
  , setSpinRange
  , color
  , setColor
  , redRange
  , setRedRange
  , greenRange
  , setGreenRange
  , blueRange
  , setBlueRange
  , alphaRange
  , setAlphaRange
  , redSpeed
  , setRedSpeed
  , greenSpeed
  , setGreenSpeed
  , blueSpeed
  , setBlueSpeed
  , alphaSpeed
  , setAlphaSpeed
  , contents
  , setContents
  , contentsScale
  , setContentsScale
  , minificationFilter
  , setMinificationFilter
  , magnificationFilter
  , setMagnificationFilter
  , minificationFilterBias
  , setMinificationFilterBias
  , emitterCells
  , setEmitterCells
  , style
  , setStyle
  , emitterCellSelector
  , defaultValueForKeySelector
  , shouldArchiveValueForKeySelector
  , nameSelector
  , setNameSelector
  , enabledSelector
  , setEnabledSelector
  , birthRateSelector
  , setBirthRateSelector
  , lifetimeSelector
  , setLifetimeSelector
  , lifetimeRangeSelector
  , setLifetimeRangeSelector
  , emissionLatitudeSelector
  , setEmissionLatitudeSelector
  , emissionLongitudeSelector
  , setEmissionLongitudeSelector
  , emissionRangeSelector
  , setEmissionRangeSelector
  , velocitySelector
  , setVelocitySelector
  , velocityRangeSelector
  , setVelocityRangeSelector
  , xAccelerationSelector
  , setXAccelerationSelector
  , yAccelerationSelector
  , setYAccelerationSelector
  , zAccelerationSelector
  , setZAccelerationSelector
  , scaleSelector
  , setScaleSelector
  , scaleRangeSelector
  , setScaleRangeSelector
  , scaleSpeedSelector
  , setScaleSpeedSelector
  , spinSelector
  , setSpinSelector
  , spinRangeSelector
  , setSpinRangeSelector
  , colorSelector
  , setColorSelector
  , redRangeSelector
  , setRedRangeSelector
  , greenRangeSelector
  , setGreenRangeSelector
  , blueRangeSelector
  , setBlueRangeSelector
  , alphaRangeSelector
  , setAlphaRangeSelector
  , redSpeedSelector
  , setRedSpeedSelector
  , greenSpeedSelector
  , setGreenSpeedSelector
  , blueSpeedSelector
  , setBlueSpeedSelector
  , alphaSpeedSelector
  , setAlphaSpeedSelector
  , contentsSelector
  , setContentsSelector
  , contentsScaleSelector
  , setContentsScaleSelector
  , minificationFilterSelector
  , setMinificationFilterSelector
  , magnificationFilterSelector
  , setMagnificationFilterSelector
  , minificationFilterBiasSelector
  , setMinificationFilterBiasSelector
  , emitterCellsSelector
  , setEmitterCellsSelector
  , styleSelector
  , setStyleSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ emitterCell@
emitterCell :: IO (Id CAEmitterCell)
emitterCell  =
  do
    cls' <- getRequiredClass "CAEmitterCell"
    sendClassMsg cls' (mkSelector "emitterCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultValueForKey:@
defaultValueForKey :: IsNSString key => key -> IO RawId
defaultValueForKey key =
  do
    cls' <- getRequiredClass "CAEmitterCell"
    withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- shouldArchiveValueForKey:@
shouldArchiveValueForKey :: (IsCAEmitterCell caEmitterCell, IsNSString key) => caEmitterCell -> key -> IO Bool
shouldArchiveValueForKey caEmitterCell  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caEmitterCell (mkSelector "shouldArchiveValueForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- name@
name :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSString)
name caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsCAEmitterCell caEmitterCell, IsNSString value) => caEmitterCell -> value -> IO ()
setName caEmitterCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterCell (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- enabled@
enabled :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO Bool
enabled caEmitterCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caEmitterCell (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsCAEmitterCell caEmitterCell => caEmitterCell -> Bool -> IO ()
setEnabled caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- birthRate@
birthRate :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
birthRate caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "birthRate") retCFloat []

-- | @- setBirthRate:@
setBirthRate :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setBirthRate caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setBirthRate:") retVoid [argCFloat (fromIntegral value)]

-- | @- lifetime@
lifetime :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
lifetime caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "lifetime") retCFloat []

-- | @- setLifetime:@
setLifetime :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setLifetime caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setLifetime:") retVoid [argCFloat (fromIntegral value)]

-- | @- lifetimeRange@
lifetimeRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
lifetimeRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "lifetimeRange") retCFloat []

-- | @- setLifetimeRange:@
setLifetimeRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setLifetimeRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setLifetimeRange:") retVoid [argCFloat (fromIntegral value)]

-- | @- emissionLatitude@
emissionLatitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
emissionLatitude caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "emissionLatitude") retCDouble []

-- | @- setEmissionLatitude:@
setEmissionLatitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setEmissionLatitude caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setEmissionLatitude:") retVoid [argCDouble (fromIntegral value)]

-- | @- emissionLongitude@
emissionLongitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
emissionLongitude caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "emissionLongitude") retCDouble []

-- | @- setEmissionLongitude:@
setEmissionLongitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setEmissionLongitude caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setEmissionLongitude:") retVoid [argCDouble (fromIntegral value)]

-- | @- emissionRange@
emissionRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
emissionRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "emissionRange") retCDouble []

-- | @- setEmissionRange:@
setEmissionRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setEmissionRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setEmissionRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- velocity@
velocity :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
velocity caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "velocity") retCDouble []

-- | @- setVelocity:@
setVelocity :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setVelocity caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | @- velocityRange@
velocityRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
velocityRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "velocityRange") retCDouble []

-- | @- setVelocityRange:@
setVelocityRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setVelocityRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setVelocityRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- xAcceleration@
xAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
xAcceleration caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "xAcceleration") retCDouble []

-- | @- setXAcceleration:@
setXAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setXAcceleration caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setXAcceleration:") retVoid [argCDouble (fromIntegral value)]

-- | @- yAcceleration@
yAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
yAcceleration caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "yAcceleration") retCDouble []

-- | @- setYAcceleration:@
setYAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setYAcceleration caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setYAcceleration:") retVoid [argCDouble (fromIntegral value)]

-- | @- zAcceleration@
zAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
zAcceleration caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "zAcceleration") retCDouble []

-- | @- setZAcceleration:@
setZAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setZAcceleration caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setZAcceleration:") retVoid [argCDouble (fromIntegral value)]

-- | @- scale@
scale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
scale caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "scale") retCDouble []

-- | @- setScale:@
setScale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setScale caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setScale:") retVoid [argCDouble (fromIntegral value)]

-- | @- scaleRange@
scaleRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
scaleRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "scaleRange") retCDouble []

-- | @- setScaleRange:@
setScaleRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setScaleRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setScaleRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- scaleSpeed@
scaleSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
scaleSpeed caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "scaleSpeed") retCDouble []

-- | @- setScaleSpeed:@
setScaleSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setScaleSpeed caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setScaleSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- spin@
spin :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
spin caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "spin") retCDouble []

-- | @- setSpin:@
setSpin :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setSpin caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setSpin:") retVoid [argCDouble (fromIntegral value)]

-- | @- spinRange@
spinRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
spinRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "spinRange") retCDouble []

-- | @- setSpinRange:@
setSpinRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setSpinRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setSpinRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- color@
color :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Ptr ())
color caEmitterCell  =
  fmap castPtr $ sendMsg caEmitterCell (mkSelector "color") (retPtr retVoid) []

-- | @- setColor:@
setColor :: IsCAEmitterCell caEmitterCell => caEmitterCell -> Ptr () -> IO ()
setColor caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setColor:") retVoid [argPtr value]

-- | @- redRange@
redRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
redRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "redRange") retCFloat []

-- | @- setRedRange:@
setRedRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setRedRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setRedRange:") retVoid [argCFloat (fromIntegral value)]

-- | @- greenRange@
greenRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
greenRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "greenRange") retCFloat []

-- | @- setGreenRange:@
setGreenRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setGreenRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setGreenRange:") retVoid [argCFloat (fromIntegral value)]

-- | @- blueRange@
blueRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
blueRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "blueRange") retCFloat []

-- | @- setBlueRange:@
setBlueRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setBlueRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setBlueRange:") retVoid [argCFloat (fromIntegral value)]

-- | @- alphaRange@
alphaRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
alphaRange caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "alphaRange") retCFloat []

-- | @- setAlphaRange:@
setAlphaRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setAlphaRange caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setAlphaRange:") retVoid [argCFloat (fromIntegral value)]

-- | @- redSpeed@
redSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
redSpeed caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "redSpeed") retCFloat []

-- | @- setRedSpeed:@
setRedSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setRedSpeed caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setRedSpeed:") retVoid [argCFloat (fromIntegral value)]

-- | @- greenSpeed@
greenSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
greenSpeed caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "greenSpeed") retCFloat []

-- | @- setGreenSpeed:@
setGreenSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setGreenSpeed caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setGreenSpeed:") retVoid [argCFloat (fromIntegral value)]

-- | @- blueSpeed@
blueSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
blueSpeed caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "blueSpeed") retCFloat []

-- | @- setBlueSpeed:@
setBlueSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setBlueSpeed caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setBlueSpeed:") retVoid [argCFloat (fromIntegral value)]

-- | @- alphaSpeed@
alphaSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
alphaSpeed caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "alphaSpeed") retCFloat []

-- | @- setAlphaSpeed:@
setAlphaSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setAlphaSpeed caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setAlphaSpeed:") retVoid [argCFloat (fromIntegral value)]

-- | @- contents@
contents :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO RawId
contents caEmitterCell  =
  fmap (RawId . castPtr) $ sendMsg caEmitterCell (mkSelector "contents") (retPtr retVoid) []

-- | @- setContents:@
setContents :: IsCAEmitterCell caEmitterCell => caEmitterCell -> RawId -> IO ()
setContents caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setContents:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- contentsScale@
contentsScale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
contentsScale caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "contentsScale") retCDouble []

-- | @- setContentsScale:@
setContentsScale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setContentsScale caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setContentsScale:") retVoid [argCDouble (fromIntegral value)]

-- | @- minificationFilter@
minificationFilter :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSString)
minificationFilter caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "minificationFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinificationFilter:@
setMinificationFilter :: (IsCAEmitterCell caEmitterCell, IsNSString value) => caEmitterCell -> value -> IO ()
setMinificationFilter caEmitterCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterCell (mkSelector "setMinificationFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- magnificationFilter@
magnificationFilter :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSString)
magnificationFilter caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "magnificationFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMagnificationFilter:@
setMagnificationFilter :: (IsCAEmitterCell caEmitterCell, IsNSString value) => caEmitterCell -> value -> IO ()
setMagnificationFilter caEmitterCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterCell (mkSelector "setMagnificationFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minificationFilterBias@
minificationFilterBias :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
minificationFilterBias caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "minificationFilterBias") retCFloat []

-- | @- setMinificationFilterBias:@
setMinificationFilterBias :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setMinificationFilterBias caEmitterCell  value =
  sendMsg caEmitterCell (mkSelector "setMinificationFilterBias:") retVoid [argCFloat (fromIntegral value)]

-- | @- emitterCells@
emitterCells :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSArray)
emitterCells caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "emitterCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmitterCells:@
setEmitterCells :: (IsCAEmitterCell caEmitterCell, IsNSArray value) => caEmitterCell -> value -> IO ()
setEmitterCells caEmitterCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterCell (mkSelector "setEmitterCells:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- style@
style :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSDictionary)
style caEmitterCell  =
  sendMsg caEmitterCell (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStyle:@
setStyle :: (IsCAEmitterCell caEmitterCell, IsNSDictionary value) => caEmitterCell -> value -> IO ()
setStyle caEmitterCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterCell (mkSelector "setStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emitterCell@
emitterCellSelector :: Selector
emitterCellSelector = mkSelector "emitterCell"

-- | @Selector@ for @defaultValueForKey:@
defaultValueForKeySelector :: Selector
defaultValueForKeySelector = mkSelector "defaultValueForKey:"

-- | @Selector@ for @shouldArchiveValueForKey:@
shouldArchiveValueForKeySelector :: Selector
shouldArchiveValueForKeySelector = mkSelector "shouldArchiveValueForKey:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @birthRate@
birthRateSelector :: Selector
birthRateSelector = mkSelector "birthRate"

-- | @Selector@ for @setBirthRate:@
setBirthRateSelector :: Selector
setBirthRateSelector = mkSelector "setBirthRate:"

-- | @Selector@ for @lifetime@
lifetimeSelector :: Selector
lifetimeSelector = mkSelector "lifetime"

-- | @Selector@ for @setLifetime:@
setLifetimeSelector :: Selector
setLifetimeSelector = mkSelector "setLifetime:"

-- | @Selector@ for @lifetimeRange@
lifetimeRangeSelector :: Selector
lifetimeRangeSelector = mkSelector "lifetimeRange"

-- | @Selector@ for @setLifetimeRange:@
setLifetimeRangeSelector :: Selector
setLifetimeRangeSelector = mkSelector "setLifetimeRange:"

-- | @Selector@ for @emissionLatitude@
emissionLatitudeSelector :: Selector
emissionLatitudeSelector = mkSelector "emissionLatitude"

-- | @Selector@ for @setEmissionLatitude:@
setEmissionLatitudeSelector :: Selector
setEmissionLatitudeSelector = mkSelector "setEmissionLatitude:"

-- | @Selector@ for @emissionLongitude@
emissionLongitudeSelector :: Selector
emissionLongitudeSelector = mkSelector "emissionLongitude"

-- | @Selector@ for @setEmissionLongitude:@
setEmissionLongitudeSelector :: Selector
setEmissionLongitudeSelector = mkSelector "setEmissionLongitude:"

-- | @Selector@ for @emissionRange@
emissionRangeSelector :: Selector
emissionRangeSelector = mkSelector "emissionRange"

-- | @Selector@ for @setEmissionRange:@
setEmissionRangeSelector :: Selector
setEmissionRangeSelector = mkSelector "setEmissionRange:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @velocityRange@
velocityRangeSelector :: Selector
velocityRangeSelector = mkSelector "velocityRange"

-- | @Selector@ for @setVelocityRange:@
setVelocityRangeSelector :: Selector
setVelocityRangeSelector = mkSelector "setVelocityRange:"

-- | @Selector@ for @xAcceleration@
xAccelerationSelector :: Selector
xAccelerationSelector = mkSelector "xAcceleration"

-- | @Selector@ for @setXAcceleration:@
setXAccelerationSelector :: Selector
setXAccelerationSelector = mkSelector "setXAcceleration:"

-- | @Selector@ for @yAcceleration@
yAccelerationSelector :: Selector
yAccelerationSelector = mkSelector "yAcceleration"

-- | @Selector@ for @setYAcceleration:@
setYAccelerationSelector :: Selector
setYAccelerationSelector = mkSelector "setYAcceleration:"

-- | @Selector@ for @zAcceleration@
zAccelerationSelector :: Selector
zAccelerationSelector = mkSelector "zAcceleration"

-- | @Selector@ for @setZAcceleration:@
setZAccelerationSelector :: Selector
setZAccelerationSelector = mkSelector "setZAcceleration:"

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @scaleRange@
scaleRangeSelector :: Selector
scaleRangeSelector = mkSelector "scaleRange"

-- | @Selector@ for @setScaleRange:@
setScaleRangeSelector :: Selector
setScaleRangeSelector = mkSelector "setScaleRange:"

-- | @Selector@ for @scaleSpeed@
scaleSpeedSelector :: Selector
scaleSpeedSelector = mkSelector "scaleSpeed"

-- | @Selector@ for @setScaleSpeed:@
setScaleSpeedSelector :: Selector
setScaleSpeedSelector = mkSelector "setScaleSpeed:"

-- | @Selector@ for @spin@
spinSelector :: Selector
spinSelector = mkSelector "spin"

-- | @Selector@ for @setSpin:@
setSpinSelector :: Selector
setSpinSelector = mkSelector "setSpin:"

-- | @Selector@ for @spinRange@
spinRangeSelector :: Selector
spinRangeSelector = mkSelector "spinRange"

-- | @Selector@ for @setSpinRange:@
setSpinRangeSelector :: Selector
setSpinRangeSelector = mkSelector "setSpinRange:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @redRange@
redRangeSelector :: Selector
redRangeSelector = mkSelector "redRange"

-- | @Selector@ for @setRedRange:@
setRedRangeSelector :: Selector
setRedRangeSelector = mkSelector "setRedRange:"

-- | @Selector@ for @greenRange@
greenRangeSelector :: Selector
greenRangeSelector = mkSelector "greenRange"

-- | @Selector@ for @setGreenRange:@
setGreenRangeSelector :: Selector
setGreenRangeSelector = mkSelector "setGreenRange:"

-- | @Selector@ for @blueRange@
blueRangeSelector :: Selector
blueRangeSelector = mkSelector "blueRange"

-- | @Selector@ for @setBlueRange:@
setBlueRangeSelector :: Selector
setBlueRangeSelector = mkSelector "setBlueRange:"

-- | @Selector@ for @alphaRange@
alphaRangeSelector :: Selector
alphaRangeSelector = mkSelector "alphaRange"

-- | @Selector@ for @setAlphaRange:@
setAlphaRangeSelector :: Selector
setAlphaRangeSelector = mkSelector "setAlphaRange:"

-- | @Selector@ for @redSpeed@
redSpeedSelector :: Selector
redSpeedSelector = mkSelector "redSpeed"

-- | @Selector@ for @setRedSpeed:@
setRedSpeedSelector :: Selector
setRedSpeedSelector = mkSelector "setRedSpeed:"

-- | @Selector@ for @greenSpeed@
greenSpeedSelector :: Selector
greenSpeedSelector = mkSelector "greenSpeed"

-- | @Selector@ for @setGreenSpeed:@
setGreenSpeedSelector :: Selector
setGreenSpeedSelector = mkSelector "setGreenSpeed:"

-- | @Selector@ for @blueSpeed@
blueSpeedSelector :: Selector
blueSpeedSelector = mkSelector "blueSpeed"

-- | @Selector@ for @setBlueSpeed:@
setBlueSpeedSelector :: Selector
setBlueSpeedSelector = mkSelector "setBlueSpeed:"

-- | @Selector@ for @alphaSpeed@
alphaSpeedSelector :: Selector
alphaSpeedSelector = mkSelector "alphaSpeed"

-- | @Selector@ for @setAlphaSpeed:@
setAlphaSpeedSelector :: Selector
setAlphaSpeedSelector = mkSelector "setAlphaSpeed:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @contentsScale@
contentsScaleSelector :: Selector
contentsScaleSelector = mkSelector "contentsScale"

-- | @Selector@ for @setContentsScale:@
setContentsScaleSelector :: Selector
setContentsScaleSelector = mkSelector "setContentsScale:"

-- | @Selector@ for @minificationFilter@
minificationFilterSelector :: Selector
minificationFilterSelector = mkSelector "minificationFilter"

-- | @Selector@ for @setMinificationFilter:@
setMinificationFilterSelector :: Selector
setMinificationFilterSelector = mkSelector "setMinificationFilter:"

-- | @Selector@ for @magnificationFilter@
magnificationFilterSelector :: Selector
magnificationFilterSelector = mkSelector "magnificationFilter"

-- | @Selector@ for @setMagnificationFilter:@
setMagnificationFilterSelector :: Selector
setMagnificationFilterSelector = mkSelector "setMagnificationFilter:"

-- | @Selector@ for @minificationFilterBias@
minificationFilterBiasSelector :: Selector
minificationFilterBiasSelector = mkSelector "minificationFilterBias"

-- | @Selector@ for @setMinificationFilterBias:@
setMinificationFilterBiasSelector :: Selector
setMinificationFilterBiasSelector = mkSelector "setMinificationFilterBias:"

-- | @Selector@ for @emitterCells@
emitterCellsSelector :: Selector
emitterCellsSelector = mkSelector "emitterCells"

-- | @Selector@ for @setEmitterCells:@
setEmitterCellsSelector :: Selector
setEmitterCellsSelector = mkSelector "setEmitterCells:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

