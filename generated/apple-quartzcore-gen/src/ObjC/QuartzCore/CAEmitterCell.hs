{-# LANGUAGE DataKinds #-}
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
  , alphaRangeSelector
  , alphaSpeedSelector
  , birthRateSelector
  , blueRangeSelector
  , blueSpeedSelector
  , colorSelector
  , contentsScaleSelector
  , contentsSelector
  , defaultValueForKeySelector
  , emissionLatitudeSelector
  , emissionLongitudeSelector
  , emissionRangeSelector
  , emitterCellSelector
  , emitterCellsSelector
  , enabledSelector
  , greenRangeSelector
  , greenSpeedSelector
  , lifetimeRangeSelector
  , lifetimeSelector
  , magnificationFilterSelector
  , minificationFilterBiasSelector
  , minificationFilterSelector
  , nameSelector
  , redRangeSelector
  , redSpeedSelector
  , scaleRangeSelector
  , scaleSelector
  , scaleSpeedSelector
  , setAlphaRangeSelector
  , setAlphaSpeedSelector
  , setBirthRateSelector
  , setBlueRangeSelector
  , setBlueSpeedSelector
  , setColorSelector
  , setContentsScaleSelector
  , setContentsSelector
  , setEmissionLatitudeSelector
  , setEmissionLongitudeSelector
  , setEmissionRangeSelector
  , setEmitterCellsSelector
  , setEnabledSelector
  , setGreenRangeSelector
  , setGreenSpeedSelector
  , setLifetimeRangeSelector
  , setLifetimeSelector
  , setMagnificationFilterSelector
  , setMinificationFilterBiasSelector
  , setMinificationFilterSelector
  , setNameSelector
  , setRedRangeSelector
  , setRedSpeedSelector
  , setScaleRangeSelector
  , setScaleSelector
  , setScaleSpeedSelector
  , setSpinRangeSelector
  , setSpinSelector
  , setStyleSelector
  , setVelocityRangeSelector
  , setVelocitySelector
  , setXAccelerationSelector
  , setYAccelerationSelector
  , setZAccelerationSelector
  , shouldArchiveValueForKeySelector
  , spinRangeSelector
  , spinSelector
  , styleSelector
  , velocityRangeSelector
  , velocitySelector
  , xAccelerationSelector
  , yAccelerationSelector
  , zAccelerationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ emitterCell@
emitterCell :: IO (Id CAEmitterCell)
emitterCell  =
  do
    cls' <- getRequiredClass "CAEmitterCell"
    sendClassMessage cls' emitterCellSelector

-- | @+ defaultValueForKey:@
defaultValueForKey :: IsNSString key => key -> IO RawId
defaultValueForKey key =
  do
    cls' <- getRequiredClass "CAEmitterCell"
    sendClassMessage cls' defaultValueForKeySelector (toNSString key)

-- | @- shouldArchiveValueForKey:@
shouldArchiveValueForKey :: (IsCAEmitterCell caEmitterCell, IsNSString key) => caEmitterCell -> key -> IO Bool
shouldArchiveValueForKey caEmitterCell key =
  sendMessage caEmitterCell shouldArchiveValueForKeySelector (toNSString key)

-- | @- name@
name :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSString)
name caEmitterCell =
  sendMessage caEmitterCell nameSelector

-- | @- setName:@
setName :: (IsCAEmitterCell caEmitterCell, IsNSString value) => caEmitterCell -> value -> IO ()
setName caEmitterCell value =
  sendMessage caEmitterCell setNameSelector (toNSString value)

-- | @- enabled@
enabled :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO Bool
enabled caEmitterCell =
  sendMessage caEmitterCell enabledSelector

-- | @- setEnabled:@
setEnabled :: IsCAEmitterCell caEmitterCell => caEmitterCell -> Bool -> IO ()
setEnabled caEmitterCell value =
  sendMessage caEmitterCell setEnabledSelector value

-- | @- birthRate@
birthRate :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
birthRate caEmitterCell =
  sendMessage caEmitterCell birthRateSelector

-- | @- setBirthRate:@
setBirthRate :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setBirthRate caEmitterCell value =
  sendMessage caEmitterCell setBirthRateSelector value

-- | @- lifetime@
lifetime :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
lifetime caEmitterCell =
  sendMessage caEmitterCell lifetimeSelector

-- | @- setLifetime:@
setLifetime :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setLifetime caEmitterCell value =
  sendMessage caEmitterCell setLifetimeSelector value

-- | @- lifetimeRange@
lifetimeRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
lifetimeRange caEmitterCell =
  sendMessage caEmitterCell lifetimeRangeSelector

-- | @- setLifetimeRange:@
setLifetimeRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setLifetimeRange caEmitterCell value =
  sendMessage caEmitterCell setLifetimeRangeSelector value

-- | @- emissionLatitude@
emissionLatitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
emissionLatitude caEmitterCell =
  sendMessage caEmitterCell emissionLatitudeSelector

-- | @- setEmissionLatitude:@
setEmissionLatitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setEmissionLatitude caEmitterCell value =
  sendMessage caEmitterCell setEmissionLatitudeSelector value

-- | @- emissionLongitude@
emissionLongitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
emissionLongitude caEmitterCell =
  sendMessage caEmitterCell emissionLongitudeSelector

-- | @- setEmissionLongitude:@
setEmissionLongitude :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setEmissionLongitude caEmitterCell value =
  sendMessage caEmitterCell setEmissionLongitudeSelector value

-- | @- emissionRange@
emissionRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
emissionRange caEmitterCell =
  sendMessage caEmitterCell emissionRangeSelector

-- | @- setEmissionRange:@
setEmissionRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setEmissionRange caEmitterCell value =
  sendMessage caEmitterCell setEmissionRangeSelector value

-- | @- velocity@
velocity :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
velocity caEmitterCell =
  sendMessage caEmitterCell velocitySelector

-- | @- setVelocity:@
setVelocity :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setVelocity caEmitterCell value =
  sendMessage caEmitterCell setVelocitySelector value

-- | @- velocityRange@
velocityRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
velocityRange caEmitterCell =
  sendMessage caEmitterCell velocityRangeSelector

-- | @- setVelocityRange:@
setVelocityRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setVelocityRange caEmitterCell value =
  sendMessage caEmitterCell setVelocityRangeSelector value

-- | @- xAcceleration@
xAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
xAcceleration caEmitterCell =
  sendMessage caEmitterCell xAccelerationSelector

-- | @- setXAcceleration:@
setXAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setXAcceleration caEmitterCell value =
  sendMessage caEmitterCell setXAccelerationSelector value

-- | @- yAcceleration@
yAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
yAcceleration caEmitterCell =
  sendMessage caEmitterCell yAccelerationSelector

-- | @- setYAcceleration:@
setYAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setYAcceleration caEmitterCell value =
  sendMessage caEmitterCell setYAccelerationSelector value

-- | @- zAcceleration@
zAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
zAcceleration caEmitterCell =
  sendMessage caEmitterCell zAccelerationSelector

-- | @- setZAcceleration:@
setZAcceleration :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setZAcceleration caEmitterCell value =
  sendMessage caEmitterCell setZAccelerationSelector value

-- | @- scale@
scale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
scale caEmitterCell =
  sendMessage caEmitterCell scaleSelector

-- | @- setScale:@
setScale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setScale caEmitterCell value =
  sendMessage caEmitterCell setScaleSelector value

-- | @- scaleRange@
scaleRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
scaleRange caEmitterCell =
  sendMessage caEmitterCell scaleRangeSelector

-- | @- setScaleRange:@
setScaleRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setScaleRange caEmitterCell value =
  sendMessage caEmitterCell setScaleRangeSelector value

-- | @- scaleSpeed@
scaleSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
scaleSpeed caEmitterCell =
  sendMessage caEmitterCell scaleSpeedSelector

-- | @- setScaleSpeed:@
setScaleSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setScaleSpeed caEmitterCell value =
  sendMessage caEmitterCell setScaleSpeedSelector value

-- | @- spin@
spin :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
spin caEmitterCell =
  sendMessage caEmitterCell spinSelector

-- | @- setSpin:@
setSpin :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setSpin caEmitterCell value =
  sendMessage caEmitterCell setSpinSelector value

-- | @- spinRange@
spinRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
spinRange caEmitterCell =
  sendMessage caEmitterCell spinRangeSelector

-- | @- setSpinRange:@
setSpinRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setSpinRange caEmitterCell value =
  sendMessage caEmitterCell setSpinRangeSelector value

-- | @- color@
color :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Ptr ())
color caEmitterCell =
  sendMessage caEmitterCell colorSelector

-- | @- setColor:@
setColor :: IsCAEmitterCell caEmitterCell => caEmitterCell -> Ptr () -> IO ()
setColor caEmitterCell value =
  sendMessage caEmitterCell setColorSelector value

-- | @- redRange@
redRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
redRange caEmitterCell =
  sendMessage caEmitterCell redRangeSelector

-- | @- setRedRange:@
setRedRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setRedRange caEmitterCell value =
  sendMessage caEmitterCell setRedRangeSelector value

-- | @- greenRange@
greenRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
greenRange caEmitterCell =
  sendMessage caEmitterCell greenRangeSelector

-- | @- setGreenRange:@
setGreenRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setGreenRange caEmitterCell value =
  sendMessage caEmitterCell setGreenRangeSelector value

-- | @- blueRange@
blueRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
blueRange caEmitterCell =
  sendMessage caEmitterCell blueRangeSelector

-- | @- setBlueRange:@
setBlueRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setBlueRange caEmitterCell value =
  sendMessage caEmitterCell setBlueRangeSelector value

-- | @- alphaRange@
alphaRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
alphaRange caEmitterCell =
  sendMessage caEmitterCell alphaRangeSelector

-- | @- setAlphaRange:@
setAlphaRange :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setAlphaRange caEmitterCell value =
  sendMessage caEmitterCell setAlphaRangeSelector value

-- | @- redSpeed@
redSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
redSpeed caEmitterCell =
  sendMessage caEmitterCell redSpeedSelector

-- | @- setRedSpeed:@
setRedSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setRedSpeed caEmitterCell value =
  sendMessage caEmitterCell setRedSpeedSelector value

-- | @- greenSpeed@
greenSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
greenSpeed caEmitterCell =
  sendMessage caEmitterCell greenSpeedSelector

-- | @- setGreenSpeed:@
setGreenSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setGreenSpeed caEmitterCell value =
  sendMessage caEmitterCell setGreenSpeedSelector value

-- | @- blueSpeed@
blueSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
blueSpeed caEmitterCell =
  sendMessage caEmitterCell blueSpeedSelector

-- | @- setBlueSpeed:@
setBlueSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setBlueSpeed caEmitterCell value =
  sendMessage caEmitterCell setBlueSpeedSelector value

-- | @- alphaSpeed@
alphaSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
alphaSpeed caEmitterCell =
  sendMessage caEmitterCell alphaSpeedSelector

-- | @- setAlphaSpeed:@
setAlphaSpeed :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setAlphaSpeed caEmitterCell value =
  sendMessage caEmitterCell setAlphaSpeedSelector value

-- | @- contents@
contents :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO RawId
contents caEmitterCell =
  sendMessage caEmitterCell contentsSelector

-- | @- setContents:@
setContents :: IsCAEmitterCell caEmitterCell => caEmitterCell -> RawId -> IO ()
setContents caEmitterCell value =
  sendMessage caEmitterCell setContentsSelector value

-- | @- contentsScale@
contentsScale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CDouble
contentsScale caEmitterCell =
  sendMessage caEmitterCell contentsScaleSelector

-- | @- setContentsScale:@
setContentsScale :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CDouble -> IO ()
setContentsScale caEmitterCell value =
  sendMessage caEmitterCell setContentsScaleSelector value

-- | @- minificationFilter@
minificationFilter :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSString)
minificationFilter caEmitterCell =
  sendMessage caEmitterCell minificationFilterSelector

-- | @- setMinificationFilter:@
setMinificationFilter :: (IsCAEmitterCell caEmitterCell, IsNSString value) => caEmitterCell -> value -> IO ()
setMinificationFilter caEmitterCell value =
  sendMessage caEmitterCell setMinificationFilterSelector (toNSString value)

-- | @- magnificationFilter@
magnificationFilter :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSString)
magnificationFilter caEmitterCell =
  sendMessage caEmitterCell magnificationFilterSelector

-- | @- setMagnificationFilter:@
setMagnificationFilter :: (IsCAEmitterCell caEmitterCell, IsNSString value) => caEmitterCell -> value -> IO ()
setMagnificationFilter caEmitterCell value =
  sendMessage caEmitterCell setMagnificationFilterSelector (toNSString value)

-- | @- minificationFilterBias@
minificationFilterBias :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO CFloat
minificationFilterBias caEmitterCell =
  sendMessage caEmitterCell minificationFilterBiasSelector

-- | @- setMinificationFilterBias:@
setMinificationFilterBias :: IsCAEmitterCell caEmitterCell => caEmitterCell -> CFloat -> IO ()
setMinificationFilterBias caEmitterCell value =
  sendMessage caEmitterCell setMinificationFilterBiasSelector value

-- | @- emitterCells@
emitterCells :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSArray)
emitterCells caEmitterCell =
  sendMessage caEmitterCell emitterCellsSelector

-- | @- setEmitterCells:@
setEmitterCells :: (IsCAEmitterCell caEmitterCell, IsNSArray value) => caEmitterCell -> value -> IO ()
setEmitterCells caEmitterCell value =
  sendMessage caEmitterCell setEmitterCellsSelector (toNSArray value)

-- | @- style@
style :: IsCAEmitterCell caEmitterCell => caEmitterCell -> IO (Id NSDictionary)
style caEmitterCell =
  sendMessage caEmitterCell styleSelector

-- | @- setStyle:@
setStyle :: (IsCAEmitterCell caEmitterCell, IsNSDictionary value) => caEmitterCell -> value -> IO ()
setStyle caEmitterCell value =
  sendMessage caEmitterCell setStyleSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emitterCell@
emitterCellSelector :: Selector '[] (Id CAEmitterCell)
emitterCellSelector = mkSelector "emitterCell"

-- | @Selector@ for @defaultValueForKey:@
defaultValueForKeySelector :: Selector '[Id NSString] RawId
defaultValueForKeySelector = mkSelector "defaultValueForKey:"

-- | @Selector@ for @shouldArchiveValueForKey:@
shouldArchiveValueForKeySelector :: Selector '[Id NSString] Bool
shouldArchiveValueForKeySelector = mkSelector "shouldArchiveValueForKey:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @birthRate@
birthRateSelector :: Selector '[] CFloat
birthRateSelector = mkSelector "birthRate"

-- | @Selector@ for @setBirthRate:@
setBirthRateSelector :: Selector '[CFloat] ()
setBirthRateSelector = mkSelector "setBirthRate:"

-- | @Selector@ for @lifetime@
lifetimeSelector :: Selector '[] CFloat
lifetimeSelector = mkSelector "lifetime"

-- | @Selector@ for @setLifetime:@
setLifetimeSelector :: Selector '[CFloat] ()
setLifetimeSelector = mkSelector "setLifetime:"

-- | @Selector@ for @lifetimeRange@
lifetimeRangeSelector :: Selector '[] CFloat
lifetimeRangeSelector = mkSelector "lifetimeRange"

-- | @Selector@ for @setLifetimeRange:@
setLifetimeRangeSelector :: Selector '[CFloat] ()
setLifetimeRangeSelector = mkSelector "setLifetimeRange:"

-- | @Selector@ for @emissionLatitude@
emissionLatitudeSelector :: Selector '[] CDouble
emissionLatitudeSelector = mkSelector "emissionLatitude"

-- | @Selector@ for @setEmissionLatitude:@
setEmissionLatitudeSelector :: Selector '[CDouble] ()
setEmissionLatitudeSelector = mkSelector "setEmissionLatitude:"

-- | @Selector@ for @emissionLongitude@
emissionLongitudeSelector :: Selector '[] CDouble
emissionLongitudeSelector = mkSelector "emissionLongitude"

-- | @Selector@ for @setEmissionLongitude:@
setEmissionLongitudeSelector :: Selector '[CDouble] ()
setEmissionLongitudeSelector = mkSelector "setEmissionLongitude:"

-- | @Selector@ for @emissionRange@
emissionRangeSelector :: Selector '[] CDouble
emissionRangeSelector = mkSelector "emissionRange"

-- | @Selector@ for @setEmissionRange:@
setEmissionRangeSelector :: Selector '[CDouble] ()
setEmissionRangeSelector = mkSelector "setEmissionRange:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector '[] CDouble
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector '[CDouble] ()
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @velocityRange@
velocityRangeSelector :: Selector '[] CDouble
velocityRangeSelector = mkSelector "velocityRange"

-- | @Selector@ for @setVelocityRange:@
setVelocityRangeSelector :: Selector '[CDouble] ()
setVelocityRangeSelector = mkSelector "setVelocityRange:"

-- | @Selector@ for @xAcceleration@
xAccelerationSelector :: Selector '[] CDouble
xAccelerationSelector = mkSelector "xAcceleration"

-- | @Selector@ for @setXAcceleration:@
setXAccelerationSelector :: Selector '[CDouble] ()
setXAccelerationSelector = mkSelector "setXAcceleration:"

-- | @Selector@ for @yAcceleration@
yAccelerationSelector :: Selector '[] CDouble
yAccelerationSelector = mkSelector "yAcceleration"

-- | @Selector@ for @setYAcceleration:@
setYAccelerationSelector :: Selector '[CDouble] ()
setYAccelerationSelector = mkSelector "setYAcceleration:"

-- | @Selector@ for @zAcceleration@
zAccelerationSelector :: Selector '[] CDouble
zAccelerationSelector = mkSelector "zAcceleration"

-- | @Selector@ for @setZAcceleration:@
setZAccelerationSelector :: Selector '[CDouble] ()
setZAccelerationSelector = mkSelector "setZAcceleration:"

-- | @Selector@ for @scale@
scaleSelector :: Selector '[] CDouble
scaleSelector = mkSelector "scale"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector '[CDouble] ()
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @scaleRange@
scaleRangeSelector :: Selector '[] CDouble
scaleRangeSelector = mkSelector "scaleRange"

-- | @Selector@ for @setScaleRange:@
setScaleRangeSelector :: Selector '[CDouble] ()
setScaleRangeSelector = mkSelector "setScaleRange:"

-- | @Selector@ for @scaleSpeed@
scaleSpeedSelector :: Selector '[] CDouble
scaleSpeedSelector = mkSelector "scaleSpeed"

-- | @Selector@ for @setScaleSpeed:@
setScaleSpeedSelector :: Selector '[CDouble] ()
setScaleSpeedSelector = mkSelector "setScaleSpeed:"

-- | @Selector@ for @spin@
spinSelector :: Selector '[] CDouble
spinSelector = mkSelector "spin"

-- | @Selector@ for @setSpin:@
setSpinSelector :: Selector '[CDouble] ()
setSpinSelector = mkSelector "setSpin:"

-- | @Selector@ for @spinRange@
spinRangeSelector :: Selector '[] CDouble
spinRangeSelector = mkSelector "spinRange"

-- | @Selector@ for @setSpinRange:@
setSpinRangeSelector :: Selector '[CDouble] ()
setSpinRangeSelector = mkSelector "setSpinRange:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Ptr ())
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Ptr ()] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @redRange@
redRangeSelector :: Selector '[] CFloat
redRangeSelector = mkSelector "redRange"

-- | @Selector@ for @setRedRange:@
setRedRangeSelector :: Selector '[CFloat] ()
setRedRangeSelector = mkSelector "setRedRange:"

-- | @Selector@ for @greenRange@
greenRangeSelector :: Selector '[] CFloat
greenRangeSelector = mkSelector "greenRange"

-- | @Selector@ for @setGreenRange:@
setGreenRangeSelector :: Selector '[CFloat] ()
setGreenRangeSelector = mkSelector "setGreenRange:"

-- | @Selector@ for @blueRange@
blueRangeSelector :: Selector '[] CFloat
blueRangeSelector = mkSelector "blueRange"

-- | @Selector@ for @setBlueRange:@
setBlueRangeSelector :: Selector '[CFloat] ()
setBlueRangeSelector = mkSelector "setBlueRange:"

-- | @Selector@ for @alphaRange@
alphaRangeSelector :: Selector '[] CFloat
alphaRangeSelector = mkSelector "alphaRange"

-- | @Selector@ for @setAlphaRange:@
setAlphaRangeSelector :: Selector '[CFloat] ()
setAlphaRangeSelector = mkSelector "setAlphaRange:"

-- | @Selector@ for @redSpeed@
redSpeedSelector :: Selector '[] CFloat
redSpeedSelector = mkSelector "redSpeed"

-- | @Selector@ for @setRedSpeed:@
setRedSpeedSelector :: Selector '[CFloat] ()
setRedSpeedSelector = mkSelector "setRedSpeed:"

-- | @Selector@ for @greenSpeed@
greenSpeedSelector :: Selector '[] CFloat
greenSpeedSelector = mkSelector "greenSpeed"

-- | @Selector@ for @setGreenSpeed:@
setGreenSpeedSelector :: Selector '[CFloat] ()
setGreenSpeedSelector = mkSelector "setGreenSpeed:"

-- | @Selector@ for @blueSpeed@
blueSpeedSelector :: Selector '[] CFloat
blueSpeedSelector = mkSelector "blueSpeed"

-- | @Selector@ for @setBlueSpeed:@
setBlueSpeedSelector :: Selector '[CFloat] ()
setBlueSpeedSelector = mkSelector "setBlueSpeed:"

-- | @Selector@ for @alphaSpeed@
alphaSpeedSelector :: Selector '[] CFloat
alphaSpeedSelector = mkSelector "alphaSpeed"

-- | @Selector@ for @setAlphaSpeed:@
setAlphaSpeedSelector :: Selector '[CFloat] ()
setAlphaSpeedSelector = mkSelector "setAlphaSpeed:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] RawId
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[RawId] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @contentsScale@
contentsScaleSelector :: Selector '[] CDouble
contentsScaleSelector = mkSelector "contentsScale"

-- | @Selector@ for @setContentsScale:@
setContentsScaleSelector :: Selector '[CDouble] ()
setContentsScaleSelector = mkSelector "setContentsScale:"

-- | @Selector@ for @minificationFilter@
minificationFilterSelector :: Selector '[] (Id NSString)
minificationFilterSelector = mkSelector "minificationFilter"

-- | @Selector@ for @setMinificationFilter:@
setMinificationFilterSelector :: Selector '[Id NSString] ()
setMinificationFilterSelector = mkSelector "setMinificationFilter:"

-- | @Selector@ for @magnificationFilter@
magnificationFilterSelector :: Selector '[] (Id NSString)
magnificationFilterSelector = mkSelector "magnificationFilter"

-- | @Selector@ for @setMagnificationFilter:@
setMagnificationFilterSelector :: Selector '[Id NSString] ()
setMagnificationFilterSelector = mkSelector "setMagnificationFilter:"

-- | @Selector@ for @minificationFilterBias@
minificationFilterBiasSelector :: Selector '[] CFloat
minificationFilterBiasSelector = mkSelector "minificationFilterBias"

-- | @Selector@ for @setMinificationFilterBias:@
setMinificationFilterBiasSelector :: Selector '[CFloat] ()
setMinificationFilterBiasSelector = mkSelector "setMinificationFilterBias:"

-- | @Selector@ for @emitterCells@
emitterCellsSelector :: Selector '[] (Id NSArray)
emitterCellsSelector = mkSelector "emitterCells"

-- | @Selector@ for @setEmitterCells:@
setEmitterCellsSelector :: Selector '[Id NSArray] ()
setEmitterCellsSelector = mkSelector "setEmitterCells:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] (Id NSDictionary)
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[Id NSDictionary] ()
setStyleSelector = mkSelector "setStyle:"

