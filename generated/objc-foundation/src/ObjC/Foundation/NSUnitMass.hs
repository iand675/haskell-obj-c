{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitMass@.
module ObjC.Foundation.NSUnitMass
  ( NSUnitMass
  , IsNSUnitMass(..)
  , kilograms
  , grams
  , decigrams
  , centigrams
  , milligrams
  , micrograms
  , nanograms
  , picograms
  , ounces
  , poundsMass
  , stones
  , metricTons
  , shortTons
  , carats
  , ouncesTroy
  , slugs
  , kilogramsSelector
  , gramsSelector
  , decigramsSelector
  , centigramsSelector
  , milligramsSelector
  , microgramsSelector
  , nanogramsSelector
  , picogramsSelector
  , ouncesSelector
  , poundsMassSelector
  , stonesSelector
  , metricTonsSelector
  , shortTonsSelector
  , caratsSelector
  , ouncesTroySelector
  , slugsSelector


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

-- | @+ kilograms@
kilograms :: IO (Id NSUnitMass)
kilograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "kilograms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ grams@
grams :: IO (Id NSUnitMass)
grams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "grams") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decigrams@
decigrams :: IO (Id NSUnitMass)
decigrams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "decigrams") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ centigrams@
centigrams :: IO (Id NSUnitMass)
centigrams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "centigrams") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milligrams@
milligrams :: IO (Id NSUnitMass)
milligrams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "milligrams") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ micrograms@
micrograms :: IO (Id NSUnitMass)
micrograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "micrograms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nanograms@
nanograms :: IO (Id NSUnitMass)
nanograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "nanograms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ picograms@
picograms :: IO (Id NSUnitMass)
picograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "picograms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ounces@
ounces :: IO (Id NSUnitMass)
ounces  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "ounces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ poundsMass@
poundsMass :: IO (Id NSUnitMass)
poundsMass  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "poundsMass") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ stones@
stones :: IO (Id NSUnitMass)
stones  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "stones") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ metricTons@
metricTons :: IO (Id NSUnitMass)
metricTons  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "metricTons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ shortTons@
shortTons :: IO (Id NSUnitMass)
shortTons  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "shortTons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ carats@
carats :: IO (Id NSUnitMass)
carats  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "carats") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ouncesTroy@
ouncesTroy :: IO (Id NSUnitMass)
ouncesTroy  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "ouncesTroy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ slugs@
slugs :: IO (Id NSUnitMass)
slugs  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMsg cls' (mkSelector "slugs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kilograms@
kilogramsSelector :: Selector
kilogramsSelector = mkSelector "kilograms"

-- | @Selector@ for @grams@
gramsSelector :: Selector
gramsSelector = mkSelector "grams"

-- | @Selector@ for @decigrams@
decigramsSelector :: Selector
decigramsSelector = mkSelector "decigrams"

-- | @Selector@ for @centigrams@
centigramsSelector :: Selector
centigramsSelector = mkSelector "centigrams"

-- | @Selector@ for @milligrams@
milligramsSelector :: Selector
milligramsSelector = mkSelector "milligrams"

-- | @Selector@ for @micrograms@
microgramsSelector :: Selector
microgramsSelector = mkSelector "micrograms"

-- | @Selector@ for @nanograms@
nanogramsSelector :: Selector
nanogramsSelector = mkSelector "nanograms"

-- | @Selector@ for @picograms@
picogramsSelector :: Selector
picogramsSelector = mkSelector "picograms"

-- | @Selector@ for @ounces@
ouncesSelector :: Selector
ouncesSelector = mkSelector "ounces"

-- | @Selector@ for @poundsMass@
poundsMassSelector :: Selector
poundsMassSelector = mkSelector "poundsMass"

-- | @Selector@ for @stones@
stonesSelector :: Selector
stonesSelector = mkSelector "stones"

-- | @Selector@ for @metricTons@
metricTonsSelector :: Selector
metricTonsSelector = mkSelector "metricTons"

-- | @Selector@ for @shortTons@
shortTonsSelector :: Selector
shortTonsSelector = mkSelector "shortTons"

-- | @Selector@ for @carats@
caratsSelector :: Selector
caratsSelector = mkSelector "carats"

-- | @Selector@ for @ouncesTroy@
ouncesTroySelector :: Selector
ouncesTroySelector = mkSelector "ouncesTroy"

-- | @Selector@ for @slugs@
slugsSelector :: Selector
slugsSelector = mkSelector "slugs"

