{-# LANGUAGE DataKinds #-}
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
  , caratsSelector
  , centigramsSelector
  , decigramsSelector
  , gramsSelector
  , kilogramsSelector
  , metricTonsSelector
  , microgramsSelector
  , milligramsSelector
  , nanogramsSelector
  , ouncesSelector
  , ouncesTroySelector
  , picogramsSelector
  , poundsMassSelector
  , shortTonsSelector
  , slugsSelector
  , stonesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ kilograms@
kilograms :: IO (Id NSUnitMass)
kilograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' kilogramsSelector

-- | @+ grams@
grams :: IO (Id NSUnitMass)
grams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' gramsSelector

-- | @+ decigrams@
decigrams :: IO (Id NSUnitMass)
decigrams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' decigramsSelector

-- | @+ centigrams@
centigrams :: IO (Id NSUnitMass)
centigrams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' centigramsSelector

-- | @+ milligrams@
milligrams :: IO (Id NSUnitMass)
milligrams  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' milligramsSelector

-- | @+ micrograms@
micrograms :: IO (Id NSUnitMass)
micrograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' microgramsSelector

-- | @+ nanograms@
nanograms :: IO (Id NSUnitMass)
nanograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' nanogramsSelector

-- | @+ picograms@
picograms :: IO (Id NSUnitMass)
picograms  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' picogramsSelector

-- | @+ ounces@
ounces :: IO (Id NSUnitMass)
ounces  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' ouncesSelector

-- | @+ poundsMass@
poundsMass :: IO (Id NSUnitMass)
poundsMass  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' poundsMassSelector

-- | @+ stones@
stones :: IO (Id NSUnitMass)
stones  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' stonesSelector

-- | @+ metricTons@
metricTons :: IO (Id NSUnitMass)
metricTons  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' metricTonsSelector

-- | @+ shortTons@
shortTons :: IO (Id NSUnitMass)
shortTons  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' shortTonsSelector

-- | @+ carats@
carats :: IO (Id NSUnitMass)
carats  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' caratsSelector

-- | @+ ouncesTroy@
ouncesTroy :: IO (Id NSUnitMass)
ouncesTroy  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' ouncesTroySelector

-- | @+ slugs@
slugs :: IO (Id NSUnitMass)
slugs  =
  do
    cls' <- getRequiredClass "NSUnitMass"
    sendClassMessage cls' slugsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kilograms@
kilogramsSelector :: Selector '[] (Id NSUnitMass)
kilogramsSelector = mkSelector "kilograms"

-- | @Selector@ for @grams@
gramsSelector :: Selector '[] (Id NSUnitMass)
gramsSelector = mkSelector "grams"

-- | @Selector@ for @decigrams@
decigramsSelector :: Selector '[] (Id NSUnitMass)
decigramsSelector = mkSelector "decigrams"

-- | @Selector@ for @centigrams@
centigramsSelector :: Selector '[] (Id NSUnitMass)
centigramsSelector = mkSelector "centigrams"

-- | @Selector@ for @milligrams@
milligramsSelector :: Selector '[] (Id NSUnitMass)
milligramsSelector = mkSelector "milligrams"

-- | @Selector@ for @micrograms@
microgramsSelector :: Selector '[] (Id NSUnitMass)
microgramsSelector = mkSelector "micrograms"

-- | @Selector@ for @nanograms@
nanogramsSelector :: Selector '[] (Id NSUnitMass)
nanogramsSelector = mkSelector "nanograms"

-- | @Selector@ for @picograms@
picogramsSelector :: Selector '[] (Id NSUnitMass)
picogramsSelector = mkSelector "picograms"

-- | @Selector@ for @ounces@
ouncesSelector :: Selector '[] (Id NSUnitMass)
ouncesSelector = mkSelector "ounces"

-- | @Selector@ for @poundsMass@
poundsMassSelector :: Selector '[] (Id NSUnitMass)
poundsMassSelector = mkSelector "poundsMass"

-- | @Selector@ for @stones@
stonesSelector :: Selector '[] (Id NSUnitMass)
stonesSelector = mkSelector "stones"

-- | @Selector@ for @metricTons@
metricTonsSelector :: Selector '[] (Id NSUnitMass)
metricTonsSelector = mkSelector "metricTons"

-- | @Selector@ for @shortTons@
shortTonsSelector :: Selector '[] (Id NSUnitMass)
shortTonsSelector = mkSelector "shortTons"

-- | @Selector@ for @carats@
caratsSelector :: Selector '[] (Id NSUnitMass)
caratsSelector = mkSelector "carats"

-- | @Selector@ for @ouncesTroy@
ouncesTroySelector :: Selector '[] (Id NSUnitMass)
ouncesTroySelector = mkSelector "ouncesTroy"

-- | @Selector@ for @slugs@
slugsSelector :: Selector '[] (Id NSUnitMass)
slugsSelector = mkSelector "slugs"

