{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMorphologyPronoun@.
module ObjC.Foundation.NSMorphologyPronoun
  ( NSMorphologyPronoun
  , IsNSMorphologyPronoun(..)
  , new
  , init_
  , initWithPronoun_morphology_dependentMorphology
  , pronoun
  , morphology
  , dependentMorphology
  , dependentMorphologySelector
  , initSelector
  , initWithPronoun_morphology_dependentMorphologySelector
  , morphologySelector
  , newSelector
  , pronounSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSMorphologyPronoun)
new  =
  do
    cls' <- getRequiredClass "NSMorphologyPronoun"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSMorphologyPronoun)
init_ nsMorphologyPronoun =
  sendOwnedMessage nsMorphologyPronoun initSelector

-- | @- initWithPronoun:morphology:dependentMorphology:@
initWithPronoun_morphology_dependentMorphology :: (IsNSMorphologyPronoun nsMorphologyPronoun, IsNSString pronoun, IsNSMorphology morphology, IsNSMorphology dependentMorphology) => nsMorphologyPronoun -> pronoun -> morphology -> dependentMorphology -> IO (Id NSMorphologyPronoun)
initWithPronoun_morphology_dependentMorphology nsMorphologyPronoun pronoun morphology dependentMorphology =
  sendOwnedMessage nsMorphologyPronoun initWithPronoun_morphology_dependentMorphologySelector (toNSString pronoun) (toNSMorphology morphology) (toNSMorphology dependentMorphology)

-- | @- pronoun@
pronoun :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSString)
pronoun nsMorphologyPronoun =
  sendMessage nsMorphologyPronoun pronounSelector

-- | @- morphology@
morphology :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSMorphology)
morphology nsMorphologyPronoun =
  sendMessage nsMorphologyPronoun morphologySelector

-- | @- dependentMorphology@
dependentMorphology :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSMorphology)
dependentMorphology nsMorphologyPronoun =
  sendMessage nsMorphologyPronoun dependentMorphologySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSMorphologyPronoun)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMorphologyPronoun)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPronoun:morphology:dependentMorphology:@
initWithPronoun_morphology_dependentMorphologySelector :: Selector '[Id NSString, Id NSMorphology, Id NSMorphology] (Id NSMorphologyPronoun)
initWithPronoun_morphology_dependentMorphologySelector = mkSelector "initWithPronoun:morphology:dependentMorphology:"

-- | @Selector@ for @pronoun@
pronounSelector :: Selector '[] (Id NSString)
pronounSelector = mkSelector "pronoun"

-- | @Selector@ for @morphology@
morphologySelector :: Selector '[] (Id NSMorphology)
morphologySelector = mkSelector "morphology"

-- | @Selector@ for @dependentMorphology@
dependentMorphologySelector :: Selector '[] (Id NSMorphology)
dependentMorphologySelector = mkSelector "dependentMorphology"

