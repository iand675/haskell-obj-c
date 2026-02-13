{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInflectionRuleExplicit@.
module ObjC.Foundation.NSInflectionRuleExplicit
  ( NSInflectionRuleExplicit
  , IsNSInflectionRuleExplicit(..)
  , initWithMorphology
  , morphology
  , initWithMorphologySelector
  , morphologySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithMorphology:@
initWithMorphology :: (IsNSInflectionRuleExplicit nsInflectionRuleExplicit, IsNSMorphology morphology) => nsInflectionRuleExplicit -> morphology -> IO (Id NSInflectionRuleExplicit)
initWithMorphology nsInflectionRuleExplicit morphology =
  sendOwnedMessage nsInflectionRuleExplicit initWithMorphologySelector (toNSMorphology morphology)

-- | @- morphology@
morphology :: IsNSInflectionRuleExplicit nsInflectionRuleExplicit => nsInflectionRuleExplicit -> IO (Id NSMorphology)
morphology nsInflectionRuleExplicit =
  sendMessage nsInflectionRuleExplicit morphologySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMorphology:@
initWithMorphologySelector :: Selector '[Id NSMorphology] (Id NSInflectionRuleExplicit)
initWithMorphologySelector = mkSelector "initWithMorphology:"

-- | @Selector@ for @morphology@
morphologySelector :: Selector '[] (Id NSMorphology)
morphologySelector = mkSelector "morphology"

