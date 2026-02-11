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

-- | @- initWithMorphology:@
initWithMorphology :: (IsNSInflectionRuleExplicit nsInflectionRuleExplicit, IsNSMorphology morphology) => nsInflectionRuleExplicit -> morphology -> IO (Id NSInflectionRuleExplicit)
initWithMorphology nsInflectionRuleExplicit  morphology =
withObjCPtr morphology $ \raw_morphology ->
    sendMsg nsInflectionRuleExplicit (mkSelector "initWithMorphology:") (retPtr retVoid) [argPtr (castPtr raw_morphology :: Ptr ())] >>= ownedObject . castPtr

-- | @- morphology@
morphology :: IsNSInflectionRuleExplicit nsInflectionRuleExplicit => nsInflectionRuleExplicit -> IO (Id NSMorphology)
morphology nsInflectionRuleExplicit  =
  sendMsg nsInflectionRuleExplicit (mkSelector "morphology") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMorphology:@
initWithMorphologySelector :: Selector
initWithMorphologySelector = mkSelector "initWithMorphology:"

-- | @Selector@ for @morphology@
morphologySelector :: Selector
morphologySelector = mkSelector "morphology"

