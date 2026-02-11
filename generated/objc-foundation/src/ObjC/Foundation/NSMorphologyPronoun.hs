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
  , newSelector
  , initSelector
  , initWithPronoun_morphology_dependentMorphologySelector
  , pronounSelector
  , morphologySelector
  , dependentMorphologySelector


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

-- | @+ new@
new :: IO (Id NSMorphologyPronoun)
new  =
  do
    cls' <- getRequiredClass "NSMorphologyPronoun"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSMorphologyPronoun)
init_ nsMorphologyPronoun  =
  sendMsg nsMorphologyPronoun (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPronoun:morphology:dependentMorphology:@
initWithPronoun_morphology_dependentMorphology :: (IsNSMorphologyPronoun nsMorphologyPronoun, IsNSString pronoun, IsNSMorphology morphology, IsNSMorphology dependentMorphology) => nsMorphologyPronoun -> pronoun -> morphology -> dependentMorphology -> IO (Id NSMorphologyPronoun)
initWithPronoun_morphology_dependentMorphology nsMorphologyPronoun  pronoun morphology dependentMorphology =
withObjCPtr pronoun $ \raw_pronoun ->
  withObjCPtr morphology $ \raw_morphology ->
    withObjCPtr dependentMorphology $ \raw_dependentMorphology ->
        sendMsg nsMorphologyPronoun (mkSelector "initWithPronoun:morphology:dependentMorphology:") (retPtr retVoid) [argPtr (castPtr raw_pronoun :: Ptr ()), argPtr (castPtr raw_morphology :: Ptr ()), argPtr (castPtr raw_dependentMorphology :: Ptr ())] >>= ownedObject . castPtr

-- | @- pronoun@
pronoun :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSString)
pronoun nsMorphologyPronoun  =
  sendMsg nsMorphologyPronoun (mkSelector "pronoun") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- morphology@
morphology :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSMorphology)
morphology nsMorphologyPronoun  =
  sendMsg nsMorphologyPronoun (mkSelector "morphology") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dependentMorphology@
dependentMorphology :: IsNSMorphologyPronoun nsMorphologyPronoun => nsMorphologyPronoun -> IO (Id NSMorphology)
dependentMorphology nsMorphologyPronoun  =
  sendMsg nsMorphologyPronoun (mkSelector "dependentMorphology") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPronoun:morphology:dependentMorphology:@
initWithPronoun_morphology_dependentMorphologySelector :: Selector
initWithPronoun_morphology_dependentMorphologySelector = mkSelector "initWithPronoun:morphology:dependentMorphology:"

-- | @Selector@ for @pronoun@
pronounSelector :: Selector
pronounSelector = mkSelector "pronoun"

-- | @Selector@ for @morphology@
morphologySelector :: Selector
morphologySelector = mkSelector "morphology"

-- | @Selector@ for @dependentMorphology@
dependentMorphologySelector :: Selector
dependentMorphologySelector = mkSelector "dependentMorphology"

