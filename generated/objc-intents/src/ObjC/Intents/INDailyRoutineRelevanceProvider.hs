{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A relevance provider that specifies relevance during a specific situation.
--
-- INDailyRoutineSituation
--
-- Generated bindings for @INDailyRoutineRelevanceProvider@.
module ObjC.Intents.INDailyRoutineRelevanceProvider
  ( INDailyRoutineRelevanceProvider
  , IsINDailyRoutineRelevanceProvider(..)
  , initWithSituation
  , situation
  , initWithSituationSelector
  , situationSelector

  -- * Enum types
  , INDailyRoutineSituation(INDailyRoutineSituation)
  , pattern INDailyRoutineSituationMorning
  , pattern INDailyRoutineSituationEvening
  , pattern INDailyRoutineSituationHome
  , pattern INDailyRoutineSituationWork
  , pattern INDailyRoutineSituationSchool
  , pattern INDailyRoutineSituationGym
  , pattern INDailyRoutineSituationCommute
  , pattern INDailyRoutineSituationHeadphonesConnected
  , pattern INDailyRoutineSituationActiveWorkout
  , pattern INDailyRoutineSituationPhysicalActivityIncomplete

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a daily routine relevance provider with the specified situation.
--
-- ObjC selector: @- initWithSituation:@
initWithSituation :: IsINDailyRoutineRelevanceProvider inDailyRoutineRelevanceProvider => inDailyRoutineRelevanceProvider -> INDailyRoutineSituation -> IO (Id INDailyRoutineRelevanceProvider)
initWithSituation inDailyRoutineRelevanceProvider  situation =
  sendMsg inDailyRoutineRelevanceProvider (mkSelector "initWithSituation:") (retPtr retVoid) [argCLong (coerce situation)] >>= ownedObject . castPtr

-- | The relevant daily routine situation of the provider.
--
-- ObjC selector: @- situation@
situation :: IsINDailyRoutineRelevanceProvider inDailyRoutineRelevanceProvider => inDailyRoutineRelevanceProvider -> IO INDailyRoutineSituation
situation inDailyRoutineRelevanceProvider  =
  fmap (coerce :: CLong -> INDailyRoutineSituation) $ sendMsg inDailyRoutineRelevanceProvider (mkSelector "situation") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSituation:@
initWithSituationSelector :: Selector
initWithSituationSelector = mkSelector "initWithSituation:"

-- | @Selector@ for @situation@
situationSelector :: Selector
situationSelector = mkSelector "situation"

