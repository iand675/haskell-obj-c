{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASENumericPair
--
-- A numeric pair.
--
-- Generated bindings for @PHASENumericPair@.
module ObjC.PHASE.PHASENumericPair
  ( PHASENumericPair
  , IsPHASENumericPair(..)
  , initWithFirstValue_secondValue
  , first
  , setFirst
  , second
  , setSecond
  , firstSelector
  , initWithFirstValue_secondValueSelector
  , secondSelector
  , setFirstSelector
  , setSecondSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithFirstValue:secondValue
--
-- Initialize a numeric pair with a first and second value.
--
-- @first@ — The first value in the pair.
--
-- @second@ — The second value in the pair.
--
-- Returns: A new pair.
--
-- ObjC selector: @- initWithFirstValue:secondValue:@
initWithFirstValue_secondValue :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> CDouble -> CDouble -> IO (Id PHASENumericPair)
initWithFirstValue_secondValue phaseNumericPair first second =
  sendOwnedMessage phaseNumericPair initWithFirstValue_secondValueSelector first second

-- | first
--
-- The first value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- first@
first :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> IO CDouble
first phaseNumericPair =
  sendMessage phaseNumericPair firstSelector

-- | first
--
-- The first value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- setFirst:@
setFirst :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> CDouble -> IO ()
setFirst phaseNumericPair value =
  sendMessage phaseNumericPair setFirstSelector value

-- | second
--
-- The second value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- second@
second :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> IO CDouble
second phaseNumericPair =
  sendMessage phaseNumericPair secondSelector

-- | second
--
-- The second value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- setSecond:@
setSecond :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> CDouble -> IO ()
setSecond phaseNumericPair value =
  sendMessage phaseNumericPair setSecondSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFirstValue:secondValue:@
initWithFirstValue_secondValueSelector :: Selector '[CDouble, CDouble] (Id PHASENumericPair)
initWithFirstValue_secondValueSelector = mkSelector "initWithFirstValue:secondValue:"

-- | @Selector@ for @first@
firstSelector :: Selector '[] CDouble
firstSelector = mkSelector "first"

-- | @Selector@ for @setFirst:@
setFirstSelector :: Selector '[CDouble] ()
setFirstSelector = mkSelector "setFirst:"

-- | @Selector@ for @second@
secondSelector :: Selector '[] CDouble
secondSelector = mkSelector "second"

-- | @Selector@ for @setSecond:@
setSecondSelector :: Selector '[CDouble] ()
setSecondSelector = mkSelector "setSecond:"

