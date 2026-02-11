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
  , initWithFirstValue_secondValueSelector
  , firstSelector
  , setFirstSelector
  , secondSelector
  , setSecondSelector


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
initWithFirstValue_secondValue phaseNumericPair  first second =
  sendMsg phaseNumericPair (mkSelector "initWithFirstValue:secondValue:") (retPtr retVoid) [argCDouble (fromIntegral first), argCDouble (fromIntegral second)] >>= ownedObject . castPtr

-- | first
--
-- The first value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- first@
first :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> IO CDouble
first phaseNumericPair  =
  sendMsg phaseNumericPair (mkSelector "first") retCDouble []

-- | first
--
-- The first value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- setFirst:@
setFirst :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> CDouble -> IO ()
setFirst phaseNumericPair  value =
  sendMsg phaseNumericPair (mkSelector "setFirst:") retVoid [argCDouble (fromIntegral value)]

-- | second
--
-- The second value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- second@
second :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> IO CDouble
second phaseNumericPair  =
  sendMsg phaseNumericPair (mkSelector "second") retCDouble []

-- | second
--
-- The second value in the numeric pair.
--
-- The default value is 0.0.
--
-- ObjC selector: @- setSecond:@
setSecond :: IsPHASENumericPair phaseNumericPair => phaseNumericPair -> CDouble -> IO ()
setSecond phaseNumericPair  value =
  sendMsg phaseNumericPair (mkSelector "setSecond:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFirstValue:secondValue:@
initWithFirstValue_secondValueSelector :: Selector
initWithFirstValue_secondValueSelector = mkSelector "initWithFirstValue:secondValue:"

-- | @Selector@ for @first@
firstSelector :: Selector
firstSelector = mkSelector "first"

-- | @Selector@ for @setFirst:@
setFirstSelector :: Selector
setFirstSelector = mkSelector "setFirst:"

-- | @Selector@ for @second@
secondSelector :: Selector
secondSelector = mkSelector "second"

-- | @Selector@ for @setSecond:@
setSecondSelector :: Selector
setSecondSelector = mkSelector "setSecond:"

