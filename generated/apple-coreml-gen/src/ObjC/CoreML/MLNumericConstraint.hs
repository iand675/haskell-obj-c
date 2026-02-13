{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Allows enforcement of constraints on the values of update parameters.
--
-- Generated bindings for @MLNumericConstraint@.
module ObjC.CoreML.MLNumericConstraint
  ( MLNumericConstraint
  , IsMLNumericConstraint(..)
  , minNumber
  , maxNumber
  , enumeratedNumbers
  , enumeratedNumbersSelector
  , maxNumberSelector
  , minNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minNumber@
minNumber :: IsMLNumericConstraint mlNumericConstraint => mlNumericConstraint -> IO (Id NSNumber)
minNumber mlNumericConstraint =
  sendMessage mlNumericConstraint minNumberSelector

-- | @- maxNumber@
maxNumber :: IsMLNumericConstraint mlNumericConstraint => mlNumericConstraint -> IO (Id NSNumber)
maxNumber mlNumericConstraint =
  sendMessage mlNumericConstraint maxNumberSelector

-- | @- enumeratedNumbers@
enumeratedNumbers :: IsMLNumericConstraint mlNumericConstraint => mlNumericConstraint -> IO (Id NSSet)
enumeratedNumbers mlNumericConstraint =
  sendMessage mlNumericConstraint enumeratedNumbersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minNumber@
minNumberSelector :: Selector '[] (Id NSNumber)
minNumberSelector = mkSelector "minNumber"

-- | @Selector@ for @maxNumber@
maxNumberSelector :: Selector '[] (Id NSNumber)
maxNumberSelector = mkSelector "maxNumber"

-- | @Selector@ for @enumeratedNumbers@
enumeratedNumbersSelector :: Selector '[] (Id NSSet)
enumeratedNumbersSelector = mkSelector "enumeratedNumbers"

