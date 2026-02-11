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
  , minNumberSelector
  , maxNumberSelector
  , enumeratedNumbersSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minNumber@
minNumber :: IsMLNumericConstraint mlNumericConstraint => mlNumericConstraint -> IO (Id NSNumber)
minNumber mlNumericConstraint  =
  sendMsg mlNumericConstraint (mkSelector "minNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maxNumber@
maxNumber :: IsMLNumericConstraint mlNumericConstraint => mlNumericConstraint -> IO (Id NSNumber)
maxNumber mlNumericConstraint  =
  sendMsg mlNumericConstraint (mkSelector "maxNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enumeratedNumbers@
enumeratedNumbers :: IsMLNumericConstraint mlNumericConstraint => mlNumericConstraint -> IO (Id NSSet)
enumeratedNumbers mlNumericConstraint  =
  sendMsg mlNumericConstraint (mkSelector "enumeratedNumbers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minNumber@
minNumberSelector :: Selector
minNumberSelector = mkSelector "minNumber"

-- | @Selector@ for @maxNumber@
maxNumberSelector :: Selector
maxNumberSelector = mkSelector "maxNumber"

-- | @Selector@ for @enumeratedNumbers@
enumeratedNumbersSelector :: Selector
enumeratedNumbersSelector = mkSelector "enumeratedNumbers"

