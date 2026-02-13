{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLMatrix4x4Array@.
module ObjC.ModelIO.MDLMatrix4x4Array
  ( MDLMatrix4x4Array
  , IsMDLMatrix4x4Array(..)
  , clear
  , initWithElementCount
  , elementCount
  , precision
  , clearSelector
  , elementCountSelector
  , initWithElementCountSelector
  , precisionSelector

  -- * Enum types
  , MDLDataPrecision(MDLDataPrecision)
  , pattern MDLDataPrecisionUndefined
  , pattern MDLDataPrecisionFloat
  , pattern MDLDataPrecisionDouble

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- clear@
clear :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> IO ()
clear mdlMatrix4x4Array =
  sendMessage mdlMatrix4x4Array clearSelector

-- | @- initWithElementCount:@
initWithElementCount :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> CULong -> IO (Id MDLMatrix4x4Array)
initWithElementCount mdlMatrix4x4Array arrayElementCount =
  sendOwnedMessage mdlMatrix4x4Array initWithElementCountSelector arrayElementCount

-- | @- elementCount@
elementCount :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> IO CULong
elementCount mdlMatrix4x4Array =
  sendMessage mdlMatrix4x4Array elementCountSelector

-- | @- precision@
precision :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> IO MDLDataPrecision
precision mdlMatrix4x4Array =
  sendMessage mdlMatrix4x4Array precisionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clear@
clearSelector :: Selector '[] ()
clearSelector = mkSelector "clear"

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector '[CULong] (Id MDLMatrix4x4Array)
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CULong
elementCountSelector = mkSelector "elementCount"

-- | @Selector@ for @precision@
precisionSelector :: Selector '[] MDLDataPrecision
precisionSelector = mkSelector "precision"

