{-# LANGUAGE PatternSynonyms #-}
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
  , initWithElementCountSelector
  , elementCountSelector
  , precisionSelector

  -- * Enum types
  , MDLDataPrecision(MDLDataPrecision)
  , pattern MDLDataPrecisionUndefined
  , pattern MDLDataPrecisionFloat
  , pattern MDLDataPrecisionDouble

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- clear@
clear :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> IO ()
clear mdlMatrix4x4Array  =
  sendMsg mdlMatrix4x4Array (mkSelector "clear") retVoid []

-- | @- initWithElementCount:@
initWithElementCount :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> CULong -> IO (Id MDLMatrix4x4Array)
initWithElementCount mdlMatrix4x4Array  arrayElementCount =
  sendMsg mdlMatrix4x4Array (mkSelector "initWithElementCount:") (retPtr retVoid) [argCULong (fromIntegral arrayElementCount)] >>= ownedObject . castPtr

-- | @- elementCount@
elementCount :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> IO CULong
elementCount mdlMatrix4x4Array  =
  sendMsg mdlMatrix4x4Array (mkSelector "elementCount") retCULong []

-- | @- precision@
precision :: IsMDLMatrix4x4Array mdlMatrix4x4Array => mdlMatrix4x4Array -> IO MDLDataPrecision
precision mdlMatrix4x4Array  =
  fmap (coerce :: CULong -> MDLDataPrecision) $ sendMsg mdlMatrix4x4Array (mkSelector "precision") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

-- | @Selector@ for @precision@
precisionSelector :: Selector
precisionSelector = mkSelector "precision"

