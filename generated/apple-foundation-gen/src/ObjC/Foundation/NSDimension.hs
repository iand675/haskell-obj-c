{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDimension@.
module ObjC.Foundation.NSDimension
  ( NSDimension
  , IsNSDimension(..)
  , initWithSymbol_converter
  , baseUnit
  , converter
  , baseUnitSelector
  , converterSelector
  , initWithSymbol_converterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithSymbol:converter:@
initWithSymbol_converter :: (IsNSDimension nsDimension, IsNSString symbol, IsNSUnitConverter converter) => nsDimension -> symbol -> converter -> IO (Id NSDimension)
initWithSymbol_converter nsDimension symbol converter =
  sendOwnedMessage nsDimension initWithSymbol_converterSelector (toNSString symbol) (toNSUnitConverter converter)

-- | @+ baseUnit@
baseUnit :: IO (Id NSDimension)
baseUnit  =
  do
    cls' <- getRequiredClass "NSDimension"
    sendClassMessage cls' baseUnitSelector

-- | @- converter@
converter :: IsNSDimension nsDimension => nsDimension -> IO (Id NSUnitConverter)
converter nsDimension =
  sendMessage nsDimension converterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSymbol:converter:@
initWithSymbol_converterSelector :: Selector '[Id NSString, Id NSUnitConverter] (Id NSDimension)
initWithSymbol_converterSelector = mkSelector "initWithSymbol:converter:"

-- | @Selector@ for @baseUnit@
baseUnitSelector :: Selector '[] (Id NSDimension)
baseUnitSelector = mkSelector "baseUnit"

-- | @Selector@ for @converter@
converterSelector :: Selector '[] (Id NSUnitConverter)
converterSelector = mkSelector "converter"

