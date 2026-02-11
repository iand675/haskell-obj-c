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
  , initWithSymbol_converterSelector
  , baseUnitSelector
  , converterSelector


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

-- | @- initWithSymbol:converter:@
initWithSymbol_converter :: (IsNSDimension nsDimension, IsNSString symbol, IsNSUnitConverter converter) => nsDimension -> symbol -> converter -> IO (Id NSDimension)
initWithSymbol_converter nsDimension  symbol converter =
withObjCPtr symbol $ \raw_symbol ->
  withObjCPtr converter $ \raw_converter ->
      sendMsg nsDimension (mkSelector "initWithSymbol:converter:") (retPtr retVoid) [argPtr (castPtr raw_symbol :: Ptr ()), argPtr (castPtr raw_converter :: Ptr ())] >>= ownedObject . castPtr

-- | @+ baseUnit@
baseUnit :: IO (Id NSDimension)
baseUnit  =
  do
    cls' <- getRequiredClass "NSDimension"
    sendClassMsg cls' (mkSelector "baseUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- converter@
converter :: IsNSDimension nsDimension => nsDimension -> IO (Id NSUnitConverter)
converter nsDimension  =
  sendMsg nsDimension (mkSelector "converter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSymbol:converter:@
initWithSymbol_converterSelector :: Selector
initWithSymbol_converterSelector = mkSelector "initWithSymbol:converter:"

-- | @Selector@ for @baseUnit@
baseUnitSelector :: Selector
baseUnitSelector = mkSelector "baseUnit"

-- | @Selector@ for @converter@
converterSelector :: Selector
converterSelector = mkSelector "converter"

