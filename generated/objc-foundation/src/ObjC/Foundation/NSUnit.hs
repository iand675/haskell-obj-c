{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnit@.
module ObjC.Foundation.NSUnit
  ( NSUnit
  , IsNSUnit(..)
  , init_
  , new
  , initWithSymbol
  , symbol
  , initSelector
  , newSelector
  , initWithSymbolSelector
  , symbolSelector


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

-- | @- init@
init_ :: IsNSUnit nsUnit => nsUnit -> IO (Id NSUnit)
init_ nsUnit  =
  sendMsg nsUnit (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSUnit)
new  =
  do
    cls' <- getRequiredClass "NSUnit"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSymbol:@
initWithSymbol :: (IsNSUnit nsUnit, IsNSString symbol) => nsUnit -> symbol -> IO (Id NSUnit)
initWithSymbol nsUnit  symbol =
withObjCPtr symbol $ \raw_symbol ->
    sendMsg nsUnit (mkSelector "initWithSymbol:") (retPtr retVoid) [argPtr (castPtr raw_symbol :: Ptr ())] >>= ownedObject . castPtr

-- | @- symbol@
symbol :: IsNSUnit nsUnit => nsUnit -> IO (Id NSString)
symbol nsUnit  =
  sendMsg nsUnit (mkSelector "symbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSymbol:@
initWithSymbolSelector :: Selector
initWithSymbolSelector = mkSelector "initWithSymbol:"

-- | @Selector@ for @symbol@
symbolSelector :: Selector
symbolSelector = mkSelector "symbol"

