{-# LANGUAGE DataKinds #-}
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
  , initWithSymbolSelector
  , newSelector
  , symbolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSUnit nsUnit => nsUnit -> IO (Id NSUnit)
init_ nsUnit =
  sendOwnedMessage nsUnit initSelector

-- | @+ new@
new :: IO (Id NSUnit)
new  =
  do
    cls' <- getRequiredClass "NSUnit"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithSymbol:@
initWithSymbol :: (IsNSUnit nsUnit, IsNSString symbol) => nsUnit -> symbol -> IO (Id NSUnit)
initWithSymbol nsUnit symbol =
  sendOwnedMessage nsUnit initWithSymbolSelector (toNSString symbol)

-- | @- symbol@
symbol :: IsNSUnit nsUnit => nsUnit -> IO (Id NSString)
symbol nsUnit =
  sendMessage nsUnit symbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSUnit)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSUnit)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSymbol:@
initWithSymbolSelector :: Selector '[Id NSString] (Id NSUnit)
initWithSymbolSelector = mkSelector "initWithSymbol:"

-- | @Selector@ for @symbol@
symbolSelector :: Selector '[] (Id NSString)
symbolSelector = mkSelector "symbol"

